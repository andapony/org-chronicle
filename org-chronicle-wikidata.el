;;; org-chronicle-wikidata.el --- Wikidata life-events import for org-chronicle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: org-chronicle contributors
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; Import a person's vitals, relations, and curated events from Wikidata
;; into an org-chronicle project.  See
;; `org-chronicle-wikidata-import' and `org-chronicle-wikidata-reconcile'.

;;; Code:

(require 'org-chronicle)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

(defgroup org-chronicle-wikidata nil
  "Wikidata integration for org-chronicle."
  :group 'org-chronicle)

(defun org-chronicle-wikidata--parse-qid (s)
  "Return the canonical Wikidata QID in string S, or nil.
S may be a bare QID, a wiki URL, or an entity URI, case-insensitively."
  (when (stringp s)
    (let ((trimmed (string-trim s)))
      (when (string-match
             "\\(?:^\\|/\\)\\([Qq][0-9]+\\)\\(?:$\\|[/?#]\\)?"
             trimmed)
        (upcase (match-string 1 trimmed))))))

(defun org-chronicle-wikidata--time->date (time precision)
  "Convert Wikidata TIME and integer PRECISION to an org-chronicle date plist.
Return nil when TIME is missing, BCE, before year 1000, or coarser than a
year (PRECISION < 9).  Reuses `org-chronicle--date-parse' so sort keys and
precision match the core model."
  (when (and (stringp time)
             (integerp precision)
             (>= precision 9)
             (string-match
              "\\`\\+\\([0-9]\\{4,\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
              time))
    (let* ((y (string-to-number (match-string 1 time)))
           (mo (string-to-number (match-string 2 time)))
           (d (string-to-number (match-string 3 time))))
      (when (>= y 1000)
        (org-chronicle--date-parse
         (pcase precision
           (9 (format "%04d" y))
           (10 (format "%04d-%02d" y mo))
           (_ (format "%04d-%02d-%02d" y mo d))))))))

(defun org-chronicle-wikidata--bindings (json)
  "Parse SPARQL JSON string JSON; return a list of binding alists."
  (let* ((data (json-parse-string json :object-type 'alist :array-type 'list))
         (results (alist-get 'results data))
         (bindings (alist-get 'bindings results)))
    bindings))

(defun org-chronicle-wikidata--cell (row var)
  "Return the string value of VAR in binding alist ROW, or nil.
VAR is a string variable name."
  (let ((b (alist-get (intern var) row)))
    (and b (alist-get 'value b))))

(defun org-chronicle-wikidata--cell-int (row var)
  "Return the integer value of VAR in ROW, or nil when absent or non-numeric."
  (let ((v (org-chronicle-wikidata--cell row var)))
    (and v (string-match-p "\\`[0-9]+\\'" v) (string-to-number v))))

(defconst org-chronicle-wikidata--alias-separator "\x1f"
  "Separator used in SPARQL GROUP_CONCAT of aliases.")

(defun org-chronicle-wikidata--row-date (row val-var prec-var)
  "Build a date plist from VAL-VAR and PREC-VAR cells of ROW, or nil."
  (org-chronicle-wikidata--time->date
   (org-chronicle-wikidata--cell row val-var)
   (org-chronicle-wikidata--cell-int row prec-var)))

(defcustom org-chronicle-wikidata-sparql-endpoint
  "https://query.wikidata.org/sparql"
  "SPARQL endpoint for the Wikidata Query Service."
  :type 'string
  :group 'org-chronicle-wikidata)

(defcustom org-chronicle-wikidata-api-endpoint
  "https://www.wikidata.org/w/api.php"
  "Wikidata REST API endpoint."
  :type 'string
  :group 'org-chronicle-wikidata)

(defcustom org-chronicle-wikidata-timeout 20
  "Seconds to wait for a Wikidata HTTP response before failing."
  :type 'integer
  :group 'org-chronicle-wikidata)

(define-error 'org-chronicle-wikidata-error "Wikidata request failed")

(define-error 'org-chronicle-wikidata-rate-limited
  "Wikidata rate limited the request" 'org-chronicle-wikidata-error)

(defun org-chronicle-wikidata--http-get (url)
  "GET URL and return the response body as a string.
Signal `org-chronicle-wikidata-rate-limited' on HTTP 429 and
`org-chronicle-wikidata-error' on any other failure or timeout."
  (let ((url-request-extra-headers
         '(("Accept" . "application/sparql-results+json")
           ("User-Agent" . "org-chronicle (Emacs)")))
        (buf (with-timeout (org-chronicle-wikidata-timeout
                            (signal 'org-chronicle-wikidata-error
                                    (list "timeout" url)))
               (url-retrieve-synchronously url t t))))
    (unless buf (signal 'org-chronicle-wikidata-error (list "no response" url)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((status (and (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                             (string-to-number (match-string 1)))))
            (cond
             ((eq status 429) (signal 'org-chronicle-wikidata-rate-limited (list url)))
             ((and status (>= status 400))
              (signal 'org-chronicle-wikidata-error (list status url)))))
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                                'utf-8))
      (kill-buffer buf))))

(defun org-chronicle-wikidata--sparql-request (query)
  "Run SPARQL QUERY against the endpoint; return parsed binding rows."
  (org-chronicle-wikidata--bindings
   (org-chronicle-wikidata--http-get
    (concat org-chronicle-wikidata-sparql-endpoint
            "?format=json&query=" (url-hexify-string query)))))

(defun org-chronicle-wikidata--search-request (term)
  "Search Wikidata for TERM; return a list of candidate plists.
Each candidate is (:qid :label :description)."
  (let* ((url (concat org-chronicle-wikidata-api-endpoint
                      "?action=wbsearchentities&format=json&language=en"
                      "&type=item&limit=10&search=" (url-hexify-string term)))
         (data (json-parse-string (org-chronicle-wikidata--http-get url)
                                  :object-type 'alist :array-type 'list)))
    (mapcar (lambda (hit)
              (list :qid (alist-get 'id hit)
                    :label (alist-get 'label hit)
                    :description (or (alist-get 'description hit) "")))
            (alist-get 'search data))))

(defun org-chronicle-wikidata--vitals-query (qid)
  "Return the SPARQL vitals query for QID (single result row)."
  (format "SELECT ?born ?bornPrec ?died ?diedPrec ?birthPlaceLabel \
?deathPlaceLabel ?fatherLabel ?motherLabel \
(GROUP_CONCAT(DISTINCT ?alias; separator=\"\\u001f\") AS ?aliases) WHERE { \
BIND(wd:%s AS ?p) \
OPTIONAL { ?p p:P569/psv:P569 ?bn. ?bn wikibase:timeValue ?born; wikibase:timePrecision ?bornPrec. } \
OPTIONAL { ?p p:P570/psv:P570 ?dn. ?dn wikibase:timeValue ?died; wikibase:timePrecision ?diedPrec. } \
OPTIONAL { ?p wdt:P19 ?birthPlace. } OPTIONAL { ?p wdt:P20 ?deathPlace. } \
OPTIONAL { ?p wdt:P22 ?father. } OPTIONAL { ?p wdt:P25 ?mother. } \
OPTIONAL { ?p skos:altLabel ?alias. FILTER(LANG(?alias)=\"en\") } \
SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". } } \
GROUP BY ?born ?bornPrec ?died ?diedPrec ?birthPlaceLabel ?deathPlaceLabel \
?fatherLabel ?motherLabel" qid))

(defun org-chronicle-wikidata--spouses-query (qid)
  "Return the SPARQL spouses query for QID (one row per spouse)."
  (format "SELECT ?spouseLabel ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:P26 ?st. ?st ps:P26 ?spouse. \
OPTIONAL { ?st pqv:P580 ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:P582 ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } \
SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". } }" qid))

(defun org-chronicle-wikidata--events-query (qid)
  "Return the SPARQL positions-held query for QID (one row per position)."
  (format "SELECT ?title ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:P39 ?st. ?st ps:P39 ?pos. \
?pos rdfs:label ?title. FILTER(LANG(?title)=\"en\") \
OPTIONAL { ?st pqv:P580 ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:P582 ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } }" qid))

(defun org-chronicle-wikidata--fetch-person (qid)
  "Fetch QID from Wikidata and return a normalized person record."
  (org-chronicle-wikidata--rows->record
   qid
   (org-chronicle-wikidata--sparql-request (org-chronicle-wikidata--vitals-query qid))
   (org-chronicle-wikidata--sparql-request (org-chronicle-wikidata--spouses-query qid))
   (org-chronicle-wikidata--sparql-request (org-chronicle-wikidata--events-query qid))))













(defun org-chronicle-wikidata--rows->record (qid vitals spouses events)
  "Assemble a person record for QID from parsed binding lists.
VITALS is the (single) vitals row list, SPOUSES and EVENTS are row lists.
Returns a plist; unrepresentable dates are dropped (see
`org-chronicle-wikidata--time->date')."
  (let* ((v (car vitals))
         (alias-str (and v (org-chronicle-wikidata--cell v "aliases"))))
    (list
     :qid qid
     :born (and v (org-chronicle-wikidata--row-date v "born" "bornPrec"))
     :died (and v (org-chronicle-wikidata--row-date v "died" "diedPrec"))
     :birthplace (and v (org-chronicle-wikidata--cell v "birthPlaceLabel"))
     :deathplace (and v (org-chronicle-wikidata--cell v "deathPlaceLabel"))
     :father (and v (org-chronicle-wikidata--cell v "fatherLabel"))
     :mother (and v (org-chronicle-wikidata--cell v "motherLabel"))
     :aliases (and alias-str (not (string-empty-p alias-str))
                   (split-string alias-str org-chronicle-wikidata--alias-separator t))
     :spouses (mapcar (lambda (row)
                        (list :name (org-chronicle-wikidata--cell row "spouseLabel")
                              :date (org-chronicle-wikidata--row-date row "start" "startPrec")
                              :end (org-chronicle-wikidata--row-date row "end" "endPrec")))
                      spouses)
     :events (mapcar (lambda (row)
                       (list :kind "position"
                             :title (org-chronicle-wikidata--cell row "title")
                             :date (org-chronicle-wikidata--row-date row "start" "startPrec")
                             :date-end (org-chronicle-wikidata--row-date row "end" "endPrec")
                             :location nil))
                     events))))






(defun org-chronicle-wikidata--url (qid)
  "Return the canonical Wikidata item URL for QID."
  (concat "https://www.wikidata.org/wiki/" qid))

(defun org-chronicle-wikidata--entity-change (group property value url)
  "Build an entity change plist for PROPERTY=VALUE in GROUP, sourced to URL.
Return nil when VALUE is nil or empty."
  (and value (not (string-empty-p value))
       (list :target 'entity :group group :property property :value value
             :provenance url :default t)))

(defun org-chronicle-wikidata--record->changes (rec name)
  "Map person record REC (for person NAME) to a list of change plists.
Each change targets either an entity property or an event entry.
See the data contract in the package commentary for field names."
  (let* ((qid (plist-get rec :qid))
         (url (org-chronicle-wikidata--url qid))
         (born (plist-get rec :born))
         (died (plist-get rec :died))
         (parents (delq nil (list (plist-get rec :father) (plist-get rec :mother))))
         (spouses (plist-get rec :spouses))
         changes)
    (dolist (c (list
                (org-chronicle-wikidata--entity-change
                 'vitals "BORN" (and born (org-chronicle--date-format born)) url)
                (org-chronicle-wikidata--entity-change
                 'vitals "DIED" (and died (org-chronicle--date-format died)) url)
                (org-chronicle-wikidata--entity-change
                 'vitals "BIRTHPLACE" (plist-get rec :birthplace) url)
                (org-chronicle-wikidata--entity-change
                 'vitals "DEATHPLACE" (plist-get rec :deathplace) url)
                (org-chronicle-wikidata--entity-change
                 'vitals "WIKIDATA" qid url)))
      (when c (push c changes)))
    (dolist (c (list
                (org-chronicle-wikidata--entity-change
                 'relations "PARENTS"
                 (and parents (org-chronicle--join parents)) url)
                (org-chronicle-wikidata--entity-change
                 'relations "SPOUSE"
                 (and spouses (org-chronicle--join
                               (mapcar (lambda (s) (plist-get s :name)) spouses)))
                 url)
                (org-chronicle-wikidata--entity-change
                 'relations "ALIASES"
                 (and (plist-get rec :aliases)
                      (org-chronicle--join (plist-get rec :aliases)))
                 url)))
      (when c (push c changes)))
    (when born
      (push (list :target 'event :group 'vitals :provenance url :default t
                  :event (list :kind "birth" :life-event "birth"
                               :title (format "Birth of %s" name)
                               :date (org-chronicle--date-format born)
                               :subject (list name)
                               :location (plist-get rec :birthplace)))
            changes))
    (when died
      (push (list :target 'event :group 'vitals :provenance url :default t
                  :event (list :kind "death" :life-event "death"
                               :title (format "Death of %s" name)
                               :date (org-chronicle--date-format died)
                               :subject (list name)
                               :location (plist-get rec :deathplace)))
            changes))
    (dolist (s spouses)
      (when (plist-get s :date)
        (push (list :target 'event :group 'relations :provenance url :default t
                    :event (list :kind "marriage" :life-event "marriage"
                                 :title (format "Marriage of %s and %s"
                                                name (plist-get s :name))
                                 :date (org-chronicle--date-format (plist-get s :date))
                                 :people (list name (plist-get s :name))))
              changes)))
    (dolist (ev (plist-get rec :events))
      (when (plist-get ev :date)
        (push (list :target 'event :group 'events :provenance url :default nil
                    :event (list :kind (plist-get ev :kind)
                                 :title (plist-get ev :title)
                                 :date (org-chronicle--date-format (plist-get ev :date))
                                 :date-end (and (plist-get ev :date-end)
                                                (org-chronicle--date-format
                                                 (plist-get ev :date-end)))
                                 :people (list name)
                                 :location (plist-get ev :location)))
              changes)))
    (nreverse changes)))

(defun org-chronicle-wikidata--classify (change current)
  "Classify CHANGE against the CURRENT local value string (or nil).
Return `new' when CURRENT is empty, `same' when it equals the change value,
or `conflict' otherwise."
  (let ((value (plist-get change :value)))
    (cond
     ((or (null current) (string-empty-p current)) 'new)
     ((equal (string-trim current) (string-trim value)) 'same)
     (t 'conflict))))

(defun org-chronicle-wikidata--apply-entity-change (change)
  "Set the entity property named in CHANGE at the heading at point.
Also records the provenance URL in SOURCES and marks TRUTH historical."
  (org-set-property (plist-get change :property) (plist-get change :value))
  (org-set-property "SOURCES" (plist-get change :provenance))
  (unless (org-entry-get nil "TRUTH")
    (org-set-property "TRUTH" "historical")))

(defun org-chronicle-wikidata--event-change-string (change)
  "Return the Org heading text for the event in CHANGE.
The heading carries LIFE-EVENT, SUBJECT, and NEW-NAME as applicable."
  (let* ((ev (plist-get change :event))
         (base (org-chronicle--event-string
                :title (plist-get ev :title)
                :truth "historical"
                :date (plist-get ev :date)
                :date-end (plist-get ev :date-end)
                :people (plist-get ev :people)
                :location (plist-get ev :location)
                :sources (plist-get change :provenance))))
    (replace-regexp-in-string
     ":END:\n"
     (concat
      (format ":LIFE-EVENT: %s\n" (plist-get ev :life-event))
      (when (plist-get ev :subject)
        (format ":SUBJECT:  %s\n" (org-chronicle--join (plist-get ev :subject))))
      ":END:\n")
     base t t)))

(defun org-chronicle-wikidata--apply-changes (changes)
  "Apply each approved change at the entity heading at point.
CHANGES is a list of change plists.  Entity changes set properties on the
heading; event changes are appended to the chronicle timeline file via
`org-chronicle--append-event'."
  (org-back-to-heading t)
  (dolist (change changes)
    (pcase (plist-get change :target)
      ('entity (org-chronicle-wikidata--apply-entity-change change))
      ('event (org-chronicle--append-event
               (org-chronicle-wikidata--event-change-string change))))))

(defun org-chronicle-wikidata--candidate-line (cand)
  "Format candidate plist CAND as a completion line."
  (format "%s — %s (%s)"
          (plist-get cand :label)
          (plist-get cand :description)
          (plist-get cand :qid)))

(defun org-chronicle-wikidata--resolve (seed)
  "Resolve SEED (a name) to a confirmed Wikidata QID.
Read a term defaulting to SEED; a pasted QID/URL short-circuits search,
otherwise present search candidates for selection.  Return the QID string."
  (let* ((term (read-string "Wikidata search (or paste QID/URL): " seed))
         (pasted (org-chronicle-wikidata--parse-qid term)))
    (or pasted
        (let* ((cands (org-chronicle-wikidata--search-request term))
               (lines (mapcar #'org-chronicle-wikidata--candidate-line cands))
               (table (cl-mapcar #'cons lines cands)))
          (unless cands (user-error "No Wikidata matches for %S" term))
          (let* ((choice (completing-read "Select item: " lines nil t))
                 (cand (cdr (assoc choice table))))
            (plist-get cand :qid))))))

(defun org-chronicle-wikidata--change-label (change)
  "Return a one-line human label describing CHANGE."
  (pcase (plist-get change :target)
    ('entity (format "%-12s %s" (plist-get change :property)
                     (plist-get change :value)))
    ('event (let ((ev (plist-get change :event)))
              (format "event       %s [%s]" (plist-get ev :title)
                      (plist-get ev :date))))))

(defun org-chronicle-wikidata--review-rows (changes)
  "Build review rows from proposed change plists.
CHANGES is a list of change plists.  Each row is a two-element list
\(STATE plist): STATE has :selected (from the change :default) and :status
\(carried through)."
  (mapcar (lambda (c)
            (list (list :selected (and (plist-get c :default) t)
                        :status (plist-get c :status))
                  c))
          changes))

(defun org-chronicle-wikidata--selected-changes (rows)
  "Return the change plists from ROWS whose STATE has :selected non-nil."
  (delq nil (mapcar (lambda (row)
                      (when (plist-get (nth 0 row) :selected)
                        (nth 1 row)))
                    rows)))

(defvar-local org-chronicle-wikidata--rows nil
  "Review rows for the current review buffer.")

(defvar-local org-chronicle-wikidata--on-confirm nil
  "Function called with the selected changes when the review is confirmed.")

(defun org-chronicle-wikidata-review-toggle ()
  "Toggle selection of the row at point."
  (interactive)
  (let ((idx (- (line-number-at-pos) 3)))
    (when (and (>= idx 0) (< idx (length org-chronicle-wikidata--rows)))
      (let ((state (nth 0 (nth idx org-chronicle-wikidata--rows))))
        (plist-put state :selected (not (plist-get state :selected))))
      (org-chronicle-wikidata--render-review)
      (forward-line (+ idx 3)))))

(defun org-chronicle-wikidata-review-confirm ()
  "Invoke the confirm callback with selected rows and close the review."
  (interactive)
  (let ((selected (org-chronicle-wikidata--selected-changes
                   org-chronicle-wikidata--rows))
        (cb org-chronicle-wikidata--on-confirm))
    (quit-window)
    (when cb (funcall cb selected))))

(defvar org-chronicle-wikidata-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'org-chronicle-wikidata-review-toggle)
    (define-key map (kbd "RET") #'org-chronicle-wikidata-review-confirm)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-chronicle-wikidata-review-mode'.")

(define-derived-mode org-chronicle-wikidata-review-mode special-mode
  "WD-Review"
  "Major mode for reviewing proposed Wikidata changes.")

(defun org-chronicle-wikidata--render-review ()
  "Render `org-chronicle-wikidata--rows' into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Wikidata import — TAB toggles, RET applies selected, q cancels\n\n")
    (dolist (row org-chronicle-wikidata--rows)
      (let* ((state (nth 0 row)) (change (nth 1 row)))
        (insert (format "[%s] %-8s %s\n"
                        (if (plist-get state :selected) "x" " ")
                        (plist-get state :status)
                        (org-chronicle-wikidata--change-label change)))))))

(defun org-chronicle-wikidata--review (changes on-confirm)
  "Open a review buffer; call ON-CONFIRM with the selected change plists.
CHANGES is the list of proposed change plists to present."
  (let ((buf (get-buffer-create "*Wikidata Import*")))
    (with-current-buffer buf
      (org-chronicle-wikidata-review-mode)
      (setq org-chronicle-wikidata--rows
            (org-chronicle-wikidata--review-rows changes))
      (setq org-chronicle-wikidata--on-confirm on-confirm)
      (org-chronicle-wikidata--render-review)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

















(provide 'org-chronicle-wikidata)
;;; org-chronicle-wikidata.el ends here
