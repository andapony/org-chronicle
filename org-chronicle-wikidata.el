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

(require 'org-element)

(require 'seq)



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
  (let* ((url-request-extra-headers
          '(("Accept" . "application/sparql-results+json")
            ("User-Agent" . "org-chronicle (Emacs)")))
         (buf (url-retrieve-synchronously url t t org-chronicle-wikidata-timeout)))
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
?deathPlaceLabel ?fatherLabel ?motherLabel ?label \
(GROUP_CONCAT(DISTINCT ?alias; separator=\"\\u001f\") AS ?aliases) WHERE { \
BIND(wd:%s AS ?p) \
OPTIONAL { ?p p:P569/psv:P569 ?bn. ?bn wikibase:timeValue ?born; wikibase:timePrecision ?bornPrec. } \
OPTIONAL { ?p p:P570/psv:P570 ?dn. ?dn wikibase:timeValue ?died; wikibase:timePrecision ?diedPrec. } \
OPTIONAL { ?p wdt:P19 ?birthPlace. } OPTIONAL { ?p wdt:P20 ?deathPlace. } \
OPTIONAL { ?p wdt:P22 ?father. } OPTIONAL { ?p wdt:P25 ?mother. } \
OPTIONAL { ?p skos:altLabel ?alias. FILTER(LANG(?alias)=\"en\") } \
?p rdfs:label ?label. FILTER(LANG(?label)=\"en\") \
SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". } } \
GROUP BY ?born ?bornPrec ?died ?diedPrec ?birthPlaceLabel ?deathPlaceLabel \
?fatherLabel ?motherLabel ?label" qid))

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
     :label (and v (org-chronicle-wikidata--cell v "label"))
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
         (aliases (let* ((label (plist-get rec :label))
                         (extra (and label (not (equal label name)) (list label))))
                    (append extra (plist-get rec :aliases))))
         changes)
    (dolist (c (list
                (org-chronicle-wikidata--entity-change
                 'vitals "BORN"
                 (and born (org-chronicle--ts (org-chronicle--date-format born)))
                 url)
                (org-chronicle-wikidata--entity-change
                 'vitals "DIED"
                 (and died (org-chronicle--ts (org-chronicle--date-format died)))
                 url)
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
                 (and aliases (org-chronicle--join aliases))
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
                                 :subject (list name (plist-get s :name))
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

(defun org-chronicle-wikidata--dates-equal-p (a b)
  "Non-nil when date strings A and B denote the same Y/M/D after parsing."
  (let ((da (org-chronicle--date-parse a))
        (db (org-chronicle--date-parse b)))
    (and da db
         (equal (plist-get da :year) (plist-get db :year))
         (equal (plist-get da :month) (plist-get db :month))
         (equal (plist-get da :day) (plist-get db :day)))))

(defun org-chronicle-wikidata--classify (change current)
  "Classify CHANGE against the CURRENT local value string (or nil).
Return `new' when CURRENT is empty, `same' when it matches the change value
\(dates compared by parsed value), or `conflict' otherwise."
  (let ((value (plist-get change :value))
        (prop (plist-get change :property)))
    (cond
     ((or (null current) (string-empty-p current)) 'new)
     ((if (member prop '("BORN" "DIED"))
          (org-chronicle-wikidata--dates-equal-p current value)
        (equal (string-trim current) (string-trim value)))
      'same)
     (t 'conflict))))

(defun org-chronicle-wikidata--add-source (url)
  "Add URL to the SOURCES property at point unless already present."
  (let* ((existing (org-chronicle--split (org-entry-get nil "SOURCES")))
         (merged (if (member url existing) existing (append existing (list url)))))
    (org-set-property "SOURCES" (org-chronicle--join merged))))

(defun org-chronicle-wikidata--apply-entity-change (change)
  "Set the entity property named in CHANGE at the heading at point.
Also records the provenance URL in SOURCES and marks TRUTH historical."
  (org-set-property (plist-get change :property) (plist-get change :value))
  (org-chronicle-wikidata--add-source (plist-get change :provenance))
  (unless (org-entry-get nil "TRUTH")
    (org-set-property "TRUTH" "historical")))

(defun org-chronicle-wikidata--event-change-string (change)
  "Return the Org heading text for the event in CHANGE.
Life events (those with a :life-event kind) go through
`org-chronicle--life-event-string'; other events use the generic
`org-chronicle--event-string'."
  (let* ((ev (plist-get change :event))
         (kind (plist-get ev :life-event)))
    (if kind
        (org-chronicle--life-event-string
         :title (plist-get ev :title)
         :kind kind
         :truth "historical"
         :date (plist-get ev :date)
         :subject (plist-get ev :subject)
         :people (plist-get ev :people)
         :location (plist-get ev :location)
         :sources (plist-get change :provenance))
      (org-chronicle--event-string
       :title (plist-get ev :title)
       :truth "historical"
       :date (plist-get ev :date)
       :date-end (plist-get ev :date-end)
       :people (plist-get ev :people)
       :location (plist-get ev :location)
       :sources (plist-get change :provenance)))))

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

(defun org-chronicle-wikidata--heading-name ()
  "Return the entity name for import, seeding from the heading or a property value.
When point is on a PARENTS/SPOUSE property line, use that value; otherwise
use the heading text."
  (or (let ((el (org-element-at-point)))
        (when (eq (org-element-type el) 'node-property)
          (let ((p (org-element-property :key el)))
            (when (member p '("PARENTS" "SPOUSE"))
              (car (org-chronicle--split (org-element-property :value el)))))))
      (org-get-heading t t t t)))

(defun org-chronicle-wikidata--ensure-entity (name)
  "Ensure point is on a person entity heading for NAME.
If point is not already on a KIND heading, create the person via
`org-chronicle-add-person' and move to it."
  (unless (org-entry-get nil "KIND")
    (org-chronicle-add-person name)))

;;;###autoload
(defun org-chronicle-wikidata-import ()
  "Import a person's Wikidata life events into the chronicle.
Resolve-or-create the entity at point (or seeded from a PARENTS/SPOUSE value),
fetch from Wikidata, review the proposed changes, and write the approved set."
  (interactive)
  (org-back-to-heading t)
  (let* ((name (org-chronicle-wikidata--heading-name))
         (stored (org-entry-get nil "WIKIDATA"))
         (qid (or stored (org-chronicle-wikidata--resolve name)))
         (rec (org-chronicle-wikidata--fetch-person qid))
         (changes (org-chronicle-wikidata--record->changes rec name))
         (marker (point-marker)))
    (when (seq-empty-p changes)
      (user-error "Nothing to import for %s (%s)" name qid))
    (dolist (c changes)
      (when (eq (plist-get c :target) 'entity)
        (plist-put c :status
                   (org-chronicle-wikidata--classify
                    c (org-entry-get nil (plist-get c :property))))))
    (org-chronicle-wikidata--review
     changes
     (lambda (selected)
       (org-with-point-at marker
         (org-chronicle-wikidata--apply-changes selected)
         (message "Imported %d change(s) for %s" (length selected) name))))))

(defun org-chronicle-wikidata--diff (changes current-fn)
  "Return entity edits that drift from local values via CURRENT-FN.
CHANGES is the proposed change list; CURRENT-FN takes a property name and
returns the current local value (or nil).
Same-valued changes are excluded; new and conflicting ones are returned, each
annotated with :status and :current."
  (delq nil
        (mapcar
         (lambda (c)
           (when (eq (plist-get c :target) 'entity)
             (let* ((cur (funcall current-fn (plist-get c :property)))
                    (status (org-chronicle-wikidata--classify c cur)))
               (unless (eq status 'same)
                 (append (list :status status :current cur) c)))))
         changes)))

;;;###autoload
(defun org-chronicle-wikidata-reconcile ()
  "Re-query the stored Wikidata item and present drift as opt-in pulls."
  (interactive)
  (org-back-to-heading t)
  (let ((qid (org-entry-get nil "WIKIDATA")))
    (unless qid
      (user-error "No WIKIDATA property here; run org-chronicle-wikidata-import first"))
    (let* ((name (org-get-heading t t t t))
           (rec (org-chronicle-wikidata--fetch-person qid))
           (changes (org-chronicle-wikidata--record->changes rec name))
           (drift (org-chronicle-wikidata--diff
                   changes (lambda (p) (org-entry-get nil p))))
           (marker (point-marker)))
      (when (seq-empty-p drift)
        (user-error "No drift from Wikidata for %s (%s)" name qid))
      (org-chronicle-wikidata--review
       drift
       (lambda (selected)
         (org-with-point-at marker
           (org-chronicle-wikidata--apply-changes selected)
           (message "Reconciled %d field(s) for %s" (length selected) name)))))))






















(provide 'org-chronicle-wikidata)
;;; org-chronicle-wikidata.el ends here
