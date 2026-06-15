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
              "\\`\\+?\\([0-9]\\{4,\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
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
  (format "SELECT ?label (SAMPLE(?bpl) AS ?birthPlaceLabel) \
(SAMPLE(?dpl) AS ?deathPlaceLabel) (SAMPLE(?fl) AS ?fatherLabel) \
(SAMPLE(?ml) AS ?motherLabel) \
(GROUP_CONCAT(DISTINCT ?alias; separator=\"\\u001f\") AS ?aliases) WHERE { \
BIND(wd:%s AS ?p) \
?p rdfs:label ?label. FILTER(LANG(?label)=\"en\") \
OPTIONAL { ?p wdt:P19 ?bp. ?bp rdfs:label ?bpl. FILTER(LANG(?bpl)=\"en\") } \
OPTIONAL { ?p wdt:P20 ?dp. ?dp rdfs:label ?dpl. FILTER(LANG(?dpl)=\"en\") } \
OPTIONAL { ?p wdt:P22 ?f. ?f rdfs:label ?fl. FILTER(LANG(?fl)=\"en\") } \
OPTIONAL { ?p wdt:P25 ?m. ?m rdfs:label ?ml. FILTER(LANG(?ml)=\"en\") } \
OPTIONAL { ?p skos:altLabel ?alias. FILTER(LANG(?alias)=\"en\") } } \
GROUP BY ?label" qid))

(defun org-chronicle-wikidata--spouses-query (qid)
  "Return the SPARQL spouses query for QID (one row per spouse)."
  (format "SELECT ?spouse ?spouseLabel ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:P26 ?st. ?st ps:P26 ?spouse. \
OPTIONAL { ?st pqv:P580 ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:P582 ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } \
SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". } }" qid))

(defun org-chronicle-wikidata--events-query (qid)
  "Return the SPARQL positions-held query for QID (one row per position)."
  (format "SELECT ?pos ?title ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:P39 ?st. ?st ps:P39 ?pos. \
?pos rdfs:label ?title. FILTER(LANG(?title)=\"en\") \
OPTIONAL { ?st pqv:P580 ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:P582 ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } }" qid))

(defun org-chronicle-wikidata--rank-symbol (uri)
  "Return `preferred', `normal', or `deprecated' for a wikibase:rank URI, else nil."
  (cond ((not (stringp uri)) nil)
        ((string-suffix-p "PreferredRank" uri) 'preferred)
        ((string-suffix-p "NormalRank" uri) 'normal)
        ((string-suffix-p "DeprecatedRank" uri) 'deprecated)
        (t nil)))

(defun org-chronicle-wikidata--dates->candidates (rows)
  "Map dates-query ROWS to neutral date-candidate plists."
  (mapcar
   (lambda (row)
     (let ((raw (org-chronicle-wikidata--cell row "value"))
           (prec (org-chronicle-wikidata--cell-int row "prec")))
       (list :prop (org-chronicle-wikidata--cell row "prop")
             :date (org-chronicle-wikidata--time->date raw prec)
             :raw raw
             :precision prec
             :rank (org-chronicle-wikidata--rank-symbol
                    (org-chronicle-wikidata--cell row "rank")))))
   rows))

(defconst org-chronicle-wikidata--kind-profiles
  '((person :start-pid "P569" :end-pid "P570" :start-prop "BORN" :end-prop "DIED")
    (place  :start-pid "P571" :end-pid "P576" :start-prop "BUILT" :end-prop "RAZED")
    (group  :start-pid "P571" :end-pid "P576" :start-prop "FOUNDED" :end-prop "DISBANDED"))
  "Per-KIND Wikidata span PIDs and chronicle span property names.")

(defun org-chronicle-wikidata--kind-span-pids (kind)
  "Return (START-PID . END-PID) Wikidata property ids for KIND."
  (let ((p (alist-get kind org-chronicle-wikidata--kind-profiles)))
    (cons (plist-get p :start-pid) (plist-get p :end-pid))))

(defun org-chronicle-wikidata--kind-span-props (kind)
  "Return (START-PROP . END-PROP) chronicle property names for KIND."
  (let ((p (alist-get kind org-chronicle-wikidata--kind-profiles)))
    (cons (plist-get p :start-prop) (plist-get p :end-prop))))

(defun org-chronicle-wikidata--kind-file (kind)
  "Return the file new KIND entities are written to."
  (if (eq kind 'place) (org-chronicle--places-file) (org-chronicle--people-file)))

(defun org-chronicle-wikidata--span-query (qid start-pid end-pid)
  "Return a SPARQL query for QID's START-PID and END-PID date statements.
One row per statement, tagged ?prop \"start\"/\"end\", with value, precision,
and rank."
  (format "SELECT ?prop ?value ?prec ?rank WHERE { \
{ wd:%s p:%s ?st. ?st psv:%s ?n. ?n wikibase:timeValue ?value; \
wikibase:timePrecision ?prec. ?st wikibase:rank ?rank. BIND(\"start\" AS ?prop) } \
UNION \
{ wd:%s p:%s ?st. ?st psv:%s ?n. ?n wikibase:timeValue ?value; \
wikibase:timePrecision ?prec. ?st wikibase:rank ?rank. BIND(\"end\" AS ?prop) } }"
          qid start-pid start-pid qid end-pid end-pid))

(defun org-chronicle-wikidata--span-select (rows)
  "Select start and end dates from span-query ROWS (parsed bindings).
Return (:start DATE :start-alternates LIST :end DATE :end-alternates LIST)."
  (let* ((cands (org-chronicle-wikidata--dates->candidates rows))
         (start (org-chronicle-wikidata--select-candidate
                 (cl-remove-if-not (lambda (c) (equal (plist-get c :prop) "start")) cands)))
         (end (org-chronicle-wikidata--select-candidate
               (cl-remove-if-not (lambda (c) (equal (plist-get c :prop) "end")) cands))))
    (list :start (plist-get start :date) :start-alternates (plist-get start :alternates)
          :end (plist-get end :date) :end-alternates (plist-get end :alternates))))

(defun org-chronicle-wikidata--create-entity (name kind)
  "Create a minimal KIND entity NAME in the kind's file; return a marker."
  (with-current-buffer (find-file-noselect (org-chronicle-wikidata--kind-file kind))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (org-chronicle--entity-string :name name :kind kind))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (save-buffer)
    (point-marker)))








(defun org-chronicle-wikidata--fetch-record (qid kind)
  "Fetch QID from Wikidata as a KIND record (person, place, or group)."
  (let* ((pids (org-chronicle-wikidata--kind-span-pids kind))
         (vitals (org-chronicle-wikidata--sparql-request
                  (org-chronicle-wikidata--vitals-query qid)))
         (span (org-chronicle-wikidata--sparql-request
                (org-chronicle-wikidata--span-query qid (car pids) (cdr pids)))))
    (if (eq kind 'person)
        (org-chronicle-wikidata--rows->record
         qid vitals span
         (org-chronicle-wikidata--sparql-request (org-chronicle-wikidata--spouses-query qid))
         (org-chronicle-wikidata--sparql-request (org-chronicle-wikidata--events-query qid)))
      (let* ((v (car vitals))
             (alias-str (and v (org-chronicle-wikidata--cell v "aliases")))
             (sp (org-chronicle-wikidata--span-select span)))
        (list :qid qid :kind kind
              :label (and v (org-chronicle-wikidata--cell v "label"))
              :aliases (and alias-str (not (string-empty-p alias-str))
                            (split-string alias-str org-chronicle-wikidata--alias-separator t))
              :start (plist-get sp :start) :start-alternates (plist-get sp :start-alternates)
              :end (plist-get sp :end) :end-alternates (plist-get sp :end-alternates))))))

(defun org-chronicle-wikidata--fetch-person (qid)
  "Fetch QID from Wikidata as a person record."
  (org-chronicle-wikidata--fetch-record qid 'person))


(defun org-chronicle-wikidata--ordinal (n)
  "Return N as an English ordinal string, e.g. 17 -> \"17th\"."
  (let ((suffix (cond ((memq (% n 100) '(11 12 13)) "th")
                      ((= (% n 10) 1) "st")
                      ((= (% n 10) 2) "nd")
                      ((= (% n 10) 3) "rd")
                      (t "th"))))
    (format "%d%s" n suffix)))

(defun org-chronicle-wikidata--coarse-date-label (raw precision)
  "Return a display label for an unrepresentable Wikidata time value RAW.
PRECISION is the integer Wikidata precision; decade is 8 and century is 7."
  (if (not (and (stringp raw) (string-match "\\`\\([-+]?\\)\\([0-9]+\\)" raw)))
      (or raw "?")
    (let ((bce (equal (match-string 1 raw) "-"))
          (year (string-to-number (match-string 2 raw))))
      (cond
       (bce (format "%d BC" year))
       ((eq precision 8) (format "%ds" (- year (% year 10))))
       ((eq precision 7) (format "%s century"
                                 (org-chronicle-wikidata--ordinal
                                  (1+ (/ (1- year) 100)))))
       (t (format "%d" year))))))

(defun org-chronicle-wikidata--candidate-label (cand)
  "Return a display string for date-candidate CAND."
  (if (plist-get cand :date)
      (org-chronicle--date-format (plist-get cand :date))
    (org-chronicle-wikidata--coarse-date-label
     (plist-get cand :raw) (plist-get cand :precision))))

(defun org-chronicle-wikidata--select-candidate (candidates)
  "Choose the best date among CANDIDATES (for one property).
Drop deprecated, keep representable, prefer preferred rank, then highest
precision.  Return a plist (:date CHOSEN-OR-NIL :alternates LIST-OF-STRINGS),
where alternates are the other distinct values as display strings."
  (let* ((live (cl-remove-if (lambda (c) (eq (plist-get c :rank) 'deprecated)) candidates))
         (representable (cl-remove-if-not (lambda (c) (plist-get c :date)) live))
         (preferred (cl-remove-if-not (lambda (c) (eq (plist-get c :rank) 'preferred))
                                      representable))
         (pool (if preferred preferred representable))
         (chosen (car (sort (copy-sequence pool)
                            (lambda (a b) (> (or (plist-get a :precision) 0)
                                             (or (plist-get b :precision) 0))))))
         (chosen-date (plist-get chosen :date))
         (chosen-label (and chosen-date (org-chronicle--date-format chosen-date)))
         (alternates
          (delete-dups
           (delq nil
                 (mapcar (lambda (c)
                           (let ((lbl (org-chronicle-wikidata--candidate-label c)))
                             (unless (and chosen-label (equal lbl chosen-label)) lbl)))
                         candidates)))))
    (list :date chosen-date :alternates alternates)))

(defun org-chronicle-wikidata--rows->record (qid vitals dates spouses events)
  "Assemble a person record for QID from parsed binding lists.
VITALS is the single vitals row list; DATES, SPOUSES, EVENTS are row lists.
Returns a plist; unrepresentable dates are dropped and competing date
statements are resolved by rank then precision (see
`org-chronicle-wikidata--select-candidate')."
  (let* ((v (car vitals))
         (alias-str (and v (org-chronicle-wikidata--cell v "aliases")))
         (span (org-chronicle-wikidata--span-select dates)))
    (list
     :qid qid
     :kind 'person
     :label (and v (org-chronicle-wikidata--cell v "label"))
     :born (plist-get span :start)
     :born-alternates (plist-get span :start-alternates)
     :died (plist-get span :end)
     :died-alternates (plist-get span :end-alternates)
     :birthplace (and v (org-chronicle-wikidata--cell v "birthPlaceLabel"))
     :deathplace (and v (org-chronicle-wikidata--cell v "deathPlaceLabel"))
     :father (and v (org-chronicle-wikidata--cell v "fatherLabel"))
     :mother (and v (org-chronicle-wikidata--cell v "motherLabel"))
     :aliases (and alias-str (not (string-empty-p alias-str))
                   (split-string alias-str org-chronicle-wikidata--alias-separator t))
     :spouses (mapcar (lambda (row)
                        (list :name (org-chronicle-wikidata--cell row "spouseLabel")
                              :qid (org-chronicle-wikidata--parse-qid
                                    (org-chronicle-wikidata--cell row "spouse"))
                              :date (org-chronicle-wikidata--row-date row "start" "startPrec")
                              :end (org-chronicle-wikidata--row-date row "end" "endPrec")))
                      spouses)
     :events (mapcar (lambda (row)
                       (list :kind "position"
                             :title (org-chronicle-wikidata--cell row "title")
                             :qid (org-chronicle-wikidata--parse-qid
                                   (org-chronicle-wikidata--cell row "pos"))
                             :date (org-chronicle-wikidata--row-date row "start" "startPrec")
                             :date-end (org-chronicle-wikidata--row-date row "end" "endPrec")
                             :location nil))
                     events))))

(defun org-chronicle-wikidata--url (qid)
  "Return the canonical Wikidata item URL for QID."
  (concat "https://www.wikidata.org/wiki/" qid))

(defun org-chronicle-wikidata--entity-change (group property value url &optional alternates)
  "Build an entity change plist for PROPERTY=VALUE in GROUP, sourced to URL.
ALTERNATES, when non-nil, is a list of display strings attached as :alternates.
Return nil when VALUE is nil or empty."
  (and value (not (string-empty-p value))
       (append (list :target 'entity :group group :property property :value value
                     :provenance url :default t)
               (and alternates (list :alternates alternates)))))

(defun org-chronicle-wikidata--entity-record->changes (rec)
  "Build entity change plists for place/group record REC.
Return span, aliases, and WIKIDATA property changes."
  (let* ((qid (plist-get rec :qid))
         (kind (plist-get rec :kind))
         (url (org-chronicle-wikidata--url qid))
         (props (org-chronicle-wikidata--kind-span-props kind))
         (start (plist-get rec :start))
         (end (plist-get rec :end))
         (aliases (plist-get rec :aliases)))
    (delq nil
          (list
           (org-chronicle-wikidata--entity-change
            'vitals (car props)
            (and start (org-chronicle--ts (org-chronicle--date-format start)))
            url (plist-get rec :start-alternates))
           (org-chronicle-wikidata--entity-change
            'vitals (cdr props)
            (and end (org-chronicle--ts (org-chronicle--date-format end)))
            url (plist-get rec :end-alternates))
           (org-chronicle-wikidata--entity-change
            'vitals "WIKIDATA" qid url)
           (org-chronicle-wikidata--entity-change
            'relations "ALIASES" (and aliases (org-chronicle--join aliases)) url)))))

(defun org-chronicle-wikidata--record->changes (rec name)
  "Map person record REC (for person NAME) to a list of change plists.
Each change targets either an entity property or an event entry.
See the data contract in the package commentary for field names."
  (if (memq (plist-get rec :kind) '(place group))
      (org-chronicle-wikidata--entity-record->changes rec)
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
                   url (plist-get rec :born-alternates))
                  (org-chronicle-wikidata--entity-change
                   'vitals "DIED"
                   (and died (org-chronicle--ts (org-chronicle--date-format died)))
                   url (plist-get rec :died-alternates))
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
                                   :object-qid (plist-get s :qid)
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
                                   :object-qid (plist-get ev :qid)
                                   :title (plist-get ev :title)
                                   :date (org-chronicle--date-format (plist-get ev :date))
                                   :date-end (and (plist-get ev :date-end)
                                                  (org-chronicle--date-format
                                                   (plist-get ev :date-end)))
                                   :people (list name)
                                   :location (plist-get ev :location)))
                changes)))
      (nreverse changes))))

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
     ((if (member prop '("BORN" "DIED" "BUILT" "RAZED" "FOUNDED" "DISBANDED"))
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

(defun org-chronicle-wikidata--apply-changes (changes index)
  "Write approved change plists to the chronicle at the heading at point.
CHANGES is a list of change plists; INDEX maps IMPORT-KEY to markers in the
events file.  Entity changes set properties on the heading; event changes are
written idempotently to the events file."
  (org-back-to-heading t)
  (dolist (change changes)
    (pcase (plist-get change :target)
      ('entity (org-chronicle-wikidata--apply-entity-change change))
      ('event (org-chronicle-wikidata--apply-event-change change index)))))

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
    ('entity (let ((alts (plist-get change :alternates)))
               (format "%-12s %s%s"
                       (plist-get change :property)
                       (plist-get change :value)
                       (if alts
                           (format "  (Wikidata also lists: %s)"
                                   (mapconcat #'identity alts "; "))
                         ""))))
    ('event (let ((ev (plist-get change :event)))
              (format "event       %s [%s]" (plist-get ev :title)
                      (plist-get ev :date))))))

(defun org-chronicle-wikidata--review-rows (changes)
  "Build review rows from proposed change plists.
CHANGES is a list of change plists.  Each row is a two-element list
\(STATE plist): STATE has :selected (default proposals that are new) and
:status (carried through)."
  (mapcar (lambda (c)
            (list (list :selected (and (plist-get c :default)
                                       (eq (plist-get c :status) 'new))
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

(defun org-chronicle-wikidata--label-at-point ()
  "Return a person label when point is on a PARENTS or SPOUSE property value.
Return nil otherwise."
  (let ((el (org-element-at-point)))
    (when (eq (org-element-type el) 'node-property)
      (let ((key (org-element-property :key el)))
        (when (member key '("PARENTS" "SPOUSE"))
          (car (org-chronicle--split (org-element-property :value el))))))))

(defun org-chronicle-wikidata--create-person (name)
  "Create a minimal person entity NAME in the people file.
Return a marker at the new heading."
  (with-current-buffer (find-file-noselect (org-chronicle--people-file))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (org-chronicle--entity-string :name name :kind 'person))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (save-buffer)
    (point-marker)))

;;;###autoload
(defun org-chronicle-wikidata-import ()
  "Import a person's Wikidata life events into the chronicle.
With point on a person entity heading, enrich that entity.  With point on a
PARENTS or SPOUSE property value, or on a non-entity heading, create a new
person entity in the people file and enrich it.  In all cases, resolve the
Wikidata item, review the proposed edits, and write the approved set."
  (interactive)
  (let* ((promote (org-chronicle-wikidata--label-at-point))
         (kind (unless promote
                 (save-excursion (org-back-to-heading t) (org-entry-get nil "KIND"))))
         (seed (or promote
                   (save-excursion (org-back-to-heading t) (org-get-heading t t t t))))
         (stored (and kind
                      (save-excursion (org-back-to-heading t)
                                      (org-entry-get nil "WIKIDATA"))))
         (qid (or stored (org-chronicle-wikidata--resolve seed)))
         (marker (if kind
                     (save-excursion (org-back-to-heading t) (point-marker))
                   (org-chronicle-wikidata--create-person seed)))
         (rec (org-chronicle-wikidata--fetch-person qid))
         (changes (org-chronicle-wikidata--record->changes rec seed)))
    (when (seq-empty-p changes)
      (user-error "Nothing to import for %s (%s)" seed qid))
    (let* ((subject-qid qid)
           (subject-orgid (org-with-point-at marker (org-id-get-create)))
           (index (org-chronicle-wikidata--events-index)))
      (setq changes
            (org-chronicle-wikidata--classify-changes
             changes marker subject-orgid subject-qid index))
      (org-chronicle-wikidata--review
       changes
       (lambda (selected)
         (org-with-point-at marker
                            (org-chronicle-wikidata--apply-changes selected index)
                            (when (buffer-file-name) (save-buffer))
                            (message "Imported %d change(s) for %s" (length selected) seed)))))))

(defun org-chronicle-wikidata--classify-changes (changes marker subject-orgid subject-qid index)
  "Stamp each proposed change with :status (and event :key); drop keyless events.
CHANGES is the raw change list.  Entity changes classify against the heading at
MARKER; event changes classify against INDEX by their IMPORT-KEY derived from
SUBJECT-ORGID/SUBJECT-QID."
  (delq nil
        (mapcar
         (lambda (c)
           (pcase (plist-get c :target)
             ('entity
              (plist-put c :status
                         (org-chronicle-wikidata--classify
                          c (org-with-point-at marker
                                               (org-entry-get nil (plist-get c :property)))))
              c)
             ('event
              (let ((key (org-chronicle-wikidata--event-key
                          (plist-get c :event) subject-orgid subject-qid)))
                (when key
                  (plist-put c :key key)
                  (plist-put c :status
                             (org-chronicle-wikidata--classify-event
                              c (gethash key index)))
                  c)))))
         changes)))

(defcustom org-chronicle-wikidata-file nil
  "File where imported Wikidata events are filed.
When nil, defaults to \"imported/events.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle-wikidata)

(defun org-chronicle-wikidata--events-file ()
  "Return the file imported events are written to.
Defaults to \"imported/events.org\" under `org-chronicle-root'."
  (or org-chronicle-wikidata-file
      (expand-file-name "imported/events.org" org-chronicle-root)))

(defun org-chronicle-wikidata--event-key (event subject-orgid subject-qid)
  "Return the IMPORT-KEY for EVENT, or nil when a required id is missing.
SUBJECT-ORGID is the chronicle entity's org id and SUBJECT-QID its Wikidata
QID.  EVENT is an event change's :event plist.  Birth and death key on the
chronicle subject; positions add the office QID; marriage keys on the sorted
pair of both participants' prefixed QIDs so it is symmetric."
  (let ((kind (plist-get event :kind))
        (obj (plist-get event :object-qid)))
    (pcase kind
      ("marriage"
       (and subject-qid obj
            (format "marriage:%s"
                    (mapconcat #'identity
                               (sort (list (concat "wd:" subject-qid)
                                           (concat "wd:" obj))
                                     #'string<)
                               ":"))))
      ("position"
       (and obj (format "position:%s:wd:%s" subject-orgid obj)))
      (_ (and subject-orgid (format "%s:%s" kind subject-orgid))))))

;;;###autoload
(defun org-chronicle-wikidata-reconcile ()
  "Re-query the stored Wikidata item and present entity and event drift.
Drift is shown as opt-in pulls; nothing is overwritten without selection."
  (interactive)
  (org-back-to-heading t)
  (let ((qid (org-entry-get nil "WIKIDATA")))
    (unless qid
      (user-error "No WIKIDATA property here; run org-chronicle-wikidata-import first"))
    (let* ((name (org-get-heading t t t t))
           (marker (point-marker))
           (subject-orgid (org-with-point-at marker (org-id-get-create)))
           (index (org-chronicle-wikidata--events-index))
           (rec (org-chronicle-wikidata--fetch-person qid))
           (changes (org-chronicle-wikidata--classify-changes
                     (org-chronicle-wikidata--record->changes rec name)
                     marker subject-orgid qid index))
           (drift (cl-remove-if (lambda (c) (eq (plist-get c :status) 'same)) changes)))
      (when (seq-empty-p drift)
        (user-error "No drift from Wikidata for %s (%s)" name qid))
      (org-chronicle-wikidata--review
       drift
       (lambda (selected)
         (org-with-point-at marker
                            (org-chronicle-wikidata--apply-changes selected index)
                            (when (buffer-file-name) (save-buffer))
                            (message "Reconciled %d change(s) for %s" (length selected) name)))))))

(defun org-chronicle-wikidata--events-index ()
  "Return a hash mapping each IMPORT-KEY to a marker in the events file.
Scans `org-chronicle-wikidata--events-file'; an absent file yields an empty
table."
  (let ((file (org-chronicle-wikidata--events-file))
        (index (make-hash-table :test 'equal)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((key (org-entry-get nil "IMPORT-KEY")))
              (when key (puthash key (point-marker) index))))))))
    index))

(defun org-chronicle-wikidata--append-to-events-file (text key index)
  "Append heading TEXT to the events file, stamp KEY, and register it in INDEX.
Creates the file's directory if needed, assigns an id, normalizes, and saves."
  (let ((file (org-chronicle-wikidata--events-file)))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert text)
      (forward-line -1)
      (org-back-to-heading t)
      (when key (org-set-property "IMPORT-KEY" key))
      (org-id-get-create)
      (org-chronicle-normalize)
      (save-buffer)
      (when key (puthash key (point-marker) index)))))

(defun org-chronicle-wikidata--field-equal-p (prop proposed date-p)
  "Non-nil if PROP at point equals PROPOSED (a string or nil).
Empty and nil are equal; compare as dates when DATE-P is non-nil."
  (let* ((cur (org-entry-get nil prop))
         (cur (and cur (not (string-empty-p cur)) cur))
         (proposed (and proposed (not (string-empty-p proposed)) proposed)))
    (cond
     ((and (null cur) (null proposed)) t)
     ((or (null cur) (null proposed)) nil)
     (date-p (org-chronicle-wikidata--dates-equal-p cur proposed))
     (t (equal (string-trim cur) (string-trim proposed))))))

(defun org-chronicle-wikidata--classify-event (change existing)
  "Classify event CHANGE against the EXISTING marker (or nil).
Return `new' when EXISTING is nil, `same' when the managed date and location
fields match, or `conflict' otherwise."
  (if (null existing)
      'new
    (let ((ev (plist-get change :event)))
      (org-with-point-at existing
        (if (and (org-chronicle-wikidata--field-equal-p "DATE" (plist-get ev :date) t)
                 (org-chronicle-wikidata--field-equal-p "DATE-END" (plist-get ev :date-end) t)
                 (org-chronicle-wikidata--field-equal-p "LOCATION" (plist-get ev :location) nil))
            'same
          'conflict)))))

(defun org-chronicle-wikidata--apply-event-change (change index)
  "Apply event CHANGE idempotently using INDEX (IMPORT-KEY -> marker).
Update the keyed heading's managed properties in place when it exists,
otherwise append a new heading carrying the key."
  (let* ((key (plist-get change :key))
         (existing (and key (gethash key index)))
         (ev (plist-get change :event)))
    (if existing
        (org-with-point-at existing
          (org-set-property "DATE" (org-chronicle--ts (plist-get ev :date)))
          (if (plist-get ev :date-end)
              (org-set-property "DATE-END" (org-chronicle--ts (plist-get ev :date-end)))
            (org-delete-property "DATE-END"))
          (if (and (plist-get ev :location)
                   (not (string-empty-p (plist-get ev :location))))
              (org-set-property "LOCATION" (plist-get ev :location))
            (org-delete-property "LOCATION"))
          (org-chronicle-wikidata--add-source (plist-get change :provenance))
          (unless (org-entry-get nil "TRUTH")
            (org-set-property "TRUTH" "historical"))
          (save-buffer))
      (org-chronicle-wikidata--append-to-events-file
       (org-chronicle-wikidata--event-change-string change) key index))))

(provide 'org-chronicle-wikidata)
;;; org-chronicle-wikidata.el ends here
