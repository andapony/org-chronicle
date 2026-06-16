;;; org-chronicle-wikibase.el --- Wikidata life-events import for org-chronicle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: org-chronicle contributors
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; Import a person's vitals, relations, and curated events from Wikidata
;; into an org-chronicle project.  See `org-chronicle-import' and
;; `org-chronicle-reconcile' (defined in `org-chronicle-sources').

;;; Code:

(require 'org-chronicle)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

(require 'org-element)

(require 'seq)

(declare-function org-chronicle-sources--pid "org-chronicle-sources" (source role))

(declare-function org-chronicle-sources--get "org-chronicle-sources" (id))

(declare-function org-chronicle-sources--kind-span-props "org-chronicle-sources" (kind))

(declare-function org-chronicle-sources--kind-file "org-chronicle-sources" (kind))

(declare-function org-chronicle-sources--span-pids "org-chronicle-sources" (source kind))

(defgroup org-chronicle-wikibase nil
  "Wikidata integration for org-chronicle."
  :group 'org-chronicle)

(defun org-chronicle-wikibase--parse-qid (s)
  "Return the canonical Wikidata QID in string S, or nil.
S may be a bare QID, a wiki URL, or an entity URI, case-insensitively."
  (when (stringp s)
    (let ((trimmed (string-trim s)))
      (when (string-match
             "\\(?:^\\|/\\)\\([Qq][0-9]+\\)\\(?:$\\|[/?#]\\)?"
             trimmed)
        (upcase (match-string 1 trimmed))))))

(defun org-chronicle-wikibase--time->date (time precision)
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

(defun org-chronicle-wikibase--bindings (json)
  "Parse SPARQL JSON string JSON; return a list of binding alists."
  (let* ((data (json-parse-string json :object-type 'alist :array-type 'list))
         (results (alist-get 'results data))
         (bindings (alist-get 'bindings results)))
    bindings))

(defun org-chronicle-wikibase--cell (row var)
  "Return the string value of VAR in binding alist ROW, or nil.
VAR is a string variable name."
  (let ((b (alist-get (intern var) row)))
    (and b (alist-get 'value b))))

(defun org-chronicle-wikibase--cell-int (row var)
  "Return the integer value of VAR in ROW, or nil when absent or non-numeric."
  (let ((v (org-chronicle-wikibase--cell row var)))
    (and v (string-match-p "\\`[0-9]+\\'" v) (string-to-number v))))

(defconst org-chronicle-wikibase--alias-separator "\x1f"
  "Separator used in SPARQL GROUP_CONCAT of aliases.")

(defun org-chronicle-wikibase--row-date (row val-var prec-var)
  "Build a date plist from VAL-VAR and PREC-VAR cells of ROW, or nil."
  (org-chronicle-wikibase--time->date
   (org-chronicle-wikibase--cell row val-var)
   (org-chronicle-wikibase--cell-int row prec-var)))

(defcustom org-chronicle-wikibase-timeout 20
  "Seconds to wait for a Wikidata HTTP response before failing."
  :type 'integer
  :group 'org-chronicle-wikibase)

(define-error 'org-chronicle-wikibase-error "Wikidata request failed")

(define-error 'org-chronicle-wikibase-rate-limited
  "Wikidata rate limited the request" 'org-chronicle-wikibase-error)

(defun org-chronicle-wikibase--http-get (url)
  "GET URL and return the response body as a string.
Signal `org-chronicle-wikibase-rate-limited' on HTTP 429 and
`org-chronicle-wikibase-error' on any other failure or timeout."
  (let* ((url-request-extra-headers
          '(("Accept" . "application/sparql-results+json")
            ("User-Agent" . "org-chronicle (Emacs)")))
         (buf (url-retrieve-synchronously url t t org-chronicle-wikibase-timeout)))
    (unless buf (signal 'org-chronicle-wikibase-error (list "no response" url)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((status (and (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                             (string-to-number (match-string 1)))))
            (cond
             ((eq status 429) (signal 'org-chronicle-wikibase-rate-limited (list url)))
             ((and status (>= status 400))
              (signal 'org-chronicle-wikibase-error (list status url)))))
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                                'utf-8))
      (kill-buffer buf))))

(defun org-chronicle-wikibase--sparql-request (source query)
  "Run SPARQL QUERY against SOURCE's endpoint; return parsed binding rows."
  (org-chronicle-wikibase--bindings
   (org-chronicle-wikibase--http-get
    (concat (plist-get source :sparql-endpoint)
            "?format=json&query=" (url-hexify-string query)))))

(defun org-chronicle-wikibase--search-request (source term)
  "Search SOURCE for TERM; return a list of candidate plists.
Each candidate is (:qid :label :description)."
  (let* ((url (concat (plist-get source :api-endpoint)
                      "?action=wbsearchentities&format=json&language=en"
                      "&type=item&limit=10&search=" (url-hexify-string term)))
         (data (json-parse-string (org-chronicle-wikibase--http-get url)
                                  :object-type 'alist :array-type 'list)))
    (mapcar (lambda (hit)
              (list :qid (alist-get 'id hit)
                    :label (alist-get 'label hit)
                    :description (or (alist-get 'description hit) "")))
            (alist-get 'search data))))

(defun org-chronicle-wikibase--label-filter (source var)
  "Return a SPARQL FILTER restricting VAR to SOURCE's label languages.
SOURCE's :label-language is a list; in Phase 3 each source uses a single
language, so this restricts VAR to that language."
  (concat "FILTER("
          (mapconcat (lambda (l) (format "LANG(%s)=\"%s\"" var l))
                     (plist-get source :label-language) "||")
          ") "))


(defun org-chronicle-wikibase--vitals-query (source qid)
  "Return the SPARQL vitals query for QID against SOURCE (single result row)."
  (let ((bpl (org-chronicle-sources--pid source :birthplace))
        (dpl (org-chronicle-sources--pid source :deathplace))
        (fa (org-chronicle-sources--pid source :father))
        (mo (org-chronicle-sources--pid source :mother)))
    (concat
     (org-chronicle-wikibase--prefixes (plist-get source :base-uri))
     (format "SELECT ?label (SAMPLE(?bpl) AS ?birthPlaceLabel) \
(SAMPLE(?dpl) AS ?deathPlaceLabel) (SAMPLE(?fl) AS ?fatherLabel) \
(SAMPLE(?ml) AS ?motherLabel) \
(GROUP_CONCAT(DISTINCT ?alias; separator=\"\\u001f\") AS ?aliases) WHERE { \
BIND(wd:%s AS ?p) \
?p rdfs:label ?label. %s\
OPTIONAL { ?p wdt:%s ?bp. ?bp rdfs:label ?bpl. %s} \
OPTIONAL { ?p wdt:%s ?dp. ?dp rdfs:label ?dpl. %s} \
OPTIONAL { ?p wdt:%s ?f. ?f rdfs:label ?fl. %s} \
OPTIONAL { ?p wdt:%s ?m. ?m rdfs:label ?ml. %s} \
OPTIONAL { ?p skos:altLabel ?alias. %s} } GROUP BY ?label"
             qid
             (org-chronicle-wikibase--label-filter source "?label")
             bpl (org-chronicle-wikibase--label-filter source "?bpl")
             dpl (org-chronicle-wikibase--label-filter source "?dpl")
             fa (org-chronicle-wikibase--label-filter source "?fl")
             mo (org-chronicle-wikibase--label-filter source "?ml")
             (org-chronicle-wikibase--label-filter source "?alias")))))

(defun org-chronicle-wikibase--spouses-query (source qid)
  "Return the SPARQL spouses query for QID against SOURCE (one row per spouse)."
  (let ((sp (org-chronicle-sources--pid source :spouse))
        (qs (org-chronicle-sources--pid source :qual-start))
        (qe (org-chronicle-sources--pid source :qual-end)))
    (concat
     (org-chronicle-wikibase--prefixes (plist-get source :base-uri))
     (format "SELECT ?spouse ?spouseLabel ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:%s ?st. ?st ps:%s ?spouse. \
OPTIONAL { ?st pqv:%s ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:%s ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } \
OPTIONAL { ?spouse rdfs:label ?spouseLabel. %s} }"
             qid sp sp qs qe
             (org-chronicle-wikibase--label-filter source "?spouseLabel")))))

(defun org-chronicle-wikibase--events-query (source qid)
  "Return the SPARQL positions-held query for QID against SOURCE."
  (let ((po (org-chronicle-sources--pid source :position))
        (qs (org-chronicle-sources--pid source :qual-start))
        (qe (org-chronicle-sources--pid source :qual-end)))
    (concat
     (org-chronicle-wikibase--prefixes (plist-get source :base-uri))
     (format "SELECT ?pos ?title ?start ?startPrec ?end ?endPrec WHERE { \
wd:%s p:%s ?st. ?st ps:%s ?pos. \
?pos rdfs:label ?title. %s\
OPTIONAL { ?st pqv:%s ?sn. ?sn wikibase:timeValue ?start; wikibase:timePrecision ?startPrec. } \
OPTIONAL { ?st pqv:%s ?en. ?en wikibase:timeValue ?end; wikibase:timePrecision ?endPrec. } }"
             qid po po
             (org-chronicle-wikibase--label-filter source "?title")
             qs qe))))

(defun org-chronicle-wikibase--rank-symbol (uri)
  "Return `preferred', `normal', or `deprecated' for a wikibase:rank URI, else nil."
  (cond ((not (stringp uri)) nil)
        ((string-suffix-p "PreferredRank" uri) 'preferred)
        ((string-suffix-p "NormalRank" uri) 'normal)
        ((string-suffix-p "DeprecatedRank" uri) 'deprecated)
        (t nil)))

(defun org-chronicle-wikibase--dates->candidates (rows)
  "Map dates-query ROWS to neutral date-candidate plists."
  (mapcar
   (lambda (row)
     (let ((raw (org-chronicle-wikibase--cell row "value"))
           (prec (org-chronicle-wikibase--cell-int row "prec")))
       (list :prop (org-chronicle-wikibase--cell row "prop")
             :date (org-chronicle-wikibase--time->date raw prec)
             :raw raw
             :precision prec
             :rank (org-chronicle-wikibase--rank-symbol
                    (org-chronicle-wikibase--cell row "rank")))))
   rows))

(defun org-chronicle-wikibase--ordinal (n)
  "Return N as an English ordinal string, e.g. 17 -> \"17th\"."
  (let ((suffix (cond ((memq (% n 100) '(11 12 13)) "th")
                      ((= (% n 10) 1) "st")
                      ((= (% n 10) 2) "nd")
                      ((= (% n 10) 3) "rd")
                      (t "th"))))
    (format "%d%s" n suffix)))

(defun org-chronicle-wikibase--coarse-date-label (raw precision)
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
                                 (org-chronicle-wikibase--ordinal
                                  (1+ (/ (1- year) 100)))))
       (t (format "%d" year))))))

(defun org-chronicle-wikibase--candidate-label (cand)
  "Return a display string for date-candidate CAND."
  (if (plist-get cand :date)
      (org-chronicle--date-format (plist-get cand :date))
    (org-chronicle-wikibase--coarse-date-label
     (plist-get cand :raw) (plist-get cand :precision))))

(defun org-chronicle-wikibase--select-candidate (candidates)
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
                           (let ((lbl (org-chronicle-wikibase--candidate-label c)))
                             (unless (and chosen-label (equal lbl chosen-label)) lbl)))
                         candidates)))))
    (list :date chosen-date :alternates alternates)))

(defun org-chronicle-wikibase--span-query (source qid start-pid end-pid)
  "Return the SPARQL span query for QID's START-PID and END-PID against SOURCE.
One row per statement, tagged ?prop \"start\"/\"end\", with value, precision,
and rank."
  (concat
   (org-chronicle-wikibase--prefixes (plist-get source :base-uri))
   (format "SELECT ?prop ?value ?prec ?rank WHERE { \
{ wd:%s p:%s ?st. ?st psv:%s ?n. ?n wikibase:timeValue ?value; \
wikibase:timePrecision ?prec. ?st wikibase:rank ?rank. BIND(\"start\" AS ?prop) } \
UNION \
{ wd:%s p:%s ?st. ?st psv:%s ?n. ?n wikibase:timeValue ?value; \
wikibase:timePrecision ?prec. ?st wikibase:rank ?rank. BIND(\"end\" AS ?prop) } }"
           qid start-pid start-pid qid end-pid end-pid)))

(defun org-chronicle-wikibase--prefixes (base-uri)
  "Return a SPARQL PREFIX preamble binding Wikibase prefixes to BASE-URI.
Instance-specific prefixes (wd:, wdt:, p:, ps:, psv:, pq:, pqv:) derive from
BASE-URI; wikibase:/rdfs:/skos:/bd: are instance-independent."
  (concat
   (format "PREFIX wd: <%s/entity/> " base-uri)
   (format "PREFIX wdt: <%s/prop/direct/> " base-uri)
   (format "PREFIX p: <%s/prop/> " base-uri)
   (format "PREFIX ps: <%s/prop/statement/> " base-uri)
   (format "PREFIX psv: <%s/prop/statement/value/> " base-uri)
   (format "PREFIX pq: <%s/prop/qualifier/> " base-uri)
   (format "PREFIX pqv: <%s/prop/qualifier/value/> " base-uri)
   "PREFIX wikibase: <http://wikiba.se/ontology#> "
   "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
   "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> "
   "PREFIX bd: <http://www.bigdata.com/rdf#> "))


(defun org-chronicle-wikibase--span-select (rows)
  "Select start and end dates from span-query ROWS (parsed bindings).
Return (:start DATE :start-alternates LIST :end DATE :end-alternates LIST)."
  (let* ((cands (org-chronicle-wikibase--dates->candidates rows))
         (start (org-chronicle-wikibase--select-candidate
                 (cl-remove-if-not (lambda (c) (equal (plist-get c :prop) "start")) cands)))
         (end (org-chronicle-wikibase--select-candidate
               (cl-remove-if-not (lambda (c) (equal (plist-get c :prop) "end")) cands))))
    (list :start (plist-get start :date) :start-alternates (plist-get start :alternates)
          :end (plist-get end :date) :end-alternates (plist-get end :alternates))))

(defun org-chronicle-wikibase--create-entity (name kind)
  "Create a minimal KIND entity NAME in the kind's file; return a marker."
  (with-current-buffer (find-file-noselect (org-chronicle-sources--kind-file kind))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (org-chronicle--entity-string :name name :kind kind))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (save-buffer)
    (point-marker)))

(defun org-chronicle-wikibase--fetch-record (source qid kind)
  "Fetch QID from SOURCE as a KIND record (person, place, or group)."
  (let* ((pids (org-chronicle-sources--span-pids source kind))
         (vitals (org-chronicle-wikibase--sparql-request
                  source (org-chronicle-wikibase--vitals-query source qid)))
         (span (org-chronicle-wikibase--sparql-request
                source (org-chronicle-wikibase--span-query
                        source qid (car pids) (cdr pids)))))
    (if (eq kind 'person)
        (org-chronicle-wikibase--rows->record
         source qid vitals span
         (org-chronicle-wikibase--sparql-request
          source (org-chronicle-wikibase--spouses-query source qid))
         (org-chronicle-wikibase--sparql-request
          source (org-chronicle-wikibase--events-query source qid)))
      (let* ((v (car vitals))
             (alias-str (and v (org-chronicle-wikibase--cell v "aliases")))
             (sp (org-chronicle-wikibase--span-select span)))
        (list :source source :qid qid :kind kind
              :label (and v (org-chronicle-wikibase--cell v "label"))
              :aliases (and alias-str (not (string-empty-p alias-str))
                            (split-string alias-str
                                          org-chronicle-wikibase--alias-separator t))
              :start (plist-get sp :start)
              :start-alternates (plist-get sp :start-alternates)
              :end (plist-get sp :end)
              :end-alternates (plist-get sp :end-alternates))))))

(defun org-chronicle-wikibase--fetch-person (qid)
  "Fetch QID from Wikidata as a person record."
  (org-chronicle-wikibase--fetch-record
   (org-chronicle-sources--get 'wikidata) qid 'person))

(defun org-chronicle-wikibase--rows->record (source qid vitals dates spouses events)
  "Assemble a person record for QID from parsed binding lists.
VITALS is the single vitals row list; DATES, SPOUSES, EVENTS are row lists.
SOURCE is the source plist the record is tagged with.  Returns a plist;
unrepresentable dates are dropped and competing date statements are resolved
by rank then precision (see `org-chronicle-wikibase--select-candidate')."
  (let* ((v (car vitals))
         (alias-str (and v (org-chronicle-wikibase--cell v "aliases")))
         (span (org-chronicle-wikibase--span-select dates)))
    (list
     :source source
     :qid qid
     :kind 'person
     :label (and v (org-chronicle-wikibase--cell v "label"))
     :born (plist-get span :start)
     :born-alternates (plist-get span :start-alternates)
     :died (plist-get span :end)
     :died-alternates (plist-get span :end-alternates)
     :birthplace (and v (org-chronicle-wikibase--cell v "birthPlaceLabel"))
     :deathplace (and v (org-chronicle-wikibase--cell v "deathPlaceLabel"))
     :father (and v (org-chronicle-wikibase--cell v "fatherLabel"))
     :mother (and v (org-chronicle-wikibase--cell v "motherLabel"))
     :aliases (and alias-str (not (string-empty-p alias-str))
                   (split-string alias-str org-chronicle-wikibase--alias-separator t))
     :spouses (mapcar (lambda (row)
                        (list :name (org-chronicle-wikibase--cell row "spouseLabel")
                              :qid (org-chronicle-wikibase--parse-qid
                                    (org-chronicle-wikibase--cell row "spouse"))
                              :date (org-chronicle-wikibase--row-date row "start" "startPrec")
                              :end (org-chronicle-wikibase--row-date row "end" "endPrec")))
                      spouses)
     :events (mapcar (lambda (row)
                       (list :kind "position"
                             :title (org-chronicle-wikibase--cell row "title")
                             :qid (org-chronicle-wikibase--parse-qid
                                   (org-chronicle-wikibase--cell row "pos"))
                             :date (org-chronicle-wikibase--row-date row "start" "startPrec")
                             :date-end (org-chronicle-wikibase--row-date row "end" "endPrec")
                             :location nil))
                     events))))

(defun org-chronicle-wikibase--url (source qid)
  "Return the provenance URL for QID using SOURCE's item-url-format."
  (format (plist-get source :item-url-format) qid))

(defun org-chronicle-wikibase--entity-change (group property value url &optional alternates)
  "Build an entity change plist for PROPERTY=VALUE in GROUP, sourced to URL.
ALTERNATES, when non-nil, is a list of display strings attached as :alternates.
Return nil when VALUE is nil or empty."
  (and value (not (string-empty-p value))
       (append (list :target 'entity :group group :property property :value value
                     :provenance url :default t)
               (and alternates (list :alternates alternates)))))

(defun org-chronicle-wikibase--entity-record->changes (rec)
  "Build entity change plists for place/group record REC.
Return span, aliases, and WIKIDATA property changes."
  (let* ((source (plist-get rec :source))
         (qid (plist-get rec :qid))
         (kind (plist-get rec :kind))
         (url (org-chronicle-wikibase--url source qid))
         (props (org-chronicle-sources--kind-span-props kind))
         (start (plist-get rec :start))
         (end (plist-get rec :end))
         (aliases (plist-get rec :aliases)))
    (delq nil
          (list
           (org-chronicle-wikibase--entity-change
            'vitals (car props)
            (and start (org-chronicle--ts (org-chronicle--date-format start)))
            url (plist-get rec :start-alternates))
           (org-chronicle-wikibase--entity-change
            'vitals (cdr props)
            (and end (org-chronicle--ts (org-chronicle--date-format end)))
            url (plist-get rec :end-alternates))
           (org-chronicle-wikibase--entity-change
            'vitals (plist-get source :key-property) qid url)
           (org-chronicle-wikibase--entity-change
            'relations "ALIASES" (and aliases (org-chronicle--join aliases)) url)))))

(defun org-chronicle-wikibase--record->changes (rec name)
  "Map person record REC (for person NAME) to a list of change plists.
Each change targets either an entity property or an event entry.
See the data contract in the package commentary for field names."
  (if (memq (plist-get rec :kind) '(place group))
      (org-chronicle-wikibase--entity-record->changes rec)
    (let* ((source (plist-get rec :source))
           (qid (plist-get rec :qid))
           (url (org-chronicle-wikibase--url source qid))
           (born (plist-get rec :born))
           (died (plist-get rec :died))
           (parents (delq nil (list (plist-get rec :father) (plist-get rec :mother))))
           (spouses (plist-get rec :spouses))
           (aliases (let* ((label (plist-get rec :label))
                           (extra (and label (not (equal label name)) (list label))))
                      (append extra (plist-get rec :aliases))))
           changes)
      (dolist (c (list
                  (org-chronicle-wikibase--entity-change
                   'vitals "BORN"
                   (and born (org-chronicle--ts (org-chronicle--date-format born)))
                   url (plist-get rec :born-alternates))
                  (org-chronicle-wikibase--entity-change
                   'vitals "DIED"
                   (and died (org-chronicle--ts (org-chronicle--date-format died)))
                   url (plist-get rec :died-alternates))
                  (org-chronicle-wikibase--entity-change
                   'vitals "BIRTHPLACE" (plist-get rec :birthplace) url)
                  (org-chronicle-wikibase--entity-change
                   'vitals "DEATHPLACE" (plist-get rec :deathplace) url)
                  (org-chronicle-wikibase--entity-change
                   'vitals (plist-get source :key-property) qid url)))
        (when c (push c changes)))
      (dolist (c (list
                  (org-chronicle-wikibase--entity-change
                   'relations "PARENTS"
                   (and parents (org-chronicle--join parents)) url)
                  (org-chronicle-wikibase--entity-change
                   'relations "SPOUSE"
                   (and spouses (org-chronicle--join
                                 (mapcar (lambda (s) (plist-get s :name)) spouses)))
                   url)
                  (org-chronicle-wikibase--entity-change
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

(defun org-chronicle-wikibase--candidate-line (cand)
  "Format candidate plist CAND as a completion line."
  (format "%s — %s (%s)"
          (plist-get cand :label)
          (plist-get cand :description)
          (plist-get cand :qid)))

(defun org-chronicle-wikibase--resolve (source seed)
  "Resolve SEED (a name) to a confirmed item id from SOURCE.
Read a term defaulting to SEED; a pasted QID/URL short-circuits search,
otherwise present search candidates for selection.  Return the id string."
  (let* ((term (read-string "Wikidata search (or paste QID/URL): " seed))
         (pasted (org-chronicle-wikibase--parse-qid term)))
    (or pasted
        (let* ((cands (org-chronicle-wikibase--search-request source term))
               (lines (mapcar #'org-chronicle-wikibase--candidate-line cands))
               (table (cl-mapcar #'cons lines cands)))
          (unless cands (user-error "No Wikidata matches for %S" term))
          (let* ((choice (completing-read "Select item: " lines nil t))
                 (cand (cdr (assoc choice table))))
            (plist-get cand :qid))))))

;; Backward-compatibility aliases for the pre-sources public commands.
;; The commands now live in `org-chronicle-sources'.
(define-obsolete-function-alias 'org-chronicle-wikibase-import
  'org-chronicle-import "org-chronicle 0.5")

(define-obsolete-function-alias 'org-chronicle-wikibase-reconcile
  'org-chronicle-reconcile "org-chronicle 0.5")

;; Backward-compatibility: the module was named org-chronicle-wikidata before
;; the multi-source split.  Keep the old feature loadable.
(provide 'org-chronicle-wikidata)

(provide 'org-chronicle-wikibase)
;;; org-chronicle-wikibase.el ends here
