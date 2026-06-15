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









(provide 'org-chronicle-wikidata)
;;; org-chronicle-wikidata.el ends here
