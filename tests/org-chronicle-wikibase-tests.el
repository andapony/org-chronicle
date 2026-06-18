;;; org-chronicle-wikibase-tests.el --- Tests for org-chronicle-wikibase -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for the Wikidata adapter layer.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle-wikibase)
(require 'cl-lib)

(require 'org-chronicle-sources)


(ert-deftest org-chronicle-wikibase-test-loads ()
  "The integration loads and defines its group."
  (should (featurep 'org-chronicle-wikibase)))

(ert-deftest org-chronicle-wikibase-test-reuse-marker ()
  "Reuse finds an entity by QID, then by unclaimed name; a same-name entity
claimed by a different QID is not reused."
  (let* ((dir (make-temp-file "ocw-reuse" t))
         (org-chronicle-root dir)
         (org-chronicle-people-file nil)
         (org-chronicle-exclude nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "people.org" dir)
            (insert "* John Doe\n:PROPERTIES:\n:ID:       ent-jd1\n"
                    ":KIND:     person\n:WIKIDATA:  Q1\n:END:\n\n"
                    "* Jane Roe\n:PROPERTIES:\n:ID:       ent-jr\n"
                    ":KIND:     person\n:END:\n"))
          ;; QID match wins regardless of the name passed.
          (let ((m (org-chronicle-wikibase--reuse-marker "Whoever" 'person "Q1" "WIKIDATA")))
            (should m)
            (should (equal (org-with-point-at m (org-get-heading t t t t)) "John Doe")))
          ;; Name match on an unclaimed entity (no WIKIDATA yet).
          (let ((m (org-chronicle-wikibase--reuse-marker "Jane Roe" 'person "Q9" "WIKIDATA")))
            (should m)
            (should (equal (org-with-point-at m (org-get-heading t t t t)) "Jane Roe")))
          ;; Same name, different QID -> distinct individual, not reused.
          (should-not (org-chronicle-wikibase--reuse-marker "John Doe" 'person "Q2" "WIKIDATA"))
          ;; No match at all.
          (should-not (org-chronicle-wikibase--reuse-marker "Nobody" 'person "Q9" "WIKIDATA")))
      (delete-directory dir t))))

(ert-deftest org-chronicle-wikibase-test-wikidata-references ()
  "Wikidata references yield a WIKIPEDIA GoToLinkedPage URL honoring the language."
  (let ((org-chronicle-wikipedia-language "en"))
    (should (equal (org-chronicle-wikibase--wikidata-references nil "Q2")
                   '(("WIKIPEDIA" . "https://www.wikidata.org/wiki/Special:GoToLinkedPage/enwiki/Q2")))))
  (let ((org-chronicle-wikipedia-language "de"))
    (should (equal (cdr (car (org-chronicle-wikibase--wikidata-references nil "Q2")))
                   "https://www.wikidata.org/wiki/Special:GoToLinkedPage/dewiki/Q2"))))

(ert-deftest org-chronicle-wikibase-test-references ()
  "--references uses the source's reference-fn, or nil when the source has none."
  (let ((org-chronicle-wikipedia-language "en"))
    (should (equal (org-chronicle-wikibase--references
                    (org-chronicle-sources--get 'wikidata) "Q2")
                   '(("WIKIPEDIA" . "https://www.wikidata.org/wiki/Special:GoToLinkedPage/enwiki/Q2"))))
    (should-not (org-chronicle-wikibase--references
                 (org-chronicle-sources--get 'factgrid) "FG2"))))

(ert-deftest org-chronicle-wikibase-test-events-query ()
  "The events query targets the place, the date range, and fetches labels."
  (let ((q (org-chronicle-wikibase--place-events-query
            (org-chronicle-sources--get 'wikidata) "Q62" 1845 1859)))
    (should (string-match-p "wd:Q62" q))
    (should (string-match-p "VALUES" q))
    (should (string-match-p "wdt:P276" q))
    (should (string-match-p "wdt:P131" q))
    (should (string-match-p "P585" q))
    (should (string-match-p "UNION" q))
    (should (string-match-p "P580" q))
    (should (string-match-p "P582" q))
    (should (string-match-p "BOUND" q))
    (should (string-match-p ">= 1845" q))
    (should (string-match-p "<= 1859" q))
    (should (string-match-p "eventLabel" q))))

(ert-deftest org-chronicle-wikibase-test-place-events-query-limit ()
  "A positive limit appends a LIMIT clause; nil omits it."
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (should (string-match-p "LIMIT 25"
                            (org-chronicle-wikibase--place-events-query wd "Q62" 1845 1859 25)))
    (should-not (string-match-p "LIMIT"
                                (org-chronicle-wikibase--place-events-query wd "Q62" 1845 1859 nil)))))


(ert-deftest org-chronicle-wikibase-test-events-changes ()
  "Each dated event row becomes an event-creation change keyed on the event id;
coarse-precision rows are dropped."
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (rows (list '((event (value . "http://www.wikidata.org/entity/Q123"))
                       (eventLabel (value . "Founding of Foo"))
                       (date (value . "1846-07-09T00:00:00Z"))
                       (prec (value . "11")))
                     '((event (value . "http://www.wikidata.org/entity/Q124"))
                       (eventLabel (value . "Vague Decade Thing"))
                       (date (value . "1850-01-01T00:00:00Z"))
                       (prec (value . "8")))))
         (changes (org-chronicle-wikibase--place-events->changes wd rows "San Francisco")))
    (should (= (length changes) 1))
    (let* ((c (car changes)) (ev (plist-get c :event)))
      (should (eq (plist-get c :target) 'event))
      (should (equal (plist-get c :key) "event:wd:Q123"))
      (should (equal (plist-get ev :title) "Founding of Foo"))
      (should (equal (plist-get ev :location) "San Francisco"))
      (should (string-match-p "1846-07-09" (plist-get ev :date))))))

(ert-deftest org-chronicle-wikibase-test-place-events-changes-spans ()
  "Span rows yield :date + :date-end; start-only yields :date only;
a coarse end is omitted while the event is kept."
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (rows (list
                '((event (value . "http://www.wikidata.org/entity/Q1"))
                  (eventLabel (value . "California Gold Rush"))
                  (date (value . "1848-01-24T00:00:00Z")) (prec (value . "11"))
                  (end (value . "1855-01-01T00:00:00Z")) (endprec (value . "9")))
                '((event (value . "http://www.wikidata.org/entity/Q2"))
                  (eventLabel (value . "Long War"))
                  (date (value . "1846-01-01T00:00:00Z")) (prec (value . "9")))
                '((event (value . "http://www.wikidata.org/entity/Q3"))
                  (eventLabel (value . "Fuzzy End"))
                  (date (value . "1850-01-01T00:00:00Z")) (prec (value . "9"))
                  (end (value . "1850-01-01T00:00:00Z")) (endprec (value . "7")))))
         (changes (org-chronicle-wikibase--place-events->changes wd rows "California")))
    (should (= (length changes) 3))
    (let ((gold (plist-get (nth 0 changes) :event)))
      (should (string-match-p "1848-01-24" (plist-get gold :date)))
      (should (plist-get gold :date-end))
      (should (string-match-p "1855" (plist-get gold :date-end))))
    (should-not (plist-get (plist-get (nth 1 changes) :event) :date-end))
    (let ((fuzzy (plist-get (nth 2 changes) :event)))
      (should (string-match-p "1850" (plist-get fuzzy :date)))
      (should-not (plist-get fuzzy :date-end)))))

(ert-deftest org-chronicle-wikibase-test-place-events-changes-dedup ()
  "An event appearing as both a point and a span yields one change, preferring
the span (the entry carrying an end date)."
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (rows (list
                '((event (value . "http://www.wikidata.org/entity/Q1"))
                  (eventLabel (value . "Gold Rush"))
                  (date (value . "1848-01-01T00:00:00Z")) (prec (value . "9"))
                  (end (value . "1854-01-01T00:00:00Z")) (endprec (value . "9")))
                '((event (value . "http://www.wikidata.org/entity/Q1"))
                  (eventLabel (value . "Gold Rush"))
                  (date (value . "1855-01-01T00:00:00Z")) (prec (value . "9")))))
         (changes (org-chronicle-wikibase--place-events->changes wd rows "California")))
    (should (= (length changes) 1))
    (let ((ev (plist-get (car changes) :event)))
      (should (string-match-p "1848" (plist-get ev :date)))
      (should (string-match-p "1854" (plist-get ev :date-end))))))

(ert-deftest org-chronicle-wikibase-test-place-events-changes-references ()
  "A Wikidata event change carries the source's WIKIPEDIA reference link."
  (let* ((org-chronicle-wikipedia-language "en")
         (wd (org-chronicle-sources--get 'wikidata))
         (rows (list '((event (value . "http://www.wikidata.org/entity/Q5"))
                       (eventLabel (value . "Some Event"))
                       (date (value . "1850-01-01T00:00:00Z")) (prec (value . "9")))))
         (change (car (org-chronicle-wikibase--place-events->changes wd rows "Place"))))
    (should (equal (assoc "WIKIPEDIA" (plist-get change :references))
                   '("WIKIPEDIA" . "https://www.wikidata.org/wiki/Special:GoToLinkedPage/enwiki/Q5")))))

(ert-deftest org-chronicle-wikibase-test-people-query ()
  "A transitive relation matches the anchor within a place, with occupation and lifespan."
  (let ((q (org-chronicle-wikibase--people-query
            (org-chronicle-sources--get 'wikidata)
            '("P551" "P937") t "Q99" "Q1930187" 1845 1859 100)))
    (should (string-match-p "wdt:P551" q))
    (should (string-match-p "wdt:P937" q))
    (should (string-match-p "wdt:P131\\*" q))
    (should (string-match-p "wd:Q99" q))
    (should (string-match-p "wd:Q1930187" q))
    (should (string-match-p "wdt:P106" q))
    (should (string-match-p "<= 1859" q))
    (should (string-match-p ">= 1845" q))
    (should (string-match-p "LIMIT 100" q))))

(ert-deftest org-chronicle-wikibase-test-people-query-blank-occupation ()
  "A nil occupation omits the occupation clause but keeps the anchor."
  (let ((q (org-chronicle-wikibase--people-query
            (org-chronicle-sources--get 'wikidata) '("P551" "P937") t "Q99" nil 1845 1859)))
    (should (string-match-p "wd:Q99" q))
    (should-not (string-match-p "wdt:P106" q))))

(ert-deftest org-chronicle-wikibase-test-people-query-direct ()
  "A non-transitive relation matches the anchor directly, without a P131* hop."
  (let ((q (org-chronicle-wikibase--people-query
            (org-chronicle-sources--get 'wikidata) '("P463" "P39") nil "Q123" nil 1845 1859)))
    (should (string-match-p "wdt:P463" q))
    (should (string-match-p "wdt:P39" q))
    (should (string-match-p "wd:Q123" q))
    (should-not (string-match-p "P131" q))))

(ert-deftest org-chronicle-wikibase-test-foundings-query ()
  "The foundings query is located-within + inception-in-window + an org-class classify."
  (let ((q (org-chronicle-wikibase--foundings-query
            (org-chronicle-sources--get 'wikidata) "Q62" 1845 1860 50)))
    (should (string-match-p "wd:Q62" q))
    (should (string-match-p "wdt:P131\\*" q))
    (should (string-match-p "P571" q))
    (should (string-match-p "P576" q))
    (should (string-match-p "Q43229" q))
    (should (string-match-p "EXISTS" q))
    (should (string-match-p ">= 1845" q))
    (should (string-match-p "<= 1860" q))
    (should (string-match-p "LIMIT 50" q))))

(ert-deftest org-chronicle-wikibase-test-foundings-changes ()
  "Each founding row becomes an entity-seed change with kind and span; dup rows
collapse and a coarse inception is dropped."
  (let* ((org-chronicle-wikipedia-language "en")
         (wd (org-chronicle-sources--get 'wikidata))
         (rows (list
                '((e (value . "http://www.wikidata.org/entity/Q100"))
                  (eLabel (value . "San Francisco Mint")) (kind (value . "group"))
                  (built (value . "1854-01-01T00:00:00Z")) (builtprec (value . "9")))
                '((e (value . "http://www.wikidata.org/entity/Q101"))
                  (eLabel (value . "Fort Point")) (kind (value . "place"))
                  (built (value . "1848-01-01T00:00:00Z")) (builtprec (value . "9"))
                  (razed (value . "1900-01-01T00:00:00Z")) (razedprec (value . "9")))
                '((e (value . "http://www.wikidata.org/entity/Q102"))
                  (eLabel (value . "Coarse")) (kind (value . "place"))
                  (built (value . "1850-01-01T00:00:00Z")) (builtprec (value . "7")))))
         (changes (org-chronicle-wikibase--foundings->changes wd rows "San Francisco")))
    (should (= (length changes) 2))
    (let ((mint (car changes)))
      (should (eq (plist-get mint :target) 'entity-seed))
      (should (eq (plist-get mint :kind) 'group))
      (should (equal (plist-get mint :qid) "Q100"))
      (should (string-match-p "1854" (plist-get mint :built)))
      (should-not (plist-get mint :razed)))
    (let ((fort (nth 1 changes)))
      (should (eq (plist-get fort :kind) 'place))
      (should (string-match-p "1900" (plist-get fort :razed))))))




(ert-deftest org-chronicle-wikibase-test-people-changes ()
  "Each person row becomes one 'person seed change keyed on QID; duplicate rows
collapse; a coarse birth yields no :born but keeps the person."
  (let* ((org-chronicle-wikipedia-language "en")
         (wd (org-chronicle-sources--get 'wikidata))
         (rows (list
                '((person (value . "http://www.wikidata.org/entity/Q10"))
                  (personLabel (value . "Jane Doe"))
                  (born (value . "1820-03-02T00:00:00Z")) (bornprec (value . "11"))
                  (died (value . "1880-01-01T00:00:00Z")) (diedprec (value . "9")))
                '((person (value . "http://www.wikidata.org/entity/Q10"))
                  (personLabel (value . "Jane Doe"))
                  (born (value . "1820-03-02T00:00:00Z")) (bornprec (value . "11"))
                  (died (value . "1880-01-01T00:00:00Z")) (diedprec (value . "9")))
                '((person (value . "http://www.wikidata.org/entity/Q11"))
                  (personLabel (value . "Coarse Born"))
                  (born (value . "1840-01-01T00:00:00Z")) (bornprec (value . "7")))))
         (changes (org-chronicle-wikibase--people->changes wd rows)))
    (should (= (length changes) 2))
    (let ((jane (car changes)))
      (should (eq (plist-get jane :target) 'person))
      (should (equal (plist-get jane :qid) "Q10"))
      (should (equal (plist-get jane :name) "Jane Doe"))
      (should (string-match-p "1820-03-02" (plist-get jane :born)))
      (should (string-match-p "1880" (plist-get jane :died)))
      (should (assoc "WIKIPEDIA" (plist-get jane :references))))
    (let ((coarse (nth 1 changes)))
      (should (equal (plist-get coarse :qid) "Q11"))
      (should-not (plist-get coarse :born)))))









(ert-deftest org-chronicle-wikibase-test-record-changes-reference ()
  "Record->changes adds a WIKIPEDIA vitals change for a Wikidata person, none for FactGrid."
  (let ((org-chronicle-wikipedia-language "en"))
    (let* ((wd (org-chronicle-sources--get 'wikidata))
           (rec (list :source wd :qid "Q2" :kind 'person
                      :born (org-chronicle--date-parse "1819")))
           (changes (org-chronicle-wikibase--record->changes rec "Sam Brannan")))
      (should (cl-find-if (lambda (c) (equal (plist-get c :property) "WIKIPEDIA")) changes)))
    (let* ((fg (org-chronicle-sources--get 'factgrid))
           (rec (list :source fg :qid "FG2" :kind 'person
                      :born (org-chronicle--date-parse "1819")))
           (changes (org-chronicle-wikibase--record->changes rec "X")))
      (should-not (cl-find-if (lambda (c) (equal (plist-get c :property) "WIKIPEDIA")) changes)))))

(ert-deftest org-chronicle-wikibase-test-compose-place ()
  "Compose-place appends admin only for flagged countries, then the country."
  (let ((org-chronicle-place-admin-countries '("Q30")))
    (should (equal (org-chronicle-wikibase--compose-place "Saco" "Maine" "United States" "Q30")
                   "Saco, Maine, United States"))
    ;; Unflagged country: admin ignored even when present.
    (should (equal (org-chronicle-wikibase--compose-place "Lyon" "Auvergne-Rhone-Alpes" "France" "Q142")
                   "Lyon, France"))
    ;; Flagged but no admin found.
    (should (equal (org-chronicle-wikibase--compose-place "Saco" nil "United States" "Q30")
                   "Saco, United States"))
    ;; No country: bare label.
    (should (equal (org-chronicle-wikibase--compose-place "Saco" nil nil nil) "Saco"))
    ;; No label: nil.
    (should-not (org-chronicle-wikibase--compose-place nil "Maine" "United States" "Q30"))))

(ert-deftest org-chronicle-wikibase-test-rows-record-place ()
  "Rows->record composes birthplace/deathplace from the new query columns."
  (let ((org-chronicle-place-admin-countries '("Q30"))
        (row '((birthPlaceLabel (value . "Saco"))
               (birthPlaceAdminLabel (value . "Maine"))
               (birthPlaceCountryLabel (value . "United States"))
               (birthPlaceCountry (value . "http://www.wikidata.org/entity/Q30"))
               (deathPlaceLabel (value . "New York City"))
               (deathPlaceCountryLabel (value . "United States"))
               (deathPlaceCountry (value . "http://www.wikidata.org/entity/Q30")))))
    (let ((rec (org-chronicle-wikibase--rows->record '(:property-map nil) "Q1" (list row) nil nil nil)))
      (should (equal (plist-get rec :birthplace) "Saco, Maine, United States"))
      (should (equal (plist-get rec :deathplace) "New York City, United States")))))

(ert-deftest org-chronicle-wikibase-test-vitals-query-place-detail ()
  "The vitals query carries place-detail clauses when the source has the PIDs."
  (let ((q (org-chronicle-wikibase--vitals-query (org-chronicle-sources--get 'wikidata) "Q1")))
    (should (string-match-p "wdt:P17" q))
    (should (string-match-p "wdt:P131\\*" q))
    (should (string-match-p "Q10864048" q))
    (should (string-match-p "birthPlaceAdminLabel" q))
    (should (string-match-p "deathPlaceCountryLabel" q)))
  ;; FactGrid lacks the place-detail PIDs, so no admin clause.
  (let ((q (org-chronicle-wikibase--vitals-query (org-chronicle-sources--get 'factgrid) "FG1")))
    (should-not (string-match-p "Q10864048" q))))







(ert-deftest org-chronicle-wikibase-test-resolve-or-create-entity ()
  "Resolve reuses a QID-matched entity without duplicating it, and creates a
new entity for a different person."
  (let* ((dir (make-temp-file "ocw-resolve" t))
         (org-chronicle-root dir)
         (org-chronicle-people-file nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "people.org" dir)
            (insert "* John Doe\n:PROPERTIES:\n:ID: ent-jd1\n:KIND: person\n:WIKIDATA: Q1\n:END:\n"))
          ;; Reuse the existing John Doe by QID, do not create a second.
          (let ((m (org-chronicle-wikibase--resolve-or-create-entity "John Doe" 'person "Q1" "WIKIDATA")))
            (should (equal (org-with-point-at m (org-get-heading t t t t)) "John Doe")))
          (with-current-buffer (find-file-noselect (expand-file-name "people.org" dir))
            (revert-buffer t t)
            (goto-char (point-min))
            (let ((n 0)) (while (re-search-forward "^\\* John Doe$" nil t) (setq n (1+ n)))
                 (should (= n 1))))
          ;; A different person is created.
          (let ((m (org-chronicle-wikibase--resolve-or-create-entity "Sam Brannan" 'person "Q2" "WIKIDATA")))
            (should (equal (org-with-point-at m (org-get-heading t t t t)) "Sam Brannan"))))
      (delete-directory dir t))))



(ert-deftest org-chronicle-wikibase-test-parse-qid ()
  "Test QID extraction from various string formats."
  (should (equal (org-chronicle-wikibase--parse-qid "Q42") "Q42"))
  (should (equal (org-chronicle-wikibase--parse-qid "  q42 ") "Q42"))
  (should (equal (org-chronicle-wikibase--parse-qid
                  "https://www.wikidata.org/wiki/Q7259") "Q7259"))
  (should (equal (org-chronicle-wikibase--parse-qid
                  "http://www.wikidata.org/entity/Q7259") "Q7259"))
  (should (null (org-chronicle-wikibase--parse-qid "not a qid")))
  (should (null (org-chronicle-wikibase--parse-qid nil)))
  (should (null (org-chronicle-wikibase--parse-qid ""))))

(ert-deftest org-chronicle-wikibase-test-time->date ()
  "Test conversion from Wikidata time strings to date plists."
  (let ((d (org-chronicle-wikibase--time->date "+1815-12-10T00:00:00Z" 11)))
    (should (equal (plist-get d :year) 1815))
    (should (equal (plist-get d :month) 12))
    (should (equal (plist-get d :day) 10))
    (should (eq (plist-get d :precision) 'day)))
  ;; The live SPARQL endpoint returns ISO 8601 without a leading '+'.
  (let ((d (org-chronicle-wikibase--time->date "1815-12-10T00:00:00Z" 11)))
    (should (equal (plist-get d :year) 1815))
    (should (equal (plist-get d :month) 12))
    (should (equal (plist-get d :day) 10))
    (should (eq (plist-get d :precision) 'day)))
  (should (eq (plist-get (org-chronicle-wikibase--time->date
                          "+1815-12-01T00:00:00Z" 10) :precision)
              'month))
  (should (eq (plist-get (org-chronicle-wikibase--time->date
                          "+1815-01-01T00:00:00Z" 9) :precision)
              'year))
  (should (null (org-chronicle-wikibase--time->date "+1810-01-01T00:00:00Z" 8)))
  (should (null (org-chronicle-wikibase--time->date "-0044-03-15T00:00:00Z" 11)))
  (should (null (org-chronicle-wikibase--time->date "+0500-01-01T00:00:00Z" 9)))
  (should (null (org-chronicle-wikibase--time->date nil 11))))

(ert-deftest org-chronicle-wikibase-test-bindings ()
  "Test SPARQL JSON parsing and binding accessors."
  (let* ((json "{\"results\":{\"bindings\":[\
{\"a\":{\"type\":\"literal\",\"value\":\"x\"},\"n\":{\"value\":\"11\"}}]}}")
         (rows (org-chronicle-wikibase--bindings json)))
    (should (= (length rows) 1))
    (should (equal (org-chronicle-wikibase--cell (car rows) "a") "x"))
    (should (null (org-chronicle-wikibase--cell (car rows) "missing")))
    (should (= (org-chronicle-wikibase--cell-int (car rows) "n") 11))
    (should (null (org-chronicle-wikibase--cell-int (car rows) "a")))))

(defvar org-chronicle-wikibase-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the wikidata test file, captured at load time.")

(ert-deftest org-chronicle-wikibase-test-search-request ()
  (cl-letf (((symbol-function 'org-chronicle-wikibase--http-get)
             (lambda (&rest _)
               (org-chronicle-wikibase-test--fixture "search-lovelace.json"))))
    (let ((cands (org-chronicle-wikibase--search-request
                  (org-chronicle-sources--get 'wikidata) "Ada Lovelace")))
      (should (equal (plist-get (car cands) :qid) "Q7259"))
      (should (equal (plist-get (car cands) :label) "Ada Lovelace"))
      (should (string-match-p "mathematician"
                              (plist-get (car cands) :description))))))

(ert-deftest org-chronicle-wikibase-test-search-terms ()
  "Search terms are the full term then its comma-trimmed prefixes, longest first."
  (should (equal (org-chronicle-wikibase--search-terms "Saco, Maine, United States")
                 '("Saco, Maine, United States" "Saco, Maine" "Saco")))
  (should (equal (org-chronicle-wikibase--search-terms "Vicksburg, Mississippi")
                 '("Vicksburg, Mississippi" "Vicksburg")))
  (should (equal (org-chronicle-wikibase--search-terms "Saco") '("Saco"))))

(ert-deftest org-chronicle-wikibase-test-search-fallback ()
  "Search falls back to trimmed prefixes until one yields candidates."
  (cl-letf (((symbol-function 'org-chronicle-wikibase--search-request)
             (lambda (_source tm)
               (when (equal tm "Saco") (list (list :qid "Q1001891" :label "Saco"))))))
    (should (equal (org-chronicle-wikibase--search nil "Saco, Maine, United States")
                   (list (list :qid "Q1001891" :label "Saco"))))
    (should-not (org-chronicle-wikibase--search nil "Nowhere, Noplace"))))



(ert-deftest org-chronicle-wikibase-test-http-error ()
  (cl-letf (((symbol-function 'org-chronicle-wikibase--http-get)
             (lambda (&rest _) (signal 'org-chronicle-wikibase-rate-limited nil))))
    (should-error (org-chronicle-wikibase--search-request
                   (org-chronicle-sources--get 'wikidata) "x")
                  :type 'org-chronicle-wikibase-rate-limited)))

(ert-deftest org-chronicle-wikibase-test-queries-mention-qid ()
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (should (string-match-p "wd:Q7259"
                            (org-chronicle-wikibase--vitals-query wd "Q7259")))
    (should (string-match-p "P26"
                            (org-chronicle-wikibase--spouses-query wd "Q7259")))
    (should (string-match-p "P39"
                            (org-chronicle-wikibase--events-query wd "Q7259")))))

(ert-deftest org-chronicle-wikibase-test-fetch-person ()
  (cl-letf (((symbol-function 'org-chronicle-wikibase--sparql-request)
             (lambda (_source q)
               (org-chronicle-wikibase--bindings
                (cond ((string-match-p "P569" q)
                       (org-chronicle-wikibase-test--fixture "lovelace-dates.json"))
                      ((string-match-p "P26" q)
                       (org-chronicle-wikibase-test--fixture "lovelace-spouses.json"))
                      ((string-match-p "P39" q)
                       (org-chronicle-wikibase-test--fixture "lovelace-events.json"))
                      (t (org-chronicle-wikibase-test--fixture "lovelace-vitals.json")))))))
    (let ((rec (org-chronicle-wikibase--fetch-person "Q7259")))
      (should (equal (plist-get rec :qid) "Q7259"))
      (should (equal (plist-get rec :birthplace) "London"))
      (should (= (length (plist-get rec :spouses)) 1)))))

(defun org-chronicle-wikibase-test--fixture (name)
  "Return the contents of fixture NAME under tests/fixtures/."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "fixtures/%s" name)
                       org-chronicle-wikibase-test--directory))
    (buffer-string)))

(ert-deftest org-chronicle-wikibase-test-record ()
  "Test assembling a person record from parsed Wikidata binding rows."
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (rec (org-chronicle-wikibase--rows->record
               wd "Q7259"
               (org-chronicle-wikibase--bindings
                (org-chronicle-wikibase-test--fixture "lovelace-vitals.json"))
               (org-chronicle-wikibase--bindings
                (org-chronicle-wikibase-test--fixture "lovelace-dates.json"))
               (org-chronicle-wikibase--bindings
                (org-chronicle-wikibase-test--fixture "lovelace-spouses.json"))
               (org-chronicle-wikibase--bindings
                (org-chronicle-wikibase-test--fixture "lovelace-events.json")))))
    (should (equal (plist-get rec :qid) "Q7259"))
    (should (equal (plist-get (plist-get rec :born) :year) 1815))
    (should (equal (plist-get (plist-get rec :died) :year) 1852))
    (should (equal (plist-get rec :birthplace) "London"))
    (should (equal (plist-get rec :deathplace) "Marylebone"))
    (should (equal (plist-get rec :father) "Lord Byron"))
    (should (equal (plist-get rec :mother) "Anne Isabella Byron"))
    (should (equal (plist-get rec :aliases) '("Augusta Ada King" "Ada King")))
    (let ((sp (car (plist-get rec :spouses))))
      (should (string-prefix-p "William King-Noel" (plist-get sp :name)))
      (should (equal (plist-get (plist-get sp :date) :year) 1835)))
    (let ((ev (car (plist-get rec :events))))
      (should (equal (plist-get ev :title) "Countess of Lovelace"))
      (should (equal (plist-get ev :kind) "position"))
      (should (equal (plist-get (plist-get ev :date) :year) 1838)))))

(ert-deftest org-chronicle-wikibase-test-record->changes ()
  "Test that a person record produces the expected set of change plists."
  (let* ((rec (list :source (org-chronicle-sources--get 'wikidata)
                    :qid "Q7259"
                    :born (org-chronicle--date-parse "1815-12-10")
                    :died (org-chronicle--date-parse "1852-11-27")
                    :birthplace "London" :deathplace "Marylebone"
                    :father "Lord Byron" :mother "Anne Isabella Byron"
                    :aliases '("Augusta Ada King")
                    :spouses (list (list :name "William King-Noel"
                                         :date (org-chronicle--date-parse "1835-07-08")
                                         :end nil))
                    :events (list (list :kind "position" :title "Countess of Lovelace"
                                        :date (org-chronicle--date-parse "1838")
                                        :date-end nil :location nil))))
         (changes (org-chronicle-wikibase--record->changes rec "Ada Lovelace")))
    (cl-flet ((prop (p) (cl-find-if (lambda (c)
                                      (and (eq (plist-get c :target) 'entity)
                                           (equal (plist-get c :property) p)))
                                    changes)))
      (should (equal (plist-get (prop "BORN") :value) "<1815-12-10>"))
      (should (equal (plist-get (prop "BIRTHPLACE") :value) "London"))
      (should (equal (plist-get (prop "PARENTS") :value) "Lord Byron; Anne Isabella Byron"))
      (should (equal (plist-get (prop "SPOUSE") :value) "William King-Noel"))
      (should (equal (plist-get (prop "ALIASES") :value) "Augusta Ada King"))
      (should (equal (plist-get (prop "WIKIDATA") :value) "Q7259"))
      (should (equal (plist-get (prop "BORN") :provenance)
                     "https://www.wikidata.org/wiki/Q7259"))
      (should (plist-get (prop "BORN") :default)))
    (let ((events (cl-remove-if-not (lambda (c) (eq (plist-get c :target) 'event)) changes)))
      (should (= (length events) 4))
      (let ((curated (cl-find "position" events
                              :key (lambda (c) (plist-get (plist-get c :event) :kind))
                              :test (lambda (_ k) (equal k "position")))))
        (should (null (plist-get curated :default)))))))

(ert-deftest org-chronicle-wikibase-test-candidate-line ()
  "Test formatting a candidate plist as a completion line."
  (should (equal (org-chronicle-wikibase--candidate-line
                  (list :qid "Q7259" :label "Ada Lovelace"
                        :description "English mathematician"))
                 "Ada Lovelace — English mathematician (Q7259)")))

(ert-deftest org-chronicle-wikibase-test-resolve-paste ()
  "Test that pasting a QID short-circuits search."
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Q42"))
              ((symbol-function 'org-chronicle-wikibase--search-request)
               (lambda (&rest _) (error "should not search"))))
      (should (equal (org-chronicle-wikibase--resolve wd "anything") "Q42")))))

(ert-deftest org-chronicle-wikibase-test-resolve-pick ()
  "Test that a name term searches and presents candidates for selection."
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ada Lovelace"))
              ((symbol-function 'org-chronicle-wikibase--search-request)
               (lambda (_source _term)
                 (list (list :qid "Q7259" :label "Ada Lovelace"
                             :description "mathematician"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (car (if (functionp collection) (funcall collection "" nil t) collection)))))
      (should (equal (org-chronicle-wikibase--resolve wd "Ada Lovelace") "Q7259")))))

(ert-deftest org-chronicle-wikibase-test-record-object-qids ()
  (let ((rec (org-chronicle-wikibase--rows->record
              (org-chronicle-sources--get 'wikidata)
              "Q7259"
              (org-chronicle-wikibase--bindings
               (org-chronicle-wikibase-test--fixture "lovelace-vitals.json"))
              (org-chronicle-wikibase--bindings
               (org-chronicle-wikibase-test--fixture "lovelace-dates.json"))
              (org-chronicle-wikibase--bindings
               (org-chronicle-wikibase-test--fixture "lovelace-spouses.json"))
              (org-chronicle-wikibase--bindings
               (org-chronicle-wikibase-test--fixture "lovelace-events.json")))))
    (should (equal (plist-get (car (plist-get rec :spouses)) :qid) "Q336789"))
    (should (equal (plist-get (car (plist-get rec :events)) :qid) "Q18810745"))))

(ert-deftest org-chronicle-wikibase-test-queries-select-object-ids ()
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (should (string-match-p "?spouse"
                            (org-chronicle-wikibase--spouses-query wd "Q7259")))
    (should (string-match-p "?pos"
                            (org-chronicle-wikibase--events-query wd "Q7259")))))

(ert-deftest org-chronicle-wikibase-test-changes-object-qid ()
  (let* ((rec (list :source (org-chronicle-sources--get 'wikidata)
                    :qid "Q7259"
                    :spouses (list (list :name "William King-Noel" :qid "Q336789"
                                         :date (org-chronicle--date-parse "1835-07-08")))
                    :events (list (list :kind "position" :title "Countess of Lovelace"
                                        :qid "Q18810745"
                                        :date (org-chronicle--date-parse "1838")))))
         (changes (org-chronicle-wikibase--record->changes rec "Ada Lovelace"))
         (events (cl-remove-if-not (lambda (c) (eq (plist-get c :target) 'event)) changes)))
    (let ((marriage (cl-find "marriage" events
                             :key (lambda (c) (plist-get (plist-get c :event) :kind))
                             :test #'equal))
          (pos (cl-find "position" events
                        :key (lambda (c) (plist-get (plist-get c :event) :kind))
                        :test #'equal)))
      (should (equal (plist-get (plist-get marriage :event) :object-qid) "Q336789"))
      (should (equal (plist-get (plist-get pos :event) :object-qid) "Q18810745")))))

(ert-deftest org-chronicle-wikibase-test-url-by-source ()
  "Provenance URL uses the source's item-url-format."
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (should (equal (org-chronicle-wikibase--url wd "Q42")
                   "https://www.wikidata.org/wiki/Q42"))))

(ert-deftest org-chronicle-wikibase-test-span-query ()
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (q (org-chronicle-wikibase--span-query wd "Q1" "P571" "P576")))
    (should (string-match-p "p:P571" q))
    (should (string-match-p "p:P576" q))
    (should (string-match-p "\"start\"" q))
    (should (string-match-p "\"end\"" q))
    (should (string-match-p "wikibase:rank" q))))

(ert-deftest org-chronicle-wikibase-test-span-select ()
  (let* ((json "{\"results\":{\"bindings\":[\
{\"prop\":{\"value\":\"start\"},\"value\":{\"value\":\"1896-01-01T00:00:00Z\"},\"prec\":{\"value\":\"9\"},\"rank\":{\"value\":\"http://wikiba.se/ontology#NormalRank\"}}]}}")
         (sp (org-chronicle-wikibase--span-select
              (org-chronicle-wikibase--bindings json))))
    (should (equal (plist-get (plist-get sp :start) :year) 1896))
    (should (null (plist-get sp :end)))))

(ert-deftest org-chronicle-wikibase-test-dates->candidates ()
  (let* ((json "{\"results\":{\"bindings\":[\
{\"prop\":{\"value\":\"born\"},\"value\":{\"value\":\"1643-01-04T00:00:00Z\"},\"prec\":{\"value\":\"11\"},\"rank\":{\"value\":\"http://wikiba.se/ontology#PreferredRank\"}},\
{\"prop\":{\"value\":\"born\"},\"value\":{\"value\":\"-0900-01-01T00:00:00Z\"},\"prec\":{\"value\":\"7\"},\"rank\":{\"value\":\"http://wikiba.se/ontology#NormalRank\"}}]}}")
         (cands (org-chronicle-wikibase--dates->candidates
                 (org-chronicle-wikibase--bindings json))))
    (should (= (length cands) 2))
    (should (eq (plist-get (nth 0 cands) :rank) 'preferred))
    (should (equal (plist-get (plist-get (nth 0 cands) :date) :year) 1643))
    (should (equal (plist-get (nth 0 cands) :prop) "born"))
    (should (null (plist-get (nth 1 cands) :date)))
    (should (= (plist-get (nth 1 cands) :precision) 7))))

(ert-deftest org-chronicle-wikibase-test-coarse-date-label ()
  (should (equal (org-chronicle-wikibase--coarse-date-label "1640-01-01T00:00:00Z" 8) "1640s"))
  (should (equal (org-chronicle-wikibase--coarse-date-label "1643-01-01T00:00:00Z" 7) "17th century"))
  (should (equal (org-chronicle-wikibase--coarse-date-label "-0900-01-01T00:00:00Z" 7) "900 BC")))

(ert-deftest org-chronicle-wikibase-test-select-candidate ()
  (cl-flet ((cand (date prec rank)
                  (list :date (and date (org-chronicle--date-parse date))
                        :raw (and date (concat date "T00:00:00Z"))
                        :precision prec :rank rank)))
    (let ((sel (org-chronicle-wikibase--select-candidate
                (list (cand "1643-01-04" 11 'normal)
                      (cand "1643-01-04" 11 'preferred)
                      (cand "1642" 9 'normal)))))
      (should (equal (plist-get (plist-get sel :date) :year) 1643))
      (should (equal (plist-get (plist-get sel :date) :day) 4))
      (should (member "1642" (plist-get sel :alternates)))
      (should-not (member "1643-01-04" (plist-get sel :alternates))))
    (should (equal (plist-get (plist-get (org-chronicle-wikibase--select-candidate
                                          (list (cand "1500" 9 'normal)
                                                (cand "1500-06-15" 11 'normal))) :date) :day) 15))
    (should (equal (plist-get (plist-get (org-chronicle-wikibase--select-candidate
                                          (list (cand "1500" 9 nil)
                                                (cand "1500-06-15" 11 nil))) :date) :day) 15))
    (let ((sel (org-chronicle-wikibase--select-candidate
                (list (cand "1500-06-15" 11 'deprecated) (cand "1500" 9 'normal)))))
      (should (equal (plist-get (plist-get sel :date) :year) 1500))
      (should (null (plist-get (plist-get sel :date) :day))))
    (let ((sel (org-chronicle-wikibase--select-candidate
                (list (list :date nil :raw "1640-01-01T00:00:00Z" :precision 8 :rank 'preferred)
                      (cand "1643-01-04" 11 'normal)))))
      (should (equal (plist-get (plist-get sel :date) :year) 1643))
      (should (member "1640s" (plist-get sel :alternates))))
    (should (null (plist-get (org-chronicle-wikibase--select-candidate
                              (list (list :date nil :raw "1640-01-01T00:00:00Z"
                                          :precision 8 :rank 'normal))) :date)))))

(ert-deftest org-chronicle-wikibase-test-place-changes ()
  "Place record produces BUILT/ALIASES/WIKIDATA but not RAZED or BORN."
  (let* ((rec (list :source (org-chronicle-sources--get 'wikidata)
                    :qid "Q3505806" :kind 'place :label "Sutro Baths"
                    :aliases '("Sutro")
                    :start (org-chronicle--date-parse "1896")
                    :start-alternates '("1894") :end nil))
         (changes (org-chronicle-wikibase--record->changes rec "Sutro Baths"))
         (props (mapcar (lambda (c) (plist-get c :property))
                        (cl-remove-if-not (lambda (c) (eq (plist-get c :target) 'entity)) changes))))
    (should (member "BUILT" props))
    (should (member "ALIASES" props))
    (should (member "WIKIDATA" props))
    (should-not (member "RAZED" props))
    (should-not (member "BORN" props))
    (should (cl-every (lambda (c) (eq (plist-get c :target) 'entity)) changes))
    (let ((built (cl-find "BUILT" changes :key (lambda (c) (plist-get c :property)) :test #'equal)))
      (should (equal (plist-get built :alternates) '("1894")))
      (should (equal (plist-get built :provenance) "https://www.wikidata.org/wiki/Q3505806")))))

(ert-deftest org-chronicle-wikibase-test-fetch-record-place ()
  "Fetch-record for a place returns kind=place with label and start from SPARQL."
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (cl-letf (((symbol-function 'org-chronicle-wikibase--sparql-request)
               (lambda (_source q)
                 (org-chronicle-wikibase--bindings
                  (if (string-match-p "P571" q)
                      "{\"results\":{\"bindings\":[{\"prop\":{\"value\":\"start\"},\"value\":{\"value\":\"1896-01-01T00:00:00Z\"},\"prec\":{\"value\":\"9\"},\"rank\":{\"value\":\"http://wikiba.se/ontology#NormalRank\"}}]}}"
                    "{\"results\":{\"bindings\":[{\"label\":{\"value\":\"Sutro Baths\"}}]}}")))))
      (let ((rec (org-chronicle-wikibase--fetch-record wd "Q3505806" 'place)))
        (should (eq (plist-get rec :kind) 'place))
        (should (eq (plist-get rec :source) wd))
        (should (equal (plist-get rec :label) "Sutro Baths"))
        (should (equal (plist-get (plist-get rec :start) :year) 1896))
        (should (null (plist-get rec :end)))))))

(ert-deftest org-chronicle-wikibase-test-prefix-preamble ()
  "The preamble binds Wikibase prefixes to the source base URI."
  (let ((p (org-chronicle-wikibase--prefixes "https://database.factgrid.de")))
    (should (string-match-p
             "PREFIX wd: <https://database.factgrid.de/entity/>" p))
    (should (string-match-p
             "PREFIX wdt: <https://database.factgrid.de/prop/direct/>" p))
    (should (string-match-p
             "PREFIX psv: <https://database.factgrid.de/prop/statement/value/>" p))
    (should (string-match-p
             "PREFIX pqv: <https://database.factgrid.de/prop/qualifier/value/>" p))
    (should (string-match-p "PREFIX wikibase: <http://wikiba.se/ontology#>" p))))

(ert-deftest org-chronicle-wikibase-test-queries-parameterized ()
  "Query builders interpolate the source's prefixes and PIDs."
  (let* ((wd (org-chronicle-sources--get 'wikidata))
         (fake '(:base-uri "https://example.org" :label-language ("en")
                 :property-map (:span ((person "P1" . "P2"))
                                :birthplace "P3" :deathplace "P4"
                                :father "P5" :mother "P6"
                                :spouse "P7" :position "P8"
                                :qual-start "P9" :qual-end "P10"))))
    (should (string-match-p "PREFIX wd: <http://www.wikidata.org/entity/>"
                            (org-chronicle-wikibase--vitals-query wd "Q42")))
    (should (string-match-p "wdt:P19" (org-chronicle-wikibase--vitals-query wd "Q42")))
    (should (string-match-p "wdt:P3" (org-chronicle-wikibase--vitals-query fake "Q42")))
    (should (string-match-p "p:P7" (org-chronicle-wikibase--spouses-query fake "Q42")))
    (should (string-match-p "ps:P8" (org-chronicle-wikibase--events-query fake "Q42")))
    (should (string-match-p "p:P1 " (org-chronicle-wikibase--span-query
                                     fake "Q42" "P1" "P2")))))



(provide 'org-chronicle-wikibase-tests)
;;; org-chronicle-wikibase-tests.el ends here
