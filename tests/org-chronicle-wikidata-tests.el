;;; org-chronicle-wikidata-tests.el --- Tests for org-chronicle-wikidata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle-wikidata)
(require 'cl-lib)

(ert-deftest org-chronicle-wikidata-test-loads ()
  "The integration loads and defines its group."
  (should (featurep 'org-chronicle-wikidata)))

(ert-deftest org-chronicle-wikidata-test-parse-qid ()
  "Test QID extraction from various string formats."
  (should (equal (org-chronicle-wikidata--parse-qid "Q42") "Q42"))
  (should (equal (org-chronicle-wikidata--parse-qid "  q42 ") "Q42"))
  (should (equal (org-chronicle-wikidata--parse-qid
                  "https://www.wikidata.org/wiki/Q7259") "Q7259"))
  (should (equal (org-chronicle-wikidata--parse-qid
                  "http://www.wikidata.org/entity/Q7259") "Q7259"))
  (should (null (org-chronicle-wikidata--parse-qid "not a qid")))
  (should (null (org-chronicle-wikidata--parse-qid nil)))
  (should (null (org-chronicle-wikidata--parse-qid ""))))

(ert-deftest org-chronicle-wikidata-test-time->date ()
  "Test conversion from Wikidata time strings to date plists."
  (let ((d (org-chronicle-wikidata--time->date "+1815-12-10T00:00:00Z" 11)))
    (should (equal (plist-get d :year) 1815))
    (should (equal (plist-get d :month) 12))
    (should (equal (plist-get d :day) 10))
    (should (eq (plist-get d :precision) 'day)))
  (should (eq (plist-get (org-chronicle-wikidata--time->date
                          "+1815-12-01T00:00:00Z" 10) :precision)
              'month))
  (should (eq (plist-get (org-chronicle-wikidata--time->date
                          "+1815-01-01T00:00:00Z" 9) :precision)
              'year))
  (should (null (org-chronicle-wikidata--time->date "+1810-01-01T00:00:00Z" 8)))
  (should (null (org-chronicle-wikidata--time->date "-0044-03-15T00:00:00Z" 11)))
  (should (null (org-chronicle-wikidata--time->date "+0500-01-01T00:00:00Z" 9)))
  (should (null (org-chronicle-wikidata--time->date nil 11))))

(ert-deftest org-chronicle-wikidata-test-bindings ()
  "Test SPARQL JSON parsing and binding accessors."
  (let* ((json "{\"results\":{\"bindings\":[\
{\"a\":{\"type\":\"literal\",\"value\":\"x\"},\"n\":{\"value\":\"11\"}}]}}")
         (rows (org-chronicle-wikidata--bindings json)))
    (should (= (length rows) 1))
    (should (equal (org-chronicle-wikidata--cell (car rows) "a") "x"))
    (should (null (org-chronicle-wikidata--cell (car rows) "missing")))
    (should (= (org-chronicle-wikidata--cell-int (car rows) "n") 11))
    (should (null (org-chronicle-wikidata--cell-int (car rows) "a")))))

(defvar org-chronicle-wikidata-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the wikidata test file, captured at load time.")

(ert-deftest org-chronicle-wikidata-test-search-request ()
  (cl-letf (((symbol-function 'org-chronicle-wikidata--http-get)
             (lambda (&rest _)
               (org-chronicle-wikidata-test--fixture "search-lovelace.json"))))
    (let ((cands (org-chronicle-wikidata--search-request "Ada Lovelace")))
      (should (equal (plist-get (car cands) :qid) "Q7259"))
      (should (equal (plist-get (car cands) :label) "Ada Lovelace"))
      (should (string-match-p "mathematician"
                              (plist-get (car cands) :description))))))

(ert-deftest org-chronicle-wikidata-test-http-error ()
  (cl-letf (((symbol-function 'org-chronicle-wikidata--http-get)
             (lambda (&rest _) (signal 'org-chronicle-wikidata-rate-limited nil))))
    (should-error (org-chronicle-wikidata--search-request "x")
                  :type 'org-chronicle-wikidata-rate-limited)))

(ert-deftest org-chronicle-wikidata-test-queries-mention-qid ()
  (should (string-match-p "wd:Q7259"
                          (org-chronicle-wikidata--vitals-query "Q7259")))
  (should (string-match-p "P26"
                          (org-chronicle-wikidata--spouses-query "Q7259")))
  (should (string-match-p "P39"
                          (org-chronicle-wikidata--events-query "Q7259"))))

(ert-deftest org-chronicle-wikidata-test-fetch-person ()
  (cl-letf (((symbol-function 'org-chronicle-wikidata--sparql-request)
             (lambda (q)
               (org-chronicle-wikidata--bindings
                (cond ((string-match-p "P26" q)
                       (org-chronicle-wikidata-test--fixture "lovelace-spouses.json"))
                      ((string-match-p "P39" q)
                       (org-chronicle-wikidata-test--fixture "lovelace-events.json"))
                      (t (org-chronicle-wikidata-test--fixture "lovelace-vitals.json")))))))
    (let ((rec (org-chronicle-wikidata--fetch-person "Q7259")))
      (should (equal (plist-get rec :qid) "Q7259"))
      (should (equal (plist-get rec :birthplace) "London"))
      (should (= (length (plist-get rec :spouses)) 1)))))

(defun org-chronicle-wikidata-test--fixture (name)
  "Return the contents of fixture NAME under tests/fixtures/."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "fixtures/%s" name)
                       org-chronicle-wikidata-test--directory))
    (buffer-string)))

(ert-deftest org-chronicle-wikidata-test-record ()
  "Test assembling a person record from parsed Wikidata binding rows."
  (let* ((rec (org-chronicle-wikidata--rows->record
               "Q7259"
               (org-chronicle-wikidata--bindings
                (org-chronicle-wikidata-test--fixture "lovelace-vitals.json"))
               (org-chronicle-wikidata--bindings
                (org-chronicle-wikidata-test--fixture "lovelace-spouses.json"))
               (org-chronicle-wikidata--bindings
                (org-chronicle-wikidata-test--fixture "lovelace-events.json")))))
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

(ert-deftest org-chronicle-wikidata-test-record->changes ()
  "Test that a person record produces the expected set of change plists."
  (let* ((rec (list :qid "Q7259"
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
         (changes (org-chronicle-wikidata--record->changes rec "Ada Lovelace")))
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
      (should (plist-get (prop "BORN") :default)))
    (let ((events (cl-remove-if-not (lambda (c) (eq (plist-get c :target) 'event)) changes)))
      (should (= (length events) 4))
      (let ((curated (cl-find "position" events
                              :key (lambda (c) (plist-get (plist-get c :event) :kind))
                              :test (lambda (_ k) (equal k "position")))))
        (should (null (plist-get curated :default)))))))

(ert-deftest org-chronicle-wikidata-test-classify ()
  "Test classification of a change against the current heading value."
  (let ((change (list :target 'entity :property "BORN" :value "1815-12-10")))
    (should (eq (org-chronicle-wikidata--classify change nil) 'new))
    (should (eq (org-chronicle-wikidata--classify change "1815-12-10") 'same))
    (should (eq (org-chronicle-wikidata--classify change "1900-01-01") 'conflict))))

(ert-deftest org-chronicle-wikidata-test-apply-changes ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n")
          (goto-char (point-min))
          (let ((changes
                 (list (list :target 'entity :property "BORN" :value "<1815-12-10>"
                             :provenance "https://www.wikidata.org/wiki/Q7259")
                       (list :target 'entity :property "WIKIDATA" :value "Q7259"
                             :provenance "https://www.wikidata.org/wiki/Q7259")
                       (list :target 'event :key "birth:ABC"
                             :provenance "https://www.wikidata.org/wiki/Q7259"
                             :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                          :date "1815-12-10" :subject (list "Ada Lovelace")
                                          :location "London")))))
            (org-chronicle-wikidata--apply-changes
             changes (org-chronicle-wikidata--events-index)))
          (goto-char (point-min))
          (should (equal (org-entry-get nil "BORN") "<1815-12-10>"))
          (should (equal (org-entry-get nil "WIKIDATA") "Q7259"))
          (should (equal (org-entry-get nil "SOURCES")
                         "https://www.wikidata.org/wiki/Q7259"))
          (let ((events (with-temp-buffer
                          (insert-file-contents org-chronicle-wikidata-file)
                          (buffer-string))))
            (should (string-match-p "Birth of Ada Lovelace" events))
            (should (string-match-p ":LIFE-EVENT: birth" events))
            (should (string-match-p ":IMPORT-KEY: birth:ABC" events))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-candidate-line ()
  "Test formatting a candidate plist as a completion line."
  (should (equal (org-chronicle-wikidata--candidate-line
                  (list :qid "Q7259" :label "Ada Lovelace"
                        :description "English mathematician"))
                 "Ada Lovelace — English mathematician (Q7259)")))

(ert-deftest org-chronicle-wikidata-test-resolve-paste ()
  "Test that pasting a QID short-circuits search."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Q42"))
            ((symbol-function 'org-chronicle-wikidata--search-request)
             (lambda (&rest _) (error "should not search"))))
    (should (equal (org-chronicle-wikidata--resolve "anything") "Q42"))))

(ert-deftest org-chronicle-wikidata-test-resolve-pick ()
  "Test that a name term searches and presents candidates for selection."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Ada Lovelace"))
            ((symbol-function 'org-chronicle-wikidata--search-request)
             (lambda (&rest _)
               (list (list :qid "Q7259" :label "Ada Lovelace"
                           :description "mathematician"))))
            ((symbol-function 'completing-read)
             (lambda (_prompt collection &rest _)
               (car (if (functionp collection) (funcall collection "" nil t) collection)))))
    (should (equal (org-chronicle-wikidata--resolve "Ada Lovelace") "Q7259"))))

(ert-deftest org-chronicle-wikidata-test-review-rows ()
  "Test that review rows carry selected state from change defaults."
  (let* ((changes
          (list (list :target 'entity :group 'vitals :property "BORN"
                      :value "1815-12-10" :default t :status 'new)
                (list :target 'event :group 'events :default nil :status 'new
                      :event (list :title "Countess of Lovelace" :date "1838"))))
         (rows (org-chronicle-wikidata--review-rows changes)))
    (should (= (length rows) 2))
    (should (plist-get (nth 0 (car rows)) :selected))
    (should-not (plist-get (nth 0 (cadr rows)) :selected))))

(ert-deftest org-chronicle-wikidata-test-selected-changes ()
  "Test that only rows with :selected non-nil are returned."
  (let ((rows (list (list (list :selected t) (list :property "BORN"))
                    (list (list :selected nil) (list :property "DIED")))))
    (let ((sel (org-chronicle-wikidata--selected-changes rows)))
      (should (= (length sel) 1))
      (should (equal (plist-get (car sel) :property) "BORN")))))

(ert-deftest org-chronicle-wikidata-test-import ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root))
         (people-file (expand-file-name "people.org" root)))
    (unwind-protect
        (progn
          (with-temp-file people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n"))
          (cl-letf (((symbol-function 'org-chronicle-wikidata--resolve)
                     (lambda (&rest _) "Q7259"))
                    ((symbol-function 'org-chronicle-wikidata--fetch-person)
                     (lambda (_qid)
                       (list :qid "Q7259"
                             :born (org-chronicle--date-parse "1815-12-10")
                             :birthplace "London")))
                    ((symbol-function 'org-chronicle-wikidata--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (with-current-buffer (find-file-noselect people-file)
              (goto-char (point-min))
              (org-chronicle-wikidata-import))
            (let ((content (with-temp-buffer
                             (insert-file-contents people-file)
                             (buffer-string))))
              (should (string-match-p ":BORN:.*1815-12-10" content))
              (should (string-match-p ":WIKIDATA:.*Q7259" content)))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-diff ()
  (let* ((changes
          (list (list :target 'entity :property "BORN" :value "1815-12-10")
                (list :target 'entity :property "DIED" :value "1852-11-27")
                (list :target 'entity :property "BIRTHPLACE" :value "London")))
         (current (lambda (p) (pcase p
                                ("BORN" "1815-12-10")
                                ("DIED" "1900-01-01")
                                (_ nil))))
         (drift (org-chronicle-wikidata--diff changes current)))
    (should (= (length drift) 2))
    (should (cl-find "DIED" drift
                     :key (lambda (d) (plist-get d :property)) :test #'equal))
    (should (cl-find "BIRTHPLACE" drift
                     :key (lambda (d) (plist-get d :property)) :test #'equal))
    (should-not (cl-find "BORN" drift
                         :key (lambda (d) (plist-get d :property)) :test #'equal))))

(ert-deftest org-chronicle-wikidata-test-live-lovelace ()
  "Hit the live endpoint.  Excluded from the default suite."
  :tags '(:wikidata-live)
  (let ((rec (org-chronicle-wikidata--fetch-person "Q7259")))
    (should (equal (plist-get (plist-get rec :born) :year) 1815))))

(ert-deftest org-chronicle-wikidata-test-event-change-string ()
  "Life events use life-event-string; position events use event-string."
  (let ((life (org-chronicle-wikidata--event-change-string
               (list :provenance "u"
                     :event (list :life-event "birth" :title "Birth of X"
                                  :date "1815-12-10" :subject (list "X")))))
        (pos (org-chronicle-wikidata--event-change-string
              (list :provenance "u"
                    :event (list :title "Countess of Lovelace" :date "1838")))))
    (should (string-match-p ":LIFE-EVENT: birth" life))
    (should (string-match-p ":SUBJECT:" life))
    (should-not (string-match-p "LIFE-EVENT" pos))
    (should-not (string-match-p "nil" pos))))

(ert-deftest org-chronicle-wikidata-test-sources-append ()
  "Applying a change appends to SOURCES rather than overwriting it."
  (with-temp-buffer
    (org-mode)
    (insert "* X\n:PROPERTIES:\n:SOURCES: my-book p.12\n:END:\n")
    (goto-char (point-min))
    (org-chronicle-wikidata--apply-entity-change
     (list :target 'entity :property "BORN" :value "<1815-12-10>"
           :provenance "https://www.wikidata.org/wiki/Q7259"))
    (goto-char (point-min))
    (let ((s (org-entry-get nil "SOURCES")))
      (should (string-match-p "my-book p.12" s))
      (should (string-match-p "Q7259" s)))))

(ert-deftest org-chronicle-wikidata-test-classify-dates ()
  "BORN/DIED classification ignores brackets around dates."
  (let ((change (list :target 'entity :property "BORN" :value "<1815-12-10>")))
    (should (eq (org-chronicle-wikidata--classify change "1815-12-10") 'same))
    (should (eq (org-chronicle-wikidata--classify change "<1815-12-10>") 'same))
    (should (eq (org-chronicle-wikidata--classify change "1900-01-01") 'conflict))))

(ert-deftest org-chronicle-wikidata-test-import-create ()
  "Non-entity heading triggers create-person then enriches the new entity."
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Charles Babbage\nsome prose\n")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'org-chronicle-wikidata--resolve)
                     (lambda (&rest _) "Q46633"))
                    ((symbol-function 'org-chronicle-wikidata--fetch-person)
                     (lambda (_qid)
                       (list :qid "Q46633"
                             :born (org-chronicle--date-parse "1791-12-26"))))
                    ((symbol-function 'org-chronicle-wikidata--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (org-chronicle-wikidata-import))
          (let ((people (with-temp-buffer
                          (insert-file-contents org-chronicle-people-file)
                          (buffer-string))))
            (should (string-match-p "Charles Babbage" people))
            (should (string-match-p ":KIND:" people))
            (should (string-match-p ":WIKIDATA:.*Q46633" people))
            (should (string-match-p ":BORN:" people))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-import-promote ()
  "Point on SPOUSE property creates a new entity seeded from the value."
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:SPOUSE: William King-Noel\n:END:\n")
          (goto-char (point-min))
          (search-forward ":SPOUSE:")
          (beginning-of-line)
          (cl-letf (((symbol-function 'org-chronicle-wikidata--resolve)
                     (lambda (seed)
                       (should (string-match-p "William" seed))
                       "Q123"))
                    ((symbol-function 'org-chronicle-wikidata--fetch-person)
                     (lambda (_qid)
                       (list :qid "Q123"
                             :born (org-chronicle--date-parse "1805"))))
                    ((symbol-function 'org-chronicle-wikidata--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (org-chronicle-wikidata-import))
          (let ((people (with-temp-buffer
                          (insert-file-contents org-chronicle-people-file)
                          (buffer-string))))
            (should (string-match-p "William King-Noel" people))
            (should (string-match-p ":WIKIDATA:.*Q123" people))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-events-file ()
  (let ((org-chronicle-root "/tmp/octw/"))
    (let ((org-chronicle-wikidata-file nil))
      (should (equal (org-chronicle-wikidata--events-file)
                     "/tmp/octw/imported/events.org")))
    (let ((org-chronicle-wikidata-file "/tmp/elsewhere.org"))
      (should (equal (org-chronicle-wikidata--events-file)
                     "/tmp/elsewhere.org")))))

(ert-deftest org-chronicle-wikidata-test-record-object-qids ()
  (let ((rec (org-chronicle-wikidata--rows->record
              "Q7259"
              (org-chronicle-wikidata--bindings
               (org-chronicle-wikidata-test--fixture "lovelace-vitals.json"))
              (org-chronicle-wikidata--bindings
               (org-chronicle-wikidata-test--fixture "lovelace-spouses.json"))
              (org-chronicle-wikidata--bindings
               (org-chronicle-wikidata-test--fixture "lovelace-events.json")))))
    (should (equal (plist-get (car (plist-get rec :spouses)) :qid) "Q336789"))
    (should (equal (plist-get (car (plist-get rec :events)) :qid) "Q18810745"))))

(ert-deftest org-chronicle-wikidata-test-queries-select-object-ids ()
  (should (string-match-p "?spouse"
                          (org-chronicle-wikidata--spouses-query "Q7259")))
  (should (string-match-p "?pos"
                          (org-chronicle-wikidata--events-query "Q7259"))))

(ert-deftest org-chronicle-wikidata-test-changes-object-qid ()
  (let* ((rec (list :qid "Q7259"
                    :spouses (list (list :name "William King-Noel" :qid "Q336789"
                                         :date (org-chronicle--date-parse "1835-07-08")))
                    :events (list (list :kind "position" :title "Countess of Lovelace"
                                        :qid "Q18810745"
                                        :date (org-chronicle--date-parse "1838")))))
         (changes (org-chronicle-wikidata--record->changes rec "Ada Lovelace"))
         (events (cl-remove-if-not (lambda (c) (eq (plist-get c :target) 'event)) changes)))
    (let ((marriage (cl-find "marriage" events
                             :key (lambda (c) (plist-get (plist-get c :event) :kind))
                             :test #'equal))
          (pos (cl-find "position" events
                        :key (lambda (c) (plist-get (plist-get c :event) :kind))
                        :test #'equal)))
      (should (equal (plist-get (plist-get marriage :event) :object-qid) "Q336789"))
      (should (equal (plist-get (plist-get pos :event) :object-qid) "Q18810745")))))

(ert-deftest org-chronicle-wikidata-test-event-key ()
  (should (equal (org-chronicle-wikidata--event-key
                  (list :kind "birth") "ABC" "Q7259") "birth:ABC"))
  (should (equal (org-chronicle-wikidata--event-key
                  (list :kind "death") "ABC" "Q7259") "death:ABC"))
  (should (equal (org-chronicle-wikidata--event-key
                  (list :kind "position" :object-qid "Q30") "ABC" "Q7259")
                 "position:ABC:wd:Q30"))
  (should (equal (org-chronicle-wikidata--event-key
                  (list :kind "marriage" :object-qid "Q123") "ABC" "Q7259")
                 (org-chronicle-wikidata--event-key
                  (list :kind "marriage" :object-qid "Q7259") "XYZ" "Q123")))
  (should (equal (org-chronicle-wikidata--event-key
                  (list :kind "marriage" :object-qid "Q123") "ABC" "Q7259")
                 "marriage:wd:Q123:wd:Q7259"))
  (should (null (org-chronicle-wikidata--event-key
                 (list :kind "marriage") "ABC" "Q7259")))
  (should (null (org-chronicle-wikidata--event-key
                 (list :kind "position") "ABC" "Q7259"))))

(ert-deftest org-chronicle-wikidata-test-events-index-and-append ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root)))
    (unwind-protect
        (let ((index (org-chronicle-wikidata--events-index)))
          (should (= (hash-table-count index) 0))
          (org-chronicle-wikidata--append-to-events-file
           "* Birth of X\n:PROPERTIES:\n:DATE: <1815-12-10>\n:END:\n"
           "birth:ABC" index)
          (should (gethash "birth:ABC" index))
          (let ((fresh (org-chronicle-wikidata--events-index)))
            (should (gethash "birth:ABC" fresh))
            (org-with-point-at (gethash "birth:ABC" fresh)
              (should (equal (org-entry-get nil "IMPORT-KEY") "birth:ABC")))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-classify-event ()
  (with-temp-buffer
    (org-mode)
    (insert "* Birth of X\n:PROPERTIES:\n:DATE: <1815-12-10>\n:LOCATION: London\n:END:\n")
    (goto-char (point-min))
    (let ((m (point-marker))
          (same (list :event (list :date "1815-12-10" :location "London")))
          (confl (list :event (list :date "1900-01-01" :location "London"))))
      (should (eq (org-chronicle-wikidata--classify-event same m) 'same))
      (should (eq (org-chronicle-wikidata--classify-event confl m) 'conflict))
      (should (eq (org-chronicle-wikidata--classify-event same nil) 'new)))))

(ert-deftest org-chronicle-wikidata-test-apply-event-idempotent ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (change (list :target 'event :key "birth:ABC"
                       :provenance "https://www.wikidata.org/wiki/Q7259"
                       :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                    :date "1815-12-10" :subject (list "Ada Lovelace")
                                    :location "London"))))
    (unwind-protect
        (let ((index (org-chronicle-wikidata--events-index)))
          (org-chronicle-wikidata--apply-event-change change index)
          (org-chronicle-wikidata--apply-event-change change index)
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-wikidata-file)
                        (buffer-string))))
            (should (= 1 (cl-count ?* body)))
            (should (string-match-p ":IMPORT-KEY: birth:ABC" body))
            (should (string-match-p "Birth of Ada Lovelace" body)))
          (let ((change2 (copy-sequence change)))
            (plist-put change2 :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                            :date "1816-01-01" :subject (list "Ada Lovelace")))
            (org-chronicle-wikidata--apply-event-change change2 index))
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-wikidata-file)
                        (buffer-string))))
            (should (= 1 (cl-count ?* body)))
            (should (string-match-p "1816-01-01" body))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-import-events-idempotent ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (people-file (expand-file-name "people.org" root)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-chronicle-wikidata--resolve)
                   (lambda (&rest _) "Q7259"))
                  ((symbol-function 'org-chronicle-wikidata--fetch-person)
                   (lambda (_qid)
                     (list :qid "Q7259"
                           :born (org-chronicle--date-parse "1815-12-10")
                           :birthplace "London")))
                  ((symbol-function 'org-chronicle-wikidata--review)
                   (lambda (changes on-confirm) (funcall on-confirm changes))))
          (with-temp-file people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n"))
          (cl-flet ((run ()
                      (with-current-buffer (find-file-noselect people-file)
                        (goto-char (point-min))
                        (org-chronicle-wikidata-import))))
            (run)
            (run)
            (let ((body (with-temp-buffer
                          (insert-file-contents org-chronicle-wikidata-file)
                          (buffer-string))))
              (should (= 1 (cl-count-if
                            (lambda (l) (string-match-p "Birth of Ada Lovelace" l))
                            (split-string body "\n")))))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikidata-test-apply-event-preserves-unmanaged ()
  (let* ((root (make-temp-file "octw-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-wikidata-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (change (list :target 'event :key "birth:ABC"
                       :provenance "https://www.wikidata.org/wiki/Q7259"
                       :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                    :date "1815-12-10" :subject (list "Ada Lovelace")))))
    (unwind-protect
        (let ((index (org-chronicle-wikidata--events-index)))
          (org-chronicle-wikidata--apply-event-change change index)
          ;; Author edits the heading: renames it and adds an unmanaged property.
          (org-with-point-at (gethash "birth:ABC" index)
            (org-back-to-heading t)
            (org-edit-headline "Ada's birth (my note)")
            (org-set-property "NOTES" "hand-written")
            (save-buffer))
          ;; Re-apply with a changed date.
          (let ((change2 (copy-sequence change)))
            (plist-put change2 :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                            :date "1816-02-02" :subject (list "Ada Lovelace")))
            (org-chronicle-wikidata--apply-event-change change2 index))
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-wikidata-file)
                        (buffer-string))))
            (should (string-match-p "1816-02-02" body))          ; managed field updated
            (should (string-match-p "Ada's birth (my note)" body)) ; title preserved
            (should (string-match-p ":NOTES: *hand-written" body)))) ; unmanaged prop preserved
      (delete-directory root t))))

(provide 'org-chronicle-wikidata-tests)
;;; org-chronicle-wikidata-tests.el ends here
