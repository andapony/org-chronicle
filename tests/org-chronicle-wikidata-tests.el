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






(provide 'org-chronicle-wikidata-tests)
;;; org-chronicle-wikidata-tests.el ends here
