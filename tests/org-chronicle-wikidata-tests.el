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



(provide 'org-chronicle-wikidata-tests)
;;; org-chronicle-wikidata-tests.el ends here
