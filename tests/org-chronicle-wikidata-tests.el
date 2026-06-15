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


(provide 'org-chronicle-wikidata-tests)
;;; org-chronicle-wikidata-tests.el ends here
