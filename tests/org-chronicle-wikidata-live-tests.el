;;; org-chronicle-wikidata-live-tests.el --- Live Wikidata tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Integration tests that hit the live Wikidata endpoint.  Run explicitly with
;; `make test-live'.  Never part of `make' / `make all'.  Each test skips (not
;; fails) when Wikidata is unreachable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-chronicle-wikidata)

(defun org-chronicle-wikidata-live--fetch-or-skip (thunk)
  "Call THUNK; on a Wikidata transport error, skip the test instead of failing.
Assertion failures in the caller still fail normally."
  (condition-case err
      (funcall thunk)
    (org-chronicle-wikidata-error
     (ert-skip (format "Wikidata unreachable — skipping: %S" err)))))

(ert-deftest org-chronicle-wikidata-test-live-search ()
  "Live `wbsearchentities' returns Ada Lovelace (Q7259)."
  :tags '(:wikidata-live)
  (let* ((cands (org-chronicle-wikidata-live--fetch-or-skip
                 (lambda () (org-chronicle-wikidata--search-request "Ada Lovelace"))))
         (hit (cl-find "Q7259" cands
                       :key (lambda (c) (plist-get c :qid)) :test #'equal)))
    (should hit)
    (should (stringp (plist-get hit :label)))
    (should (> (length (plist-get hit :label)) 0))
    (should (stringp (plist-get hit :description)))))

(ert-deftest org-chronicle-wikidata-test-live-fetch-ada ()
  "Live fetch of Ada Lovelace (Q7259) parses vitals, a spouse, and aliases."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikidata-live--fetch-or-skip
               (lambda () (org-chronicle-wikidata--fetch-person "Q7259"))))
         (born (plist-get rec :born))
         (died (plist-get rec :died))
         (spouse (car (plist-get rec :spouses)))
         (aliases (plist-get rec :aliases)))
    (should (equal (plist-get born :year) 1815))
    (should (equal (plist-get born :month) 12))
    (should (equal (plist-get born :day) 10))
    (should (equal (plist-get died :year) 1852))
    (should (equal (plist-get died :month) 11))
    (should (equal (plist-get died :day) 27))
    (should (equal (plist-get rec :birthplace) "London"))
    (should (equal (plist-get rec :deathplace) "Marylebone"))
    (should (equal (plist-get rec :father) "Lord Byron"))
    (should (equal (plist-get rec :mother) "Anne Isabella Byron"))
    (should (equal (plist-get rec :label) "Ada Lovelace"))
    (should spouse)
    (should (string-match-p "\\`Q[0-9]+\\'" (plist-get spouse :qid)))
    (should (>= (length aliases) 1))
    (should (cl-every #'stringp aliases))))

(ert-deftest org-chronicle-wikidata-test-live-position-brannan ()
  "Live fetch of Samuel Brannan (Q936075) parses a position-held event."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikidata-live--fetch-or-skip
               (lambda () (org-chronicle-wikidata--fetch-person "Q936075"))))
         (event (car (plist-get rec :events))))
    (should (equal (plist-get (plist-get rec :born) :year) 1819))
    (should event)
    (should (equal (plist-get event :kind) "position"))
    (should (string-match-p "\\`Q[0-9]+\\'" (plist-get event :qid)))))




(provide 'org-chronicle-wikidata-live-tests)
;;; org-chronicle-wikidata-live-tests.el ends here
