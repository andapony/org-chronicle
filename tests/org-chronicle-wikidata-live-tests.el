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


(provide 'org-chronicle-wikidata-live-tests)
;;; org-chronicle-wikidata-live-tests.el ends here
