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

(provide 'org-chronicle-wikidata-live-tests)
;;; org-chronicle-wikidata-live-tests.el ends here
