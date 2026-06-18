;;; org-chronicle-solve-tests.el --- Tests for org-chronicle-solve -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for the constraint engine.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle-solve)
(require 'cl-lib)

(ert-deftest org-chronicle-solve-test-loads ()
  "The solver feature loads."
  (should (featurep 'org-chronicle-solve)))

(provide 'org-chronicle-solve-tests)

;;; org-chronicle-solve-tests.el ends here
