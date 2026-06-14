;;; org-chronicle-tests.el --- Tests for org-chronicle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle)
(require 'cl-lib)

(ert-deftest org-chronicle-test-loads ()
  "The package loads and defines its group."
  (should (featurep 'org-chronicle)))

(provide 'org-chronicle-tests)
;;; org-chronicle-tests.el ends here
