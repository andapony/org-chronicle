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

;;;; Date module

(ert-deftest org-chronicle-test-date-parse-day ()
  (let ((d (org-chronicle--date-parse "<1863-07-04>")))
    (should (equal (plist-get d :year) 1863))
    (should (equal (plist-get d :month) 7))
    (should (equal (plist-get d :day) 4))
    (should (eq (plist-get d :precision) 'day))))

(ert-deftest org-chronicle-test-date-parse-precisions ()
  (should (eq (plist-get (org-chronicle--date-parse "1863") :precision) 'year))
  (should (eq (plist-get (org-chronicle--date-parse "1863-07") :precision) 'month))
  (should (null (org-chronicle--date-parse "not a date")))
  (should (null (org-chronicle--date-parse nil))))

(ert-deftest org-chronicle-test-date-lessp ()
  (let ((a (org-chronicle--date-parse "1863-07-04"))
        (b (org-chronicle--date-parse "1865-04-09")))
    (should (org-chronicle--date-lessp a b))
    (should-not (org-chronicle--date-lessp b a))
    (should-not (org-chronicle--date-lessp a a))))

(ert-deftest org-chronicle-test-date-format ()
  (should (equal (org-chronicle--date-format (org-chronicle--date-parse "1863-07-04")) "1863-07-04"))
  (should (equal (org-chronicle--date-format (org-chronicle--date-parse "1863-07")) "1863-07"))
  (should (equal (org-chronicle--date-format (org-chronicle--date-parse "1863")) "1863")))

(ert-deftest org-chronicle-test-date-in-span ()
  (let ((d (org-chronicle--date-parse "1864-01-01"))
        (from (org-chronicle--date-parse "1863-01-03"))
        (to (org-chronicle--date-parse "1865-04-27")))
    (should (org-chronicle--date-in-span-p d from to))
    (should (org-chronicle--date-in-span-p d nil to))
    (should (org-chronicle--date-in-span-p d from nil))
    (should-not (org-chronicle--date-in-span-p (org-chronicle--date-parse "1870-01-01") from to))
    (should-not (org-chronicle--date-in-span-p (org-chronicle--date-parse "1860-01-01") from to))))

(provide 'org-chronicle-tests)
;;; org-chronicle-tests.el ends here
