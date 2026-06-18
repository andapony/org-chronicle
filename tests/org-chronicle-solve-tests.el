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

(defun org-chronicle-solve-tests--net (nodes edges)
  "Build a network plist from NODES and EDGES for testing."
  (list :nodes nodes :edges edges))

(ert-deftest org-chronicle-solve-test-absolute-bounds ()
  "A node bounded below and above resolves to that closed interval."
  ;; x >= 10  ==>  zero - x <= -10  ==> edge (x zero -10)
  ;; x <= 20  ==>  x - zero <= 20   ==> edge (zero x 20)
  (let* ((net (org-chronicle-solve-tests--net
               '(:zero x)
               '((x :zero -10 lo) (:zero x 20 hi))))
         (sol (org-chronicle--stn-solve net)))
    (should (plist-get sol :consistent))
    (should (= (gethash 'x (plist-get sol :lo)) 10))
    (should (= (gethash 'x (plist-get sol :hi)) 20))))

(ert-deftest org-chronicle-solve-test-ordering-propagates ()
  "x <= y with an upper bound on y tightens x; a lower bound on x tightens y."
  ;; x <= y  ==>  x - y <= 0  ==> edge (y x 0)
  (let* ((net (org-chronicle-solve-tests--net
               '(:zero x y)
               '((y x 0 order) (x :zero -5 lox) (:zero y 30 hiy))))
         (sol (org-chronicle--stn-solve net)))
    (should (plist-get sol :consistent))
    (should (= (gethash 'x (plist-get sol :lo)) 5))
    (should (= (gethash 'x (plist-get sol :hi)) 30))
    (should (= (gethash 'y (plist-get sol :lo)) 5))))

(ert-deftest org-chronicle-solve-test-open-bounds-are-nil ()
  "A node with no upper constraint has a nil (open) upper bound."
  (let* ((net (org-chronicle-solve-tests--net
               '(:zero x) '((x :zero -10 lo))))
         (sol (org-chronicle--stn-solve net)))
    (should (plist-get sol :consistent))
    (should (= (gethash 'x (plist-get sol :lo)) 10))
    (should (null (gethash 'x (plist-get sol :hi))))))

(ert-deftest org-chronicle-solve-test-inconsistent ()
  "Lower bound above upper bound is inconsistent."
  (let* ((net (org-chronicle-solve-tests--net
               '(:zero x) '((x :zero -30 lo) (:zero x 20 hi))))
         (sol (org-chronicle--stn-solve net)))
    (should-not (plist-get sol :consistent))))






(provide 'org-chronicle-solve-tests)

;;; org-chronicle-solve-tests.el ends here
