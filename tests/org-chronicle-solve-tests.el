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

(ert-deftest org-chronicle-solve-test-conflict-labels ()
  "An over-constrained node yields the labels of its conflicting edges."
  (let* ((net (org-chronicle-solve-tests--net
               '(:zero x) '((x :zero -30 need-after) (:zero x 20 need-before))))
         (labels (org-chronicle--stn-conflict net)))
    (should (member 'need-after labels))
    (should (member 'need-before labels))))

(ert-deftest org-chronicle-solve-test-no-conflict-when-consistent ()
  "A consistent network reports no conflict."
  (let ((net (org-chronicle-solve-tests--net
              '(:zero x) '((x :zero -10 lo) (:zero x 20 hi)))))
    (should (null (org-chronicle--stn-conflict net)))))

(defun org-chronicle-solve-tests--ctx (events)
  "Return a context plist whose :events-by-id is keyed by EVENTS' :id."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (e events) (puthash (plist-get e :id) e by-id))
    (list :entities nil :idx nil :index nil :events-by-id by-id)))

(defun org-chronicle-solve-tests--scene (marker &rest kvs)
  "Return a scene plist with MARKER and KVS, defaulting list fields to nil."
  (append (list :marker marker) kvs
          (list :refs nil :event-ids nil
                :after-ids nil :before-ids nil
                :own-date nil :own-date-end nil :earliest nil :latest nil)))

(ert-deftest org-chronicle-solve-test-earliest-latest-window ()
  "EARLIEST/LATEST bound a floating scene's start/end."
  (let* ((s (org-chronicle-solve-tests--scene
             'm :earliest (org-chronicle--date-parse "1850")
             :latest (org-chronicle--date-parse "1855")))
         (net (org-chronicle--build-network (list s)
                                            (org-chronicle-solve-tests--ctx nil)))
         (sol (org-chronicle--stn-solve net))
         (start (gethash 'm (plist-get net :starts)))
         (end (gethash 'm (plist-get net :ends))))
    (should (plist-get sol :consistent))
    (should (= (gethash start (plist-get sol :lo))
               (org-chronicle--date-ordinal
                (org-chronicle--date-lower-bound (org-chronicle--date-parse "1850")))))
    (should (= (gethash end (plist-get sol :hi))
               (org-chronicle--date-ordinal
                (org-chronicle--date-upper-bound (org-chronicle--date-parse "1855")))))))

(ert-deftest org-chronicle-solve-test-after-event-propagates ()
  "A scene AFTER a dated event gets a lower bound from the event's end."
  (let* ((ev (list :id "E1" :title "Duel" :date (org-chronicle--date-parse "1862")
                   :date-end nil))
         (s (org-chronicle-solve-tests--scene 'm :after-ids '("E1")))
         (net (org-chronicle--build-network
               (list s) (org-chronicle-solve-tests--ctx (list ev))))
         (sol (org-chronicle--stn-solve net))
         (start (gethash 'm (plist-get net :starts))))
    (should (= (gethash start (plist-get sol :lo))
               (org-chronicle--date-ordinal
                (org-chronicle--date-lower-bound (org-chronicle--date-parse "1862")))))))

(ert-deftest org-chronicle-solve-test-floating-to-floating ()
  "B AFTER floating A: bounding A bounds B even though neither is dated."
  (let* ((a (org-chronicle-solve-tests--scene
             'ma :earliest (org-chronicle--date-parse "1870")))
         ;; B references A's id; A's marker is its node key, but AFTER uses
         ;; ids, so give A an :id and register scenes-by-id via ctx extension.
         (a (plist-put a :id "A"))
         (b (org-chronicle-solve-tests--scene 'mb :after-ids '("A")))
         (ctx (org-chronicle-solve-tests--ctx nil))
         (net (org-chronicle--build-network (list a b) ctx))
         (sol (org-chronicle--stn-solve net))
         (bstart (gethash 'mb (plist-get net :starts))))
    (should (plist-get sol :consistent))
    (should (= (gethash bstart (plist-get sol :lo))
               (org-chronicle--date-ordinal
                (org-chronicle--date-lower-bound (org-chronicle--date-parse "1870")))))))






(provide 'org-chronicle-solve-tests)

;;; org-chronicle-solve-tests.el ends here
