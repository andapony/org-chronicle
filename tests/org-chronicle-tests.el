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

;;;; Store

(defmacro org-chronicle-test--with-org (text &rest body)
  "Run BODY in a temp Org buffer containing TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defconst org-chronicle-test--timeline "\
* Vicksburg falls
:PROPERTIES:
:ID:       evt-vicksburg
:TRUTH:    historical
:DATE:     <1863-07-04>
:PEOPLE:   Ulysses S. Grant; John C. Pemberton
:LOCATION: Vicksburg, Mississippi
:END:
* Secret meeting
:PROPERTIES:
:ID:       evt-meeting
:TRUTH:    fictional
:DATE:     <1864-07-12>
:DATE-END: <1864-07-13>
:PEOPLE:   Ulysses S. Grant; Abraham Lincoln
:END:
* Not an event
:PROPERTIES:
:NOTE: skip me
:END:
")

(ert-deftest org-chronicle-test-split ()
  (should (equal (org-chronicle--split "a; b ;c") '("a" "b" "c")))
  (should (null (org-chronicle--split "   ")))
  (should (null (org-chronicle--split nil))))

(ert-deftest org-chronicle-test-buffer-events ()
  (org-chronicle-test--with-org org-chronicle-test--timeline
    (let ((events (org-chronicle--buffer-events)))
      (should (= (length events) 2))
      (let ((e (car events)))
        (should (equal (plist-get e :title) "Vicksburg falls"))
        (should (equal (plist-get e :truth) "historical"))
        (should (equal (plist-get e :people) '("Ulysses S. Grant" "John C. Pemberton")))
        (should (eq (plist-get (plist-get e :date) :precision) 'day)))
      (let ((m (nth 1 events)))
        (should (plist-get m :date-end))))))

(defconst org-chronicle-test--entities "\
* Pinkerton Agency
:PROPERTIES:
:ID:       ent-pinkerton
:KIND:     group
:FOUNDED:  <1850-01-01>
:END:
* Ulysses S. Grant
:PROPERTIES:
:ID:        ent-grant
:KIND:      person
:ALIASES:   Grant; U.S. Grant
:MEMBER-OF: ent-pinkerton
:BORN:      <1822-04-27>
:DIED:      <1885-07-23>
:END:
* Mississippi
:PROPERTIES:
:ID:   ent-ms
:KIND: place
:END:
* Vicksburg, Mississippi
:PROPERTIES:
:ID:      ent-vicksburg
:KIND:    place
:PART-OF: ent-ms
:END:
")

(ert-deftest org-chronicle-test-buffer-entities ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let* ((ents (org-chronicle--buffer-entities))
           (grant (cl-find "ent-grant" ents
                           :key (lambda (e) (plist-get e :id)) :test #'equal)))
      (should (= (length ents) 4))
      (should (eq (plist-get grant :kind) 'person))
      (should (equal (plist-get grant :aliases) '("Grant" "U.S. Grant")))
      (should (equal (plist-get grant :member-of) '("ent-pinkerton")))
      (should (eq (plist-get (plist-get grant :span-from) :precision) 'day)))))

(ert-deftest org-chronicle-test-span-by-kind ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let* ((ents (org-chronicle--buffer-entities))
           (pink (cl-find "ent-pinkerton" ents
                          :key (lambda (e) (plist-get e :id)) :test #'equal)))
      (should (plist-get pink :span-from))
      (should (null (plist-get pink :span-to))))))

(ert-deftest org-chronicle-test-alias-index ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let ((idx (org-chronicle--alias-index (org-chronicle--buffer-entities))))
      (should (equal (gethash "grant" idx) "Ulysses S. Grant"))
      (should (equal (gethash "u.s. grant" idx) "Ulysses S. Grant"))
      (should (equal (gethash "ulysses s. grant" idx) "Ulysses S. Grant")))))





(provide 'org-chronicle-tests)
;;; org-chronicle-tests.el ends here
