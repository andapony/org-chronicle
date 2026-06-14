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

(ert-deftest org-chronicle-test-event-reads-life-fields ()
  "An event heading exposes its life-event, subject, and new-name fields."
  (org-chronicle-test--with-org "\
* Birth of Grant
:PROPERTIES:
:TRUTH: historical
:LIFE-EVENT: birth
:DATE: <1822-04-27>
:SUBJECT: Ulysses S. Grant
:PEOPLE: Ulysses S. Grant; Jesse Root Grant
:LOCATION: Point Pleasant, Ohio
:END:
"
    (let ((e (car (org-chronicle--buffer-events))))
      (should (equal (plist-get e :life-event) "birth"))
      (should (equal (plist-get e :subject) '("Ulysses S. Grant")))
      (should (null (plist-get e :new-name))))))

(ert-deftest org-chronicle-test-event-reads-topics ()
  "An event heading exposes its TOPICS as a list of topic strings."
  (org-chronicle-test--with-org "\
* Wreck of the Pegasus
:PROPERTIES:
:TRUTH: fictional
:DATE: <1851-11-03>
:TOPICS: shipping; crime
:END:
"
				(let ((e (car (org-chronicle--buffer-events))))
				  (should (equal (plist-get e :topics) '("shipping" "crime"))))))

(ert-deftest org-chronicle-test-entity-reads-deathplace ()
  "A person entity exposes its DEATHPLACE property."
  (org-chronicle-test--with-org "\
* Grant
:PROPERTIES:
:KIND: person
:DEATHPLACE: Mount McGregor, New York
:END:
"
    (let ((ent (car (org-chronicle--buffer-entities))))
      (should (equal (plist-get ent :deathplace) "Mount McGregor, New York")))))



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

;;;; Relations

(ert-deftest org-chronicle-test-group-members ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let ((ents (org-chronicle--buffer-entities)))
      (should (equal (org-chronicle--group-member-names "ent-pinkerton" ents)
                     '("Ulysses S. Grant"))))))

(ert-deftest org-chronicle-test-place-descendants ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let ((ents (org-chronicle--buffer-entities)))
      (should (equal (sort (org-chronicle--place-descendant-names "ent-ms" ents)
                           #'string<)
                     '("Mississippi" "Vicksburg, Mississippi"))))))

(ert-deftest org-chronicle-test-children ()
  (org-chronicle-test--with-org "\
* Parent
:PROPERTIES:
:ID: p1
:KIND: person
:END:
* Kid
:PROPERTIES:
:ID: k1
:KIND: person
:PARENTS: p1
:END:
"
    (let ((ents (org-chronicle--buffer-entities)))
      (should (equal (org-chronicle--children-names "p1" ents) '("Kid"))))))

(ert-deftest org-chronicle-test-filter-truth-and-range ()
  (org-chronicle-test--with-org org-chronicle-test--timeline
    (let* ((events (org-chronicle--buffer-events))
           (idx (make-hash-table :test #'equal))
           (out (org-chronicle--filter-events
                 events idx
                 :truth '("historical")
                 :from (org-chronicle--date-parse "1863-01-01")
                 :until (org-chronicle--date-parse "1863-12-31"))))
      (should (= (length out) 1))
      (should (equal (plist-get (car out) :title) "Vicksburg falls")))))

(ert-deftest org-chronicle-test-build-person-lane ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let* ((ents (org-chronicle--buffer-entities))
           (lane (org-chronicle--build-lane "Grant" 'people ents :collapse)))
      (should (equal (plist-get lane :label) "Ulysses S. Grant"))
      (should (member "Ulysses S. Grant" (plist-get lane :names))))))

(ert-deftest org-chronicle-test-build-group-lane-collapse-vs-expand ()
  (org-chronicle-test--with-org org-chronicle-test--entities
    (let* ((ents (org-chronicle--buffer-entities))
           (collapsed (org-chronicle--build-lane "Pinkerton Agency" 'people ents :collapse))
           (expanded (org-chronicle--build-lanes-for "Pinkerton Agency" 'people ents :expand)))
      (should (member "Ulysses S. Grant" (plist-get collapsed :names)))
      (should (= (length expanded) 1))
      (should (equal (plist-get (car expanded) :label) "Ulysses S. Grant")))))

(ert-deftest org-chronicle-test-event-in-lane ()
  (let ((lane (list :label "Grant" :domain 'people :names '("Ulysses S. Grant")))
        (event (list :people '("Ulysses S. Grant") :location nil)))
    (should (org-chronicle--event-in-lane-p event lane (make-hash-table :test #'equal)))))

(ert-deftest org-chronicle-test-event-in-topic-lane ()
  "An event lands in a topic lane when one of its topics matches the lane names."
  (let ((lane (list :label "shipping" :domain 'topic :names '("shipping")))
        (hit (list :topics '("crime" "shipping")))
        (miss (list :topics '("census"))))
    (should (org-chronicle--event-in-lane-p hit lane (make-hash-table :test #'equal)))
    (should-not (org-chronicle--event-in-lane-p miss lane (make-hash-table :test #'equal)))))

;;;; Render

(ert-deftest org-chronicle-test-truth-marker ()
  (should (equal (org-chronicle--truth-marker "historical") "[H]"))
  (should (equal (org-chronicle--truth-marker "fictionalized") "[~]"))
  (should (equal (org-chronicle--truth-marker "fictional") "[F]"))
  (should (equal (org-chronicle--truth-marker nil) "[?]")))

(ert-deftest org-chronicle-test-life-event-glyph ()
  (should (equal (org-chronicle--life-event-glyph "birth") "⊕"))
  (should (equal (org-chronicle--life-event-glyph "name-change") "↦"))
  (should (null (org-chronicle--life-event-glyph nil)))
  (should (null (org-chronicle--life-event-glyph "unknown"))))

(ert-deftest org-chronicle-test-cell-text-prepends-glyph ()
  (let ((birth (list :title "Birth of Grant" :truth "historical"
                     :life-event "birth" :marker nil))
        (plain (list :title "Vicksburg" :truth "historical" :marker nil)))
    (should (string-prefix-p "⊕ Birth of Grant" (org-chronicle--cell-text birth)))
    (should (string-prefix-p "Vicksburg [H]" (org-chronicle--cell-text plain)))))



(ert-deftest org-chronicle-test-render-columns ()
  (let* ((events
          (list (list :title "Vicksburg" :truth "historical"
                      :date (org-chronicle--date-parse "1863-07-04")
                      :people '("Grant") :location nil :marker nil)
                (list :title "Address" :truth "historical"
                      :date (org-chronicle--date-parse "1863-11-19")
                      :people '("Lincoln") :location nil :marker nil)))
         (lanes (list (list :label "Grant" :domain 'people :names '("Grant"))
                      (list :label "Lincoln" :domain 'people :names '("Lincoln"))))
         (idx (make-hash-table :test #'equal))
         (text (org-chronicle--render events lanes idx 20)))
    (should (string-match-p "Grant" text))
    (should (string-match-p "Lincoln" text))
    (should (string-match-p "1863-07-04" text))
    (should (string-match-p "Vicksburg \\[H\\]" text))
    (should (string-match-p "Address \\[H\\]" text))))

;;;; View

(ert-deftest org-chronicle-test-dblock-writer ()
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :title "Vicksburg" :truth "historical"
                                    :date (org-chronicle--date-parse "1863-07-04")
                                    :people '("Grant") :location nil :marker nil))))
            ((symbol-function 'org-chronicle--all-entities) (lambda () '())))
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: chronicle :people (\"Grant\")\n#+END:\n")
      (goto-char (point-min))
      (org-chronicle-dblock-write '(:people ("Grant")))
      (should (string-match-p "Vicksburg \\[H\\]" (buffer-string))))))

;;;; Capture

(ert-deftest org-chronicle-test-event-string ()
  (let ((s (org-chronicle--event-string
            :title "Vicksburg falls" :truth "historical"
            :date "1863-07-04" :people '("Grant" "Pemberton")
            :location "Vicksburg")))
    (should (string-match-p "^\\* Vicksburg falls" s))
    (should (string-match-p ":TRUTH:    historical" s))
    (should (string-match-p ":DATE:     <1863-07-04>" s))
    (should (string-match-p ":PEOPLE:   Grant; Pemberton" s))
    (should (string-match-p ":LOCATION: Vicksburg" s))))

(ert-deftest org-chronicle-test-event-string-topics ()
  "Topics are emitted as a semicolon-joined TOPICS property, omitted when empty."
  (let ((with (org-chronicle--event-string
               :title "Wreck" :truth "fictional" :date "1851-11-03"
               :topics '("shipping" "crime")))
        (without (org-chronicle--event-string
                  :title "Plain" :truth "historical" :date "1851-11-03")))
    (should (string-match-p ":TOPICS:   shipping; crime" with))
    (should-not (string-match-p ":TOPICS:" without))))

(ert-deftest org-chronicle-test-read-date-requires-value ()
  "A blank date prompt signals a `user-error'; a real value passes through."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should-error (org-chronicle--read-date "Date: ") :type 'user-error))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1863-07-04")))
    (should (equal (org-chronicle--read-date "Date: ") "1863-07-04"))))


(ert-deftest org-chronicle-test-life-event-title ()
  (should (equal (org-chronicle--life-event-title "birth" '("Grant") nil) "Birth of Grant"))
  (should (equal (org-chronicle--life-event-title "death" '("Grant") nil) "Death of Grant"))
  (should (equal (org-chronicle--life-event-title "marriage" '("Grant" "Julia Dent") nil)
                 "Marriage of Grant and Julia Dent"))
  (should (equal (org-chronicle--life-event-title "name-change" '("Mary Smith") "Mary Doe")
                 "Mary Smith becomes Mary Doe")))

(ert-deftest org-chronicle-test-life-event-string ()
  (let ((s (org-chronicle--life-event-string
            :title "Birth of Grant" :kind "birth" :truth "historical"
            :date "1822-04-27" :subject '("Ulysses S. Grant")
            :people '("Ulysses S. Grant" "Jesse Root Grant")
            :location "Point Pleasant, Ohio")))
    (should (string-match-p "^\\* Birth of Grant" s))
    (should (string-match-p ":LIFE-EVENT: birth" s))
    (should (string-match-p ":DATE:       <1822-04-27>" s))
    (should (string-match-p ":SUBJECT:    Ulysses S. Grant" s))
    (should (string-match-p ":PEOPLE:     Ulysses S. Grant; Jesse Root Grant" s))
    (should (string-match-p ":LOCATION:   Point Pleasant, Ohio" s))))



(ert-deftest org-chronicle-test-normalize-mirrors-tag ()
  (org-chronicle-test--with-org "\
* Some event
:PROPERTIES:
:TRUTH: fictional
:DATE:  <1864-07-12>
:END:
"
    (goto-char (point-min))
    (org-chronicle-normalize)
    (should (member "fictional" (org-get-tags)))))

(ert-deftest org-chronicle-test-life-event-tag ()
  (should (equal (org-chronicle--life-event-tag "birth") "birth"))
  (should (equal (org-chronicle--life-event-tag "name-change") "name_change"))
  (should (null (org-chronicle--life-event-tag nil))))

(ert-deftest org-chronicle-test-alias-list-with ()
  (should (equal (org-chronicle--alias-list-with '("Grant") "U.S. Grant" "Ulysses S. Grant")
                 '("Grant" "U.S. Grant")))
  (should (equal (org-chronicle--alias-list-with '("Grant") "Grant" "Ulysses S. Grant")
                 '("Grant")))
  (should (equal (org-chronicle--alias-list-with '("Grant") "Ulysses S. Grant" "Ulysses S. Grant")
                 '("Grant")))
  (should (equal (org-chronicle--alias-list-with nil "" "X") nil)))

(ert-deftest org-chronicle-test-normalize-mirrors-life-event-tag ()
  (let ((org-chronicle-entities-files nil))
    (org-chronicle-test--with-org "\
* Mary becomes Mary Doe
:PROPERTIES:
:TRUTH: fictional
:LIFE-EVENT: name-change
:DATE: <1850-06-01>
:SUBJECT: Mary Smith
:NEW-NAME: Mary Doe
:END:
"
      (goto-char (point-min))
      (org-chronicle-normalize)
      (should (member "fictional" (org-get-tags)))
      (should (member "name_change" (org-get-tags))))))




;;;; Entity creation

(ert-deftest org-chronicle-test-entity-string ()
  (let ((s (org-chronicle--entity-string
            :name "Ulysses S. Grant" :kind 'person
            :aliases '("Grant" "U.S. Grant")
            :props '(("BORN" . "<1822-04-27>") ("DIED" . "<1885-07-23>")))))
    (should (string-match-p "^\\* Ulysses S. Grant" s))
    (should (string-match-p ":KIND:    person" s))
    (should (string-match-p ":ALIASES: Grant; U.S. Grant" s))
    (should (string-match-p ":BORN:    <1822-04-27>" s))))

(ert-deftest org-chronicle-test-find-entity-by-name ()
  "Existing entities match by name or alias (case-insensitively); new names are nil."
  (let* ((entities (list (list :id "g" :name "Ulysses S. Grant" :kind 'person
                               :aliases '("Grant" "U.S. Grant"))))
         (idx (org-chronicle--alias-index entities)))
    (should (equal (plist-get (org-chronicle--find-entity-by-name "Ulysses S. Grant" entities idx) :id) "g"))
    (should (equal (plist-get (org-chronicle--find-entity-by-name "grant" entities idx) :id) "g"))
    (should (equal (plist-get (org-chronicle--find-entity-by-name "U.S. Grant" entities idx) :id) "g"))
    (should (null (org-chronicle--find-entity-by-name "Abraham Lincoln" entities idx)))))

(ert-deftest org-chronicle-test-group-id-for-name ()
  "A group name or alias resolves to the group id; non-groups and unknowns are nil."
  (let* ((entities (list (list :id "pink" :name "Pinkerton Agency" :kind 'group
                               :aliases '("Pinkertons"))
                         (list :id "grant" :name "Ulysses S. Grant" :kind 'person
                               :aliases nil)))
         (idx (org-chronicle--alias-index entities)))
    (should (equal (org-chronicle--group-id-for-name "Pinkerton Agency" entities idx) "pink"))
    (should (equal (org-chronicle--group-id-for-name "Pinkertons" entities idx) "pink"))
    (should (null (org-chronicle--group-id-for-name "Ulysses S. Grant" entities idx)))
    (should (null (org-chronicle--group-id-for-name "Nope" entities idx)))))

(defconst org-chronicle-test--life "\
* Birth of Grant
:PROPERTIES:
:LIFE-EVENT: birth
:DATE: <1822-04-27>
:SUBJECT: Ulysses S. Grant
:LOCATION: Point Pleasant, Ohio
:END:
* Death of Grant
:PROPERTIES:
:LIFE-EVENT: death
:DATE: <1885-07-23>
:SUBJECT: Ulysses S. Grant
:LOCATION: Mount McGregor, New York
:END:
* Marriage
:PROPERTIES:
:LIFE-EVENT: marriage
:DATE: <1848-08-22>
:SUBJECT: Ulysses S. Grant; Julia Dent
:END:
")

(ert-deftest org-chronicle-test-life-index ()
  (org-chronicle-test--with-org org-chronicle-test--life
    (let* ((events (org-chronicle--buffer-events))
           (idx (make-hash-table :test #'equal))
           (index (org-chronicle--life-index events idx))
           (grant (gethash "Ulysses S. Grant" index)))
      (should (equal (plist-get (car (plist-get grant :birth)) :year) 1822))
      (should (equal (cdr (plist-get grant :birth)) "Point Pleasant, Ohio"))
      (should (equal (plist-get (car (plist-get grant :death)) :year) 1885))
      (should (equal (plist-get grant :spouses) '("Julia Dent")))
      (should (equal (plist-get (gethash "Julia Dent" index) :spouses)
                     '("Ulysses S. Grant"))))))





;;;; Sources

(ert-deftest org-chronicle-test-source-link-format ()
  (should (equal (org-chronicle--source-link "abc123" "Foote, Civil War" "p.412")
                 "[[id:abc123][Foote, Civil War]] p.412"))
  (should (equal (org-chronicle--source-link "abc123" "Foote" nil)
                 "[[id:abc123][Foote]]")))

(ert-deftest org-chronicle-test-known-people ()
  "Known people come from event participants and person/group entities, not places."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :people '("Abraham Lincoln" "U.S. Grant")))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Ulysses S. Grant" :kind 'person :aliases '("Grant"))
                              (list :name "Pinkerton Agency" :kind 'group :aliases nil)
                              (list :name "Vicksburg" :kind 'place :aliases nil)))))
    (let ((people (org-chronicle--known-people)))
      (should (member "Abraham Lincoln" people))
      (should (member "U.S. Grant" people))
      (should (member "Ulysses S. Grant" people))
      (should (member "Grant" people))
      (should (member "Pinkerton Agency" people))
      (should-not (member "Vicksburg" people)))))

(ert-deftest org-chronicle-test-known-locations ()
  "Known locations come from event locations and place entities, not people."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :location "Vicksburg, Mississippi"))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Sultana" :kind 'place :aliases '("the Sultana"))
                              (list :name "Grant" :kind 'person :aliases nil)))))
    (let ((locs (org-chronicle--known-locations)))
      (should (member "Vicksburg, Mississippi" locs))
      (should (member "Sultana" locs))
      (should (member "the Sultana" locs))
      (should-not (member "Grant" locs)))))

(ert-deftest org-chronicle-test-file-events-missing-is-nil ()
  "Reading events from a non-existent file returns nil without prompting."
  (should (null (org-chronicle--file-events "/tmp/org-chronicle-nonexistent-xyz.org"))))

(ert-deftest org-chronicle-test-completion-table-metadata ()
  "The completion table reports its category and completes candidates."
  (let ((table (org-chronicle--completion-table '("alpha" "beta") 'org-chronicle-person)))
    (should (equal (funcall table "" nil 'metadata)
                   '(metadata (category . org-chronicle-person))))
    (should (member "alpha" (funcall table "" nil t)))))





(ert-deftest org-chronicle-test-add-source-free-text-when-no-reading-list ()
  (cl-letf (((symbol-function 'featurep)
             (lambda (f &rest _) (unless (eq f 'org-reading-list) t))))
    (should (equal (org-chronicle--read-source "NY Herald 1863") "NY Herald 1863"))))


;;;; Lint

(ert-deftest org-chronicle-test-lint-flags-out-of-span ()
  (let* ((entities
          (list (list :id "e1" :name "Sultana" :kind 'place
                      :aliases nil :member-of nil :part-of nil
                      :span-from (org-chronicle--date-parse "1863-01-03")
                      :span-to (org-chronicle--date-parse "1865-04-27"))))
         (idx (org-chronicle--alias-index entities))
         (index (make-hash-table :test #'equal))
         (bad (list :title "Meeting aboard Sultana" :truth "fictional"
                    :date (org-chronicle--date-parse "1866-01-01")
                    :people nil :location "Sultana" :marker nil))
         (ok (list :title "Earlier meeting" :truth "fictional"
                   :date (org-chronicle--date-parse "1864-01-01")
                   :people nil :location "Sultana" :marker nil)))
    (should (org-chronicle--event-anachronisms bad entities idx index))
    (should-not (org-chronicle--event-anachronisms ok entities idx index))))

(ert-deftest org-chronicle-test-span-prefers-life-events ()
  "Span comes from birth/death events when present, else from BORN/DIED."
  (let* ((entities (list (list :name "Grant" :kind 'person :aliases nil
                               :span-from (org-chronicle--date-parse "1800-01-01")
                               :span-to (org-chronicle--date-parse "1900-01-01"))))
         (idx (org-chronicle--alias-index entities))
         (index (make-hash-table :test #'equal)))
    (should (equal (plist-get (car (org-chronicle--span-for-name "Grant" entities idx index)) :year)
                   1800))
    (puthash "Grant" (list :birth (cons (org-chronicle--date-parse "1822-04-27") "Ohio")
                           :death (cons (org-chronicle--date-parse "1885-07-23") "New York"))
             index)
    (let ((span (org-chronicle--span-for-name "Grant" entities idx index)))
      (should (equal (plist-get (car span) :year) 1822))
      (should (equal (plist-get (cdr span) :year) 1885)))))






















(ert-deftest org-chronicle-test-read-source-unmatched-is-free-text ()
  "An unlisted pick is returned as free text, not a [[id:nil]] link."
  (cl-letf (((symbol-function 'featurep) (lambda (f &rest _) (eq f 'org-reading-list)))
            ((symbol-function 'org-reading-list-entries) (lambda () '(("Foote" . "id1"))))
            ((symbol-function 'completing-read) (lambda (&rest _) "Unlisted clipping")))
    (should (equal (org-chronicle--read-source) "Unlisted clipping"))))

(provide 'org-chronicle-tests)
;;; org-chronicle-tests.el ends here
