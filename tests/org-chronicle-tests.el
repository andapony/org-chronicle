;;; org-chronicle-tests.el --- Tests for org-chronicle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle)
(require 'cl-lib)

(defvar org-chronicle-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the test file, captured at load time.")

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

(ert-deftest org-chronicle-test-expand-all ()
  "The all-lanes token expands to every candidate; other input passes through."
  (let ((cands '("Ada" "Bram" "Cleo")))
    (should (equal (org-chronicle--expand-all
                    (list org-chronicle--all-lanes-token) cands)
                   cands))
    (should (equal (org-chronicle--expand-all '("Bram") cands) '("Bram")))
    (should (null (org-chronicle--expand-all nil cands)))
    (should (equal (org-chronicle--expand-all
                    (list "Bram" org-chronicle--all-lanes-token) cands)
                   cands))))

(ert-deftest org-chronicle-test-read-names-all ()
  "ALLOW-ALL expands the all token to every candidate; otherwise it is literal."
  (let ((cands '("Ada" "Bram")))
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (&rest _) (list org-chronicle--all-lanes-token))))
      (should (equal (org-chronicle--read-names "P: " cands nil t) cands))
      (should (equal (org-chronicle--read-names "P: " cands nil)
                     (list org-chronicle--all-lanes-token))))))

(ert-deftest org-chronicle-test-render-drops-empty-rows ()
  "Render omits date rows where no selected lane has any content."
  (let* ((idx (org-chronicle--alias-index nil))
         (e1 (list :title "Heist" :truth "historical"
                   :date (org-chronicle--date-parse "1850") :people '("Ada")))
         (e2 (list :title "Cargo" :truth "historical"
                   :date (org-chronicle--date-parse "1851") :people '("Bob")))
         (lane (list :domain 'people :names '("Ada") :label "Ada"))
         (out (org-chronicle--render (list e1 e2) (list lane) idx 22)))
    (should (string-match-p "^1850" out))
    (should (string-match-p "Heist" out))
    (should-not (string-match-p "^1851" out))))

(ert-deftest org-chronicle-test-read-truth ()
  "Truth read returns nil (all) for blank or \"all\", else the chosen subset."
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _) (list org-chronicle--all-lanes-token))))
    (should (null (org-chronicle--read-truth))))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _) (list ""))))
    (should (null (org-chronicle--read-truth))))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _) (list "historical" "fictional"))))
    (should (equal (org-chronicle--read-truth) '("historical" "fictional")))))

(ert-deftest org-chronicle-test-event-in-lane-subject ()
  "A person lane matches a life event where the person is its SUBJECT.
Imported life events carry the principal in :subject, not :people."
  (let* ((idx (org-chronicle--alias-index nil))
         (birth (list :subject '("James King of William") :life-event "birth"
                      :date (org-chronicle--date-parse "1822-01-28")))
         (other (list :people '("Someone Else")
                      :date (org-chronicle--date-parse "1850")))
         (lane (list :domain 'people :names '("James King of William") :label "JK")))
    (should (org-chronicle--event-in-lane-p birth lane idx))
    (should-not (org-chronicle--event-in-lane-p other lane idx))))






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

(ert-deftest org-chronicle-test-date-lower-bound ()
  "Lower bound expands a coarse date to its earliest instant."
  (let ((y (org-chronicle--date-lower-bound (org-chronicle--date-parse "1880"))))
    (should (equal (plist-get y :year) 1880))
    (should (equal (plist-get y :month) 1))
    (should (equal (plist-get y :day) 1)))
  (let ((m (org-chronicle--date-lower-bound (org-chronicle--date-parse "1880-06"))))
    (should (equal (plist-get m :day) 1)))
  (let ((d (org-chronicle--date-lower-bound (org-chronicle--date-parse "1880-06-15"))))
    (should (equal (plist-get d :day) 15))))

(ert-deftest org-chronicle-test-date-upper-bound ()
  "Upper bound expands a coarse date to its latest instant."
  (let ((y (org-chronicle--date-upper-bound (org-chronicle--date-parse "1880"))))
    (should (equal (plist-get y :month) 12))
    (should (equal (plist-get y :day) 31)))
  (let ((m (org-chronicle--date-upper-bound (org-chronicle--date-parse "1881-02"))))
    (should (equal (plist-get m :day) 28)))
  (let ((d (org-chronicle--date-upper-bound (org-chronicle--date-parse "1880-06-15"))))
    (should (equal (plist-get d :day) 15))))

(ert-deftest org-chronicle-test-date-ordinal-monotonic ()
  "Ordinals respect chronological order and bound expansion."
  (let ((lo (org-chronicle--date-ordinal
             (org-chronicle--date-lower-bound (org-chronicle--date-parse "1880"))))
        (hi (org-chronicle--date-ordinal
             (org-chronicle--date-upper-bound (org-chronicle--date-parse "1880")))))
    (should (< lo hi))
    (should (= (- hi lo) 365))))

;;;; Store

(defmacro org-chronicle-test--with-org (text &rest body)
  "Run BODY in a temp Org buffer containing TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro org-chronicle-test--with-root (files &rest body)
  "Run BODY with `org-chronicle-root' set to a temp dir populated with FILES.
FILES evaluates to a list of (RELPATH . CONTENT) conses; intermediate
directories are created.  The temp dir is removed afterward."
  (declare (indent 1))
  `(let ((org-chronicle-root (make-temp-file "octest" t)))
     (unwind-protect
         (progn
           (dolist (f ,files)
             (let ((path (expand-file-name (car f) org-chronicle-root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr f)))))
           ,@body)
       (delete-directory org-chronicle-root t))))

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

(ert-deftest org-chronicle-test-source-files ()
  "Source files are *.org under the root, recursively, minus excluded paths."
  (org-chronicle-test--with-root
      '(("timeline.org" . "* e\n")
        ("chapters/ch1.org" . "* e\n")
        ("notes.txt" . "x")
        ("drafts/d1.org" . "* e\n"))
    (let* ((org-chronicle-exclude '("/drafts/"))
           (rels (mapcar (lambda (f) (file-relative-name f org-chronicle-root))
                         (org-chronicle--source-files))))
      (should (member "timeline.org" rels))
      (should (member "chapters/ch1.org" rels))
      (should-not (member "notes.txt" rels))
      (should-not (member "drafts/d1.org" rels)))))

(ert-deftest org-chronicle-test-source-files-skips-lock-files ()
  "`org-chronicle--source-files' ignores Emacs lock files (.#*.org)."
  (let* ((dir (make-temp-file "ocsf" t))
         (org-chronicle-root dir)
         (org-chronicle-exclude nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "people.org" dir) (insert "* X\n"))
          (with-temp-file (expand-file-name ".#people.org" dir) (insert "lock"))
          (let ((files (mapcar #'file-name-nondirectory (org-chronicle--source-files))))
            (should (member "people.org" files))
            (should-not (member ".#people.org" files))))
      (delete-directory dir t))))

(ert-deftest org-chronicle-test-source-files-missing-root ()
  "A nonexistent root yields nil, not an error."
  (let ((org-chronicle-root "/no/such/dir/xyzzy"))
    (should (null (org-chronicle--source-files)))))

(ert-deftest org-chronicle-test-root-exclude-safe-local ()
  "Root and exclude are marked safe-local-variable for dir-locals."
  (should (eq (get 'org-chronicle-root 'safe-local-variable) #'stringp))
  (should (functionp (get 'org-chronicle-exclude 'safe-local-variable))))

(ert-deftest org-chronicle-test-file-ignored-p ()
  "A buffer with `#+CHRONICLE: ignore' is ignored; otherwise it is not."
  (org-chronicle-test--with-org "#+CHRONICLE: ignore\n* e\n"
    (should (org-chronicle--file-ignored-p)))
  (org-chronicle-test--with-org "#+TITLE: x\n* e\n"
    (should-not (org-chronicle--file-ignored-p))))

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
  (should (equal (org-chronicle--truth-marker "historical") "H"))
  (should (equal (org-chronicle--truth-marker "fictionalized") "~"))
  (should (equal (org-chronicle--truth-marker "fictional") "F"))
  (should (equal (org-chronicle--truth-marker nil) "?")))

(ert-deftest org-chronicle-test-life-event-glyph ()
  (should (equal (org-chronicle--life-event-glyph "birth") "⊕"))
  (should (equal (org-chronicle--life-event-glyph "name-change") "↦"))
  (should (null (org-chronicle--life-event-glyph nil)))
  (should (null (org-chronicle--life-event-glyph "unknown"))))

(ert-deftest org-chronicle-test-cell-text-prepends-glyph ()
  (let ((birth (list :title "Birth of Grant" :truth "historical"
                     :life-event "birth" :marker nil))
        (plain (list :title "Vicksburg" :truth "historical" :marker nil)))
    (should (string-prefix-p "H ⊕ Birth of Grant" (org-chronicle--cell-text birth)))
    (should (string-prefix-p "H Vicksburg" (org-chronicle--cell-text plain)))))

(ert-deftest org-chronicle-test-topic-cell-text ()
  "In a topic lane a cell shows the topic glyph and uses the topic face."
  (let ((org-chronicle-topic-glyphs '(("shipping" . "⚓")))
        (org-chronicle-topic-faces '(("shipping" . org-chronicle-fictional)))
        (lane (list :label "shipping" :domain 'topic :names '("shipping")))
        (people-lane (list :label "Grant" :domain 'people :names '("Grant")))
        (event (list :title "Wreck" :truth "fictional" :marker nil)))
    (let ((cell (org-chronicle--cell-text-for-lane event lane)))
      (should (string-prefix-p "F ⚓ Wreck" cell))
      (should (eq (get-text-property 0 'face cell) 'org-chronicle-fictional)))
    ;; A non-topic lane is unchanged: no glyph, truth face.
    (should (string-prefix-p "F Wreck"
                             (org-chronicle--cell-text-for-lane event people-lane)))))

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
    (should (string-match-p "H Vicksburg" text))
    (should (string-match-p "H Address" text))))

(ert-deftest org-chronicle-test-lane-width ()
  "Lane width splits the post-date space among lanes, floored at the minimum."
  (let ((org-chronicle-lane-column-width 22))
    ;; 100 - 12 (date column) = 88, split two ways = 44.
    (should (= (org-chronicle--lane-width 100 2) 44))
    ;; 88 split eight ways = 11, floored at the 22 minimum.
    (should (= (org-chronicle--lane-width 100 8) 22))
    ;; No lanes: fall back to the minimum.
    (should (= (org-chronicle--lane-width 100 0) 22))))

(ert-deftest org-chronicle-test-compose-width-divides-lanes ()
  "Compose divides a given :width among the lanes; omitting it keeps the floor."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :title "Wreck" :truth "historical"
                                    :date (org-chronicle--date-parse "1863-07-04")
                                    :people '("Grant") :location nil :marker nil))))
            ((symbol-function 'org-chronicle--all-entities) (lambda () '()))
            (org-chronicle-lane-column-width 22))
    ;; One lane in 200 columns: 200 - 12 = 188-wide lane, 200-wide header.
    (let ((header (car (split-string
                        (org-chronicle--compose :people '("Grant") :width 200)
                        "\n"))))
      (should (= (string-width header) 200)))
    ;; No :width: each lane stays at the configured floor (12 + 22 = 34).
    (let ((header (car (split-string
                        (org-chronicle--compose :people '("Grant"))
                        "\n"))))
      (should (= (string-width header) 34)))))

(ert-deftest org-chronicle-test-entity-link-segments ()
  "Resolving names yield (BEG END NAME ID); unpromoted names are skipped."
  (let* ((entities (list (list :name "Ulysses S. Grant" :kind 'person
                               :aliases '("Grant") :id "ent-grant")
                         (list :name "Abraham Lincoln" :kind 'person
                               :aliases nil :id "ent-lincoln")))
         (idx (org-chronicle--alias-index entities))
         (org-chronicle-multi-value-separator "; "))
    ;; "Grant" is an alias of ent-grant; "Eliza Harlan" is unpromoted (skipped).
    (should (equal (org-chronicle--entity-link-segments
                    "Grant; Eliza Harlan; Abraham Lincoln" entities idx)
                   '((0 5 "Grant" "ent-grant")
                     (21 36 "Abraham Lincoln" "ent-lincoln"))))
    ;; Nothing resolves -> empty.
    (should (null (org-chronicle--entity-link-segments "Nobody Here" entities idx)))))

(ert-deftest org-chronicle-test-entity-cache-invalidation ()
  "The entity cache builds lazily and invalidation clears it."
  (cl-letf (((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Grant" :kind 'person
                                    :aliases nil :id "ent-grant")))))
    (let ((org-chronicle--entity-cache nil)
          (org-chronicle--entity-link-buffers nil))
      (let ((cache (org-chronicle--entity-cache)))
        (should (equal (car cache)
                       (list (list :name "Grant" :kind 'person
                                   :aliases nil :id "ent-grant"))))
        (should (hash-table-p (cdr cache))))
      (should org-chronicle--entity-cache)
      (org-chronicle--invalidate-entity-cache)
      (should-not org-chronicle--entity-cache))))

(ert-deftest org-chronicle-test-file-under-root-p ()
  "Files inside `org-chronicle-root' are recognized; outside and nil are not."
  (let ((org-chronicle-root "/home/u/chron/"))
    (should (org-chronicle--file-under-root-p "/home/u/chron/people.org"))
    (should (org-chronicle--file-under-root-p "/home/u/chron/sub/x.org"))
    (should-not (org-chronicle--file-under-root-p "/home/u/other/x.org"))
    (should-not (org-chronicle--file-under-root-p nil))))

(ert-deftest org-chronicle-test-entity-links-mode-buttonizes ()
  "Enabling the mode buttonizes resolving names and leaves others plain."
  (cl-letf (((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Ulysses S. Grant" :kind 'person
                                    :aliases '("Grant") :id "ent-grant")))))
    (let ((org-chronicle--entity-cache nil)
          (org-chronicle--entity-link-buffers nil)
          (org-chronicle-multi-value-separator "; ")
          (visited nil))
      (with-temp-buffer
        (org-mode)
        (insert "* Some event\n:PROPERTIES:\n:PEOPLE:   Grant; Eliza Harlan\n:END:\n")
        (org-chronicle-entity-links-mode 1)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "Grant")
        (let ((p (match-beginning 0)))
          (should (equal (get-text-property p 'org-chronicle-entity-id) "ent-grant"))
          (should (eq (get-text-property p 'face) 'org-chronicle-entity-link)))
        (goto-char (point-min))
        (search-forward "Eliza")
        (should-not (get-text-property (match-beginning 0) 'org-chronicle-entity-id))
        (cl-letf (((symbol-function 'org-id-goto)
                   (lambda (id) (setq visited id))))
          (goto-char (point-min))
          (search-forward "Grant")
          (goto-char (match-beginning 0))
          (org-chronicle-visit-entity-at-point))
        (should (equal visited "ent-grant"))))))

(ert-deftest org-chronicle-test-entity-links-buttonizes-vitals ()
  "BIRTHPLACE/DEATHPLACE values buttonize like event property values."
  (cl-letf (((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Saco, Maine, United States" :kind 'place
                                    :aliases nil :id "ent-saco")))))
    (let ((org-chronicle--entity-cache nil)
          (org-chronicle--entity-link-buffers nil))
      (with-temp-buffer
        (org-mode)
        (insert "* Someone\n:PROPERTIES:\n:BIRTHPLACE: Saco, Maine, United States\n:END:\n")
        (org-chronicle-entity-links-mode 1)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward "Saco")
        (should (equal (get-text-property (match-beginning 0) 'org-chronicle-entity-id)
                       "ent-saco"))))))

(ert-deftest org-chronicle-test-entity-links-refresh ()
  "The refresh command clears the entity cache so it rebuilds from disk."
  (let ((org-chronicle--entity-cache (cons 'stale (make-hash-table :test #'equal)))
        (org-chronicle--entity-link-buffers nil))
    (org-chronicle-entity-links-refresh)
    (should-not org-chronicle--entity-cache)))

(ert-deftest org-chronicle-test-turn-on-entity-links ()
  "Globalized turn-on enables the mode only in org buffers under the root."
  (let ((org-chronicle-root "/home/u/chron/")
        (org-chronicle--entity-link-buffers nil))
    ;; org-mode file under root -> enabled.
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/home/u/chron/timeline.org")
      (org-chronicle--turn-on-entity-links)
      (should org-chronicle-entity-links-mode))
    ;; org-mode file outside root -> not enabled.
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/home/u/other/x.org")
      (org-chronicle--turn-on-entity-links)
      (should-not org-chronicle-entity-links-mode))
    ;; non-org file under root -> not enabled.
    (with-temp-buffer
      (fundamental-mode)
      (setq buffer-file-name "/home/u/chron/notes.txt")
      (org-chronicle--turn-on-entity-links)
      (should-not org-chronicle-entity-links-mode))))

(ert-deftest org-chronicle-test-history-record ()
  "Recording follows builds a trail; a new follow truncates forward history."
  (let ((org-chronicle--history nil)
        (org-chronicle--history-position -1)
        (a (list :file "a" :pos 1))
        (b (list :file "b" :pos 1))
        (c (list :file "c" :pos 1)))
    (org-chronicle--history-record a b)
    (should (equal org-chronicle--history (list a b)))
    (should (= org-chronicle--history-position 1))
    (org-chronicle--history-record b c)
    (should (equal org-chronicle--history (list a b c)))
    (should (= org-chronicle--history-position 2))
    (should (equal (org-chronicle--history-go -1) b))
    (should (= org-chronicle--history-position 1))
    ;; New follow from b truncates the forward entry (c) before appending.
    (org-chronicle--history-record b a)
    (should (equal org-chronicle--history (list a b a)))
    (should (= org-chronicle--history-position 2))))

(ert-deftest org-chronicle-test-history-go-bounds ()
  "Moving past either end of the history signals a `user-error'."
  (let ((org-chronicle--history (list (list :file "a" :pos 1) (list :file "b" :pos 1)))
        (org-chronicle--history-position 0))
    (should (equal (org-chronicle--history-go 1) (list :file "b" :pos 1)))
    (should-error (org-chronicle--history-go 1) :type 'user-error)
    (should (equal (org-chronicle--history-go -1) (list :file "a" :pos 1)))
    (should-error (org-chronicle--history-go -1) :type 'user-error)))

(ert-deftest org-chronicle-test-history-back-forward-visits ()
  "Back and forward switch to the recorded buffer and point."
  (let ((org-chronicle--history nil)
        (org-chronicle--history-position -1)
        (bufA (generate-new-buffer "tA"))
        (bufB (generate-new-buffer "tB"))
        locA locB)
    (with-current-buffer bufA (insert "hello world") (goto-char 3)
                         (setq locA (org-chronicle--history-location)))
    (with-current-buffer bufB (insert "another buf") (goto-char 5)
                         (setq locB (org-chronicle--history-location)))
    (org-chronicle--history-record locA locB)
    (org-chronicle-history-back)
    (should (eq (current-buffer) bufA))
    (should (= (point) 3))
    (org-chronicle-history-forward)
    (should (eq (current-buffer) bufB))
    (should (= (point) 5))
    (kill-buffer bufA)
    (kill-buffer bufB)))

(ert-deftest org-chronicle-test-visit-entity-records-history ()
  "Following an entity button records origin and target in the history."
  (let ((org-chronicle--history nil)
        (org-chronicle--history-position -1)
        (target-buf (generate-new-buffer "target")))
    (with-current-buffer target-buf (insert "entity heading") (goto-char 1))
    (cl-letf (((symbol-function 'org-id-goto)
               (lambda (_id) (switch-to-buffer target-buf) (goto-char 4))))
      (with-temp-buffer
        (insert "name")
        (put-text-property (point-min) (point-max) 'org-chronicle-entity-id "ent-x")
        (goto-char 2)
        (org-chronicle-visit-entity-at-point)
        (should (eq (current-buffer) target-buf))
        (should (= (point) 4))
        (should (= (length org-chronicle--history) 2))
        (should (= org-chronicle--history-position 1))
        (should (eq (marker-buffer
                     (plist-get (nth 1 org-chronicle--history) :marker))
                    target-buf))))
    (kill-buffer target-buf)))

(ert-deftest org-chronicle-test-history-keybindings ()
  "Back/forward keys are bound in the entity-links and view keymaps."
  (should (eq (lookup-key org-chronicle-entity-links-mode-map (kbd "C-c <left>"))
              #'org-chronicle-history-back))
  (should (eq (lookup-key org-chronicle-entity-links-mode-map (kbd "C-c <right>"))
              #'org-chronicle-history-forward))
  (should (eq (lookup-key org-chronicle-entity-links-mode-map [mouse-8])
              #'org-chronicle-history-back))
  (should (eq (lookup-key org-chronicle-view-mode-map "l")
              #'org-chronicle-history-back))
  (should (eq (lookup-key org-chronicle-view-mode-map "r")
              #'org-chronicle-history-forward)))

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
      (should (string-match-p "H Vicksburg" (buffer-string))))))

(ert-deftest org-chronicle-test-compose-topic-column ()
  "Compose builds a topic column holding events that carry the topic."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :title "Wreck" :truth "fictional"
                                    :date (org-chronicle--date-parse "1851-11-03")
                                    :people nil :location nil
                                    :topics '("shipping") :marker nil))))
            ((symbol-function 'org-chronicle--all-entities) (lambda () '())))
    (let ((text (org-chronicle--compose :topics '("shipping"))))
      (should (string-match-p "SHIPPING" text))
      (should (string-match-p "F Wreck" text)))))

(ert-deftest org-chronicle-test-compose-root-override ()
  "Compose honors an explicit :root override, gathering from it even when the
global root differs."
  (org-chronicle-test--with-root
   '(("timeline.org" . "* Wreck\n:PROPERTIES:\n:TRUTH: fictional\n:DATE: <1851-11-03>\n:PEOPLE: Grant\n:END:\n"))
   (let ((captured-root org-chronicle-root))
     ;; Point the global root elsewhere; the override must still win.
     (let ((org-chronicle-root "/no/such/dir/xyzzy"))
       (let ((text (org-chronicle--compose :people '("Grant") :root captured-root)))
         (should (string-match-p "F Wreck" text)))))))

(ert-deftest org-chronicle-test-dblock-topics ()
  "The chronicle dynamic block accepts a :topics parameter."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :title "Wreck" :truth "fictional"
                                    :date (org-chronicle--date-parse "1851-11-03")
                                    :people nil :location nil
                                    :topics '("shipping") :marker nil))))
            ((symbol-function 'org-chronicle--all-entities) (lambda () '())))
    (with-temp-buffer
      (org-mode)
      (org-chronicle-dblock-write '(:topics ("shipping")))
      (should (string-match-p "F Wreck" (buffer-string))))))

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

(ert-deftest org-chronicle-test-add-event-includes-topics ()
  "`org-chronicle-add-event' threads read topics into the appended heading."
  (let (appended)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'org-chronicle--read-date) (lambda (&rest _) "1851-11-03"))
              ((symbol-function 'completing-read) (lambda (&rest _) "fictional"))
              ((symbol-function 'org-chronicle--read-people) (lambda () nil))
              ((symbol-function 'org-chronicle--read-location) (lambda () nil))
              ((symbol-function 'org-chronicle--read-topics) (lambda (&rest _) '("shipping")))
              ((symbol-function 'org-chronicle--append-event)
               (lambda (text) (setq appended text))))
      (org-chronicle-add-event)
      (should (string-match-p ":TOPICS:   shipping" appended)))))

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
  (cl-letf (((symbol-function 'org-chronicle--all-entities) (lambda () '())))
    (org-chronicle-test--with-org "\
* Some event
:PROPERTIES:
:TRUTH: fictional
:DATE:  <1864-07-12>
:END:
"
				  (goto-char (point-min))
				  (org-chronicle-normalize)
				  (should (member "fictional" (org-get-tags))))))

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
  (cl-letf (((symbol-function 'org-chronicle--all-entities) (lambda () '())))
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

(ert-deftest org-chronicle-test-normalize-canonicalizes-topics ()
  "Normalize rewrites TOPICS to canonical names via the alias index."
  (cl-letf (((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :id "t-ship" :name "Maritime shipping" :kind 'topic
                                    :aliases '("shipping"))))))
    (org-chronicle-test--with-org "\
* Wreck
:PROPERTIES:
:TRUTH: fictional
:DATE: <1851-11-03>
:TOPICS: shipping
:END:
"
      (goto-char (point-min))
      (org-chronicle-normalize)
      (should (equal (org-entry-get nil "TOPICS") "Maritime shipping"))
      ;; Topics are NOT mirrored to an Org tag.
      (should-not (member "shipping" (org-get-tags)))
      (should-not (member "Maritime_shipping" (org-get-tags))))))

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

(ert-deftest org-chronicle-test-add-topic ()
  "`org-chronicle-add-topic' files a KIND: topic entity with aliases and description."
  (let (filed-file filed-text)
    (cl-letf (((symbol-function 'org-chronicle--all-entities) (lambda () '()))
              ((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("shipping")))
              ((symbol-function 'read-string) (lambda (&rest _) "Trade and wrecks."))
              ((symbol-function 'org-chronicle--file-entity)
               (lambda (file text) (setq filed-file file filed-text text) "topic-id")))
      (let ((org-chronicle-topics-file "/tmp/topics.org"))
        (org-chronicle-add-topic "Maritime shipping"))
      (should (equal filed-file "/tmp/topics.org"))
      (should (string-match-p "^\\* Maritime shipping" filed-text))
      (should (string-match-p ":KIND:    topic" filed-text))
      (should (string-match-p ":ALIASES: shipping" filed-text))
      (should (string-match-p ":DESCRIPTION: Trade and wrecks." filed-text)))))

(ert-deftest org-chronicle-test-write-target-accessors ()
  "Write-target accessors resolve under the root when nil, else use the set value."
  (let ((org-chronicle-root "/tmp/chron")
        (org-chronicle-timeline-file nil)
        (org-chronicle-people-file nil)
        (org-chronicle-places-file nil)
        (org-chronicle-topics-file nil))
    (should (equal (org-chronicle--timeline-file) (expand-file-name "timeline.org" "/tmp/chron")))
    (should (equal (org-chronicle--people-file) (expand-file-name "people.org" "/tmp/chron")))
    (should (equal (org-chronicle--places-file) (expand-file-name "places.org" "/tmp/chron")))
    (should (equal (org-chronicle--topics-file) (expand-file-name "topics.org" "/tmp/chron"))))
  (let ((org-chronicle-timeline-file "~/explicit/tl.org")
        (org-chronicle-people-file "~/explicit/pe.org")
        (org-chronicle-places-file "~/explicit/pl.org")
        (org-chronicle-topics-file "~/explicit/to.org"))
    (should (equal (org-chronicle--timeline-file) "~/explicit/tl.org"))
    (should (equal (org-chronicle--people-file) "~/explicit/pe.org"))
    (should (equal (org-chronicle--places-file) "~/explicit/pl.org"))
    (should (equal (org-chronicle--topics-file) "~/explicit/to.org"))))

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
  "Known people present only preferred names; aliases fold to the entity name."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :people '("Abraham Lincoln" "U.S. Grant" "Grant")))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Ulysses S. Grant" :kind 'person :aliases '("Grant"))
                              (list :name "Pinkerton Agency" :kind 'group :aliases nil)
                              (list :name "Vicksburg" :kind 'place :aliases nil)))))
    (let ((people (org-chronicle--known-people)))
      (should (member "Abraham Lincoln" people))
      (should (member "U.S. Grant" people))
      (should (member "Ulysses S. Grant" people))
      (should (member "Pinkerton Agency" people))
      ;; The alias "Grant" (entity- and event-supplied) folds into the
      ;; preferred name, and places never appear among people.
      (should-not (member "Grant" people))
      (should-not (member "Vicksburg" people)))))

(ert-deftest org-chronicle-test-known-locations ()
  "Known locations present only preferred names; aliases fold to the place name."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :location "Vicksburg, Mississippi"))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Sultana" :kind 'place :aliases '("the Sultana"))
                              (list :name "Grant" :kind 'person :aliases nil)))))
    (let ((locs (org-chronicle--known-locations)))
      (should (member "Vicksburg, Mississippi" locs))
      (should (member "Sultana" locs))
      ;; The alias "the Sultana" folds into the preferred name.
      (should-not (member "the Sultana" locs))
      (should-not (member "Grant" locs)))))

(ert-deftest org-chronicle-test-known-topics ()
  "Known topics come from event TOPICS values and topic entities, not people."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () (list (list :topics '("crime" "shipping")))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () (list (list :name "Maritime shipping" :kind 'topic
                                    :aliases '("shipping"))
                              (list :name "Grant" :kind 'person :aliases nil)))))
    (let ((topics (org-chronicle--known-topics)))
      (should (member "crime" topics))
      (should (member "shipping" topics))
      (should (member "Maritime shipping" topics))
      (should-not (member "Grant" topics)))))

(ert-deftest org-chronicle-test-file-events-missing-is-nil ()
  "Reading events from a non-existent file returns nil without prompting."
  (should (null (org-chronicle--file-events "/tmp/org-chronicle-nonexistent-xyz.org"))))

(ert-deftest org-chronicle-test-all-events-from-root ()
  "Events are gathered from every source file, including subdirectories."
  (org-chronicle-test--with-root
      '(("timeline.org" . "* Vicksburg\n:PROPERTIES:\n:DATE: <1863-07-04>\n:END:\n")
        ("chapters/ch1.org" . "* Scene\n:PROPERTIES:\n:DATE: <1863-11-19>\n:END:\n")
        ("ignored.org" . "#+CHRONICLE: ignore\n* Nope\n:PROPERTIES:\n:DATE: <1900-01-01>\n:END:\n"))
    (let ((titles (mapcar (lambda (e) (plist-get e :title))
                          (org-chronicle--all-events))))
      (should (member "Vicksburg" titles))
      (should (member "Scene" titles))
      (should-not (member "Nope" titles)))))

(ert-deftest org-chronicle-test-all-entities-from-root ()
  "Entities are gathered from every source file under the root."
  (org-chronicle-test--with-root
   '(("people.org" . "* Grant\n:PROPERTIES:\n:KIND: person\n:END:\n")
     ("places/p.org" . "* Vicksburg\n:PROPERTIES:\n:KIND: place\n:END:\n")
     ("ignored.org" . "#+CHRONICLE: ignore\n* Ghost\n:PROPERTIES:\n:KIND: person\n:END:\n"))
   (let ((names (mapcar (lambda (e) (plist-get e :name))
                        (org-chronicle--all-entities))))
     (should (member "Grant" names))
     (should (member "Vicksburg" names))
     (should-not (member "Ghost" names)))))

(ert-deftest org-chronicle-test-accrue-alias-across-root ()
  "Alias accrual finds and edits the subject entity by ID anywhere under the root."
  (org-chronicle-test--with-root
   '(("people/grant.org" . "* Ulysses S. Grant\n:PROPERTIES:\n:ID: ent-grant\n:KIND: person\n:END:\n"))
   (org-chronicle--accrue-alias "Ulysses S. Grant" "Sam Grant")
   (let ((ent (cl-find "Ulysses S. Grant" (org-chronicle--all-entities)
                       :key (lambda (e) (plist-get e :name)) :test #'equal)))
     (should ent)
     (should (member "Sam Grant" (plist-get ent :aliases))))))

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

(ert-deftest org-chronicle-test-link-export-uses-description ()
  "A chronicle: link exports as its description text."
  (should (equal (org-chronicle--link-export "any-id" "Mrs. Grant" 'html)
                 "Mrs. Grant")))

(ert-deftest org-chronicle-test-link-export-falls-back-to-path ()
  "With no description the export falls back to the raw path."
  (cl-letf (((symbol-function 'org-chronicle--reference-title) (lambda (_) nil)))
    (should (equal (org-chronicle--link-export "evt-x" nil 'html) "evt-x"))))

(ert-deftest org-chronicle-test-link-registered ()
  "The chronicle link type is registered with org."
  (should (org-link-get-parameter "chronicle" :follow)))

(ert-deftest org-chronicle-test-link-export-falls-back-to-title ()
  "With no description the export uses the target's heading title."
  (cl-letf (((symbol-function 'org-chronicle--reference-title)
             (lambda (_) "Ulysses S. Grant")))
    (should (equal (org-chronicle--link-export "grant-id" nil 'html)
                   "Ulysses S. Grant"))))

(ert-deftest org-chronicle-test-reference-title ()
  "A reference title resolves among events and entities, else nil."
  (cl-letf (((symbol-function 'org-chronicle--all-events)
             (lambda () '((:id "e1" :title "Vicksburg"))))
            ((symbol-function 'org-chronicle--all-entities)
             (lambda () '((:id "p1" :name "Eliza Dent")))))
    (should (equal (org-chronicle--reference-title "p1") "Eliza Dent"))
    (should (equal (org-chronicle--reference-title "e1") "Vicksburg"))
    (should (null (org-chronicle--reference-title "missing")))))

(ert-deftest org-chronicle-test-extract-ids-from-links ()
  "Ids are pulled out of id: links in a property value."
  (should (equal (org-chronicle--extract-ids "[[id:abc]]; [[id:def]]")
                 '("abc" "def"))))

(ert-deftest org-chronicle-test-extract-ids-bare ()
  "A bare (unlinked) id value is accepted."
  (should (equal (org-chronicle--extract-ids "abc") '("abc")))
  (should (equal (org-chronicle--extract-ids nil) nil))
  (should (equal (org-chronicle--extract-ids "  ") nil)))

(ert-deftest org-chronicle-test-scan-references ()
  "Inline chronicle: links are scanned with id and description."
  (org-chronicle-test--with-org
      "Prose [[chronicle:eliza][Mrs. Grant]] then [[chronicle:fort-wade]] end.\n"
    (let ((refs (org-chronicle--scan-references)))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) refs)
                     '("eliza" "fort-wade")))
      (should (equal (plist-get (car refs) :name) "Mrs. Grant"))
      (should (null (plist-get (cadr refs) :name))))))

(ert-deftest org-chronicle-test-buffer-scenes-detects-triggers ()
  "Headings with EVENT/AFTER/BEFORE, a fictional DATE, or an inline link are scenes."
  (org-chronicle-test--with-org
      (concat "* Plain\n"
              "* Has event\n:PROPERTIES:\n:EVENT: [[id:e1]]\n:END:\nbody\n"
              "* Fictional\n:PROPERTIES:\n:DATE: <1863-06-01>\n:TRUTH: fictional\n:END:\n"
              "* Linked\nprose [[chronicle:x][X]] more\n")
    (let ((titles (mapcar (lambda (s) (plist-get s :title))
                          (org-chronicle--buffer-scenes))))
      (should (equal titles '("Has event" "Fictional" "Linked")))
      (should-not (member "Plain" titles)))))

(ert-deftest org-chronicle-test-buffer-scenes-nearest-enclosing ()
  "A link inside a descendant scene is owned by the descendant, not the ancestor."
  (org-chronicle-test--with-org
      (concat "* Outer\n:PROPERTIES:\n:EVENT: [[id:e1]]\n:END:\n"
              "outer [[chronicle:a][A]]\n"
              "** Inner\ninner [[chronicle:b][B]]\n")
    (let* ((scenes (org-chronicle--buffer-scenes))
           (outer (cl-find "Outer" scenes :key (lambda (s) (plist-get s :title)) :test #'equal))
           (inner (cl-find "Inner" scenes :key (lambda (s) (plist-get s :title)) :test #'equal)))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) (plist-get outer :refs)) '("a")))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) (plist-get inner :refs)) '("b"))))))

(ert-deftest org-chronicle-test-date-min-max ()
  "Date min/max treat nil as an open (ignored) bound."
  (let ((a (org-chronicle--date-parse "1863"))
        (b (org-chronicle--date-parse "1865")))
    (should (equal (org-chronicle--date-max a b) b))
    (should (equal (org-chronicle--date-min a b) a))
    (should (equal (org-chronicle--date-max nil b) b))
    (should (equal (org-chronicle--date-min a nil) a))))

(ert-deftest org-chronicle-test-name-adoption ()
  "A name-change event records the adoption date of its NEW-NAME for the subject."
  (let* ((events
          (list (list :life-event "name-change"
                      :subject '("Eliza Dent") :new-name "Mrs. Grant"
                      :date (org-chronicle--date-parse "1863-08-22"))))
         (idx (make-hash-table :test #'equal))
         (adopt (org-chronicle--name-adoption events idx)))
    (should (equal (gethash (cons "Eliza Dent" "mrs. grant") adopt)
                   (org-chronicle--date-parse "1863-08-22")))))

(defconst org-chronicle-test--scene-root
  '(("people.org" . "\
* Eliza Dent
:PROPERTIES:
:ID:      eliza
:KIND:    person
:BORN:    <1826-01-26>
:DIED:    <1902-12-14>
:ALIASES: Mrs. Grant
:END:
* Marek
:PROPERTIES:
:ID:    marek
:KIND:  person
:BORN:  <1870-01-01>
:END:
")
    ("events.org" . "\
* Vicksburg
:PROPERTIES:
:ID:    vicksburg
:DATE:  <1863-07-04>
:END:
* Appomattox
:PROPERTIES:
:ID:    appomattox
:DATE:  <1865-04-09>
:END:
* Eliza renamed
:PROPERTIES:
:LIFE-EVENT: name-change
:SUBJECT:    Eliza Dent
:NEW-NAME:   Mrs. Grant
:DATE:       <1863-08-22>
:END:
"))
  "A self-contained root used by scene-window/finding tests.")

(ert-deftest org-chronicle-test-scene-window-bounded ()
  "An entity reference plus AFTER/BEFORE constraints intersect to a window."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :refs (list (list :id "eliza" :name nil))
                        :after-ids '("vicksburg") :before-ids '("appomattox")))
           (w (org-chronicle--scene-window scene ctx)))
      (should (equal (car w) (org-chronicle--date-parse "1863-07-04")))
      (should (equal (cdr w) (org-chronicle--date-parse "1865-04-09"))))))

(ert-deftest org-chronicle-test-scene-window-empty ()
  "A lower bound later than an upper bound yields :empty.
Marek is born 1870, but BEFORE Vicksburg (1863) caps the window earlier."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let ((ctx (org-chronicle--scene-context)))
      (should (eq (org-chronicle--scene-window
                   (list :refs (list (list :id "marek" :name nil))
                         :before-ids '("vicksburg"))
                   ctx)
                  :empty)))))

(ert-deftest org-chronicle-test-scene-window-unbounded ()
  "A scene with no resolvable bounds is unbounded (nil . nil)."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let ((ctx (org-chronicle--scene-context)))
      (should (equal (org-chronicle--scene-window (list :refs nil) ctx) '(nil))))))

(ert-deftest org-chronicle-test-scene-anchor ()
  "Own DATE wins; else the EVENT span; else nil."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let ((ctx (org-chronicle--scene-context)))
      (should (equal (car (org-chronicle--scene-anchor
                           (list :own-date (org-chronicle--date-parse "1864")) ctx))
                     (org-chronicle--date-parse "1864")))
      (should (equal (car (org-chronicle--scene-anchor
                           (list :event-ids '("vicksburg")) ctx))
                     (org-chronicle--date-parse "1863-07-04")))
      (should (null (org-chronicle--scene-anchor (list :refs nil) ctx))))))

(ert-deftest org-chronicle-test-findings-out-of-window ()
  "A pinned date outside the window yields an out-of-window verdict + reason."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :title "S" :marker (point-marker)
                        :own-date (org-chronicle--date-parse "1862-01-01")
                        :refs (list (list :id "eliza" :name "Mrs. Grant"
                                          :marker (point-marker)))))
           (f (org-chronicle--scene-findings scene ctx)))
      (should (eq (plist-get f :verdict) 'out-of-window))
      (should (cl-some (lambda (r) (string-match-p "not adopted" (car r)))
                       (plist-get f :reasons))))))

(ert-deftest org-chronicle-test-findings-dangling ()
  "An unresolved :EVENT: id yields a dangling verdict."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :title "S" :marker (point-marker)
                        :event-ids '("missing-99") :refs nil))
           (f (org-chronicle--scene-findings scene ctx)))
      (should (eq (plist-get f :verdict) 'dangling)))))

(ert-deftest org-chronicle-test-findings-floating ()
  "An undated scene with a resolvable window is floating, not an error."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :title "S" :marker (point-marker)
                        :after-ids '("vicksburg") :refs nil))
           (f (org-chronicle--scene-findings scene ctx)))
      (should (eq (plist-get f :verdict) 'floating)))))

(ert-deftest org-chronicle-test-findings-clean ()
  "A pinned date inside the window produces no finding."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :title "S" :marker (point-marker)
                        :own-date (org-chronicle--date-parse "1864-01-01")
                        :refs (list (list :id "eliza" :name nil
                                          :marker (point-marker))))))
      (should (null (org-chronicle--scene-findings scene ctx))))))

(ert-deftest org-chronicle-test-findings-span-end-out-of-window ()
  "A span anchor whose END exceeds the window is flagged at its end date."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (let* ((ctx (org-chronicle--scene-context))
           (scene (list :title "S" :marker (point-marker)
                        :own-date (org-chronicle--date-parse "1863-07-04")
                        :own-date-end (org-chronicle--date-parse "1866-01-01")
                        :before-ids '("vicksburg") :refs nil))
           (f (org-chronicle--scene-findings scene ctx)))
      (should (eq (plist-get f :verdict) 'out-of-window))
      (should (equal (plist-get f :offending)
                     (org-chronicle--date-parse "1866-01-01")))
      (should (plist-get f :reasons))
      (should (string-match-p
               "1866-01-01"
               (org-chronicle--scene-verdict-line f))))))

(ert-deftest org-chronicle-test-lint-scenes-reports ()
  "The command lists an out-of-window scene and a clean run says so."
  (org-chronicle-test--with-root
      (append org-chronicle-test--scene-root
              '(("chapters/01.org" . "\
* The siege begins
:PROPERTIES:
:EVENT: [[id:vicksburg]]
:DATE:  <1862-01-01>
:TRUTH: fictional
:END:
Eliza, called [[chronicle:eliza][Mrs. Grant]], watched.
")))
    (org-chronicle-lint-scenes)
    (with-current-buffer "*org-chronicle-lint-scenes*"
      (should (string-match-p "The siege begins" (buffer-string)))
      (should (string-match-p "not adopted" (buffer-string)))
      (should (eq major-mode 'org-chronicle-scene-lint-mode)))))

(ert-deftest org-chronicle-test-lint-scenes-clean ()
  "A root with no scene issues reports a clean result."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (org-chronicle-lint-scenes)
    (with-current-buffer "*org-chronicle-lint-scenes*"
      (should (string-match-p "No scene issues found" (buffer-string))))))

(ert-deftest org-chronicle-test-insert-reference ()
  "Inserting a reference writes a chronicle: link with the chosen name."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Eliza Dent")))
      (with-temp-buffer
        (org-mode)
        (org-chronicle-insert-reference)
        (should (equal (buffer-string) "[[chronicle:eliza][Eliza Dent]]"))))))

(ert-deftest org-chronicle-test-set-event ()
  "Setting the event writes an :EVENT: id link on the heading."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Vicksburg")))
      (org-chronicle-test--with-org "* A scene\nbody\n"
        (org-chronicle-set-event)
        (should (equal (org-entry-get nil "EVENT") "[[id:vicksburg]]"))))))

(ert-deftest org-chronicle-test-add-constraint-appends ()
  "Adding a second AFTER constraint appends to the existing value."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Appomattox")))
      (org-chronicle-test--with-org
          "* A scene\n:PROPERTIES:\n:AFTER: [[id:vicksburg]]\n:END:\nbody\n"
        (org-chronicle-add-constraint 'after)
        (should (equal (org-entry-get nil "AFTER")
                       "[[id:vicksburg]]; [[id:appomattox]]"))))))

(ert-deftest org-chronicle-test-set-scene-date-bounded ()
  "Setting a self-defining scene's date writes DATE and stamps TRUTH fictional."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "1864-01-01")))
      (org-chronicle-test--with-org
          "* A scene\n:PROPERTIES:\n:AFTER: [[id:vicksburg]]\n:END:\nbody\n"
        (org-chronicle-set-scene-date)
        (should (equal (org-entry-get nil "DATE") "<1864-01-01>"))
        (should (equal (org-entry-get nil "TRUTH") "fictional"))))))

(ert-deftest org-chronicle-test-set-scene-date-empty-refuses ()
  "An over-constrained scene refuses with a user-error."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (org-chronicle-test--with-org
        (concat "* A scene\n:PROPERTIES:\n:BEFORE: [[id:vicksburg]]\n:END:\n"
                "ref [[chronicle:marek][Marek]]\n")
      (should-error (org-chronicle-set-scene-date) :type 'user-error))))

;;;; Scene EARLIEST/LATEST bounds

(ert-deftest org-chronicle-test-scene-props-bounds ()
  "EARLIEST/LATEST parse into the scene plist and mark a heading a scene."
  (with-temp-buffer
    (org-mode)
    (insert "* A scene\n:PROPERTIES:\n:EARLIEST: 1850\n:LATEST: 1855-06\n:END:\n")
    (goto-char (point-min))
    (let ((h (org-chronicle--heading-scene-props)))
      (should (equal (plist-get (plist-get h :earliest) :year) 1850))
      (should (equal (plist-get (plist-get h :latest) :month) 6))
      (should (org-chronicle--structural-scene-p h)))))

(ert-deftest org-chronicle-test-scene-reference-targets ()
  "A titled scene heading appears among reference targets."
  (with-temp-buffer
    (org-mode)
    (insert "* The duel\n:PROPERTIES:\n:ID: duel-1\n:EARLIEST: 1850\n:END:\n")
    (let* ((scenes (org-chronicle--buffer-scenes))
           (targets (org-chronicle--scene-targets scenes)))
      (should (assoc "The duel" targets))
      (should (equal (cdr (assoc "The duel" targets)) "duel-1")))))

(ert-deftest org-chronicle-test-context-cache ()
  "Cached context is reused until invalidated."
  (let ((org-chronicle--context-cache nil)
        (calls 0))
    (cl-letf (((symbol-function 'org-chronicle--scene-context)
               (lambda () (cl-incf calls) (list :stamp calls))))
      (should (= (plist-get (org-chronicle--cached-context) :stamp) 1))
      (should (= (plist-get (org-chronicle--cached-context) :stamp) 1))
      (org-chronicle--invalidate-context)
      (should (= (plist-get (org-chronicle--cached-context) :stamp) 2)))))

(ert-deftest org-chronicle-test-accept-writes-date ()
  "Accepting a placement writes the scene's DATE and marks it fictional."
  (let ((file (expand-file-name "fixtures/solver-scenes.org"
                                org-chronicle-tests--dir)))
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^\\* Floating with earliest")
            (org-chronicle--commit-placement
             (point-marker) (org-chronicle--date-parse "1850"))
            (should (equal (org-entry-get nil "DATE")
                           (org-chronicle--ts "1850")))
            (should (equal (org-entry-get nil "TRUTH") "fictional")))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(ert-deftest org-chronicle-test-peek-string ()
  "The peek string names the verdict and window."
  (let ((s (org-chronicle--peek-string
            'floating (cons (org-chronicle--date-parse "1850")
                            (org-chronicle--date-parse "1855")))))
    (should (string-match-p "floating" s))
    (should (string-match-p "1850" s))
    (should (string-match-p "1855" s))))

(ert-deftest org-chronicle-test-annotate-windows-overlay ()
  "Annotate-windows draws overlays on floating scenes and toggle removes them.
Buffer-modified-p must stay nil throughout."
  (let* ((file (expand-file-name "fixtures/solver-scenes.org"
                                 org-chronicle-tests--dir))
         (org-chronicle-root (file-name-directory file))
         (org-chronicle--context-cache nil))
    (with-current-buffer (find-file-noselect file)
      (unwind-protect
          (progn
            (set-buffer-modified-p nil)
            ;; Enable annotations.
            (org-chronicle-annotate-windows)
            ;; At least one floating scene should have gotten an overlay.
            (should (> (length org-chronicle--window-overlays) 0))
            ;; Overlays should carry an after-string.
            (should (cl-every (lambda (ov) (overlay-get ov 'after-string))
                              org-chronicle--window-overlays))
            ;; Buffer must NOT be marked modified.
            (should-not (buffer-modified-p))
            ;; Toggle off.
            (org-chronicle-annotate-windows)
            ;; Overlays gone.
            (should (null org-chronicle--window-overlays))
            ;; Still not modified.
            (should-not (buffer-modified-p)))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(ert-deftest org-chronicle-test-affixation ()
  "Candidate affixation appends the chapter and window annotation."
  (let* ((annot (make-hash-table :test #'equal))
         (_ (puthash "The duel" "[03-duel.org · 1850..1855]" annot))
         (fn (org-chronicle--affixation-function annot))
         (row (car (funcall fn '("The duel")))))
    (should (equal (nth 0 row) "The duel"))
    (should (string-match-p "1850\\.\\.1855" (nth 2 row)))))

(ert-deftest org-chronicle-test-add-constraint-nil-id-guard ()
  "Add-constraint signals user-error when target cannot be resolved to an id."
  (org-chronicle-test--with-root org-chronicle-test--scene-root
    (cl-letf (((symbol-function 'org-chronicle--read-reference)
               (lambda () (cons nil "Unresolvable scene"))))
      (org-chronicle-test--with-org "* A scene\nbody\n"
        (should-error (org-chronicle-add-constraint 'after)
                      :type 'user-error)))))

(provide 'org-chronicle-tests)
;;; org-chronicle-tests.el ends here
