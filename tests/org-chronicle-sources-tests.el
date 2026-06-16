;;; org-chronicle-sources-tests.el --- Tests for org-chronicle-sources -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Offline unit tests for the source-neutral import layer.  Run with:  make test

;;; Code:

(require 'ert)
(require 'org-chronicle-sources)
(require 'cl-lib)

(ert-deftest org-chronicle-sources-test-loads ()
  "The source-neutral layer loads."
  (should (featurep 'org-chronicle-sources)))

(ert-deftest org-chronicle-sources-test-classify ()
  "Test classification of a change against the current heading value."
  (let ((change (list :target 'entity :property "BORN" :value "1815-12-10")))
    (should (eq (org-chronicle-sources--classify change nil) 'new))
    (should (eq (org-chronicle-sources--classify change "1815-12-10") 'same))
    (should (eq (org-chronicle-sources--classify change "1900-01-01") 'conflict))))

(ert-deftest org-chronicle-sources-test-classify-dates ()
  "BORN/DIED classification ignores brackets around dates."
  (let ((change (list :target 'entity :property "BORN" :value "<1815-12-10>")))
    (should (eq (org-chronicle-sources--classify change "1815-12-10") 'same))
    (should (eq (org-chronicle-sources--classify change "<1815-12-10>") 'same))
    (should (eq (org-chronicle-sources--classify change "1900-01-01") 'conflict))))

(ert-deftest org-chronicle-sources-test-classify-span-dates ()
  (let ((change (list :target 'entity :property "BUILT" :value "<1896>")))
    (should (eq (org-chronicle-sources--classify change "1896") 'same))
    (should (eq (org-chronicle-sources--classify change "1900") 'conflict))))

(ert-deftest org-chronicle-sources-test-event-key ()
  (let ((wd (org-chronicle-sources--get 'wikidata)))
    (should (equal (org-chronicle-sources--event-key
                    (list :kind "birth") "ABC" wd "Q7259") "birth:ABC"))
    (should (equal (org-chronicle-sources--event-key
                    (list :kind "death") "ABC" wd "Q7259") "death:ABC"))
    (should (equal (org-chronicle-sources--event-key
                    (list :kind "position" :object-qid "Q30") "ABC" wd "Q7259")
                   "position:ABC:wd:Q30"))
    (should (equal (org-chronicle-sources--event-key
                    (list :kind "marriage" :object-qid "Q123") "ABC" wd "Q7259")
                   (org-chronicle-sources--event-key
                    (list :kind "marriage" :object-qid "Q7259") "XYZ" wd "Q123")))
    (should (equal (org-chronicle-sources--event-key
                    (list :kind "marriage" :object-qid "Q123") "ABC" wd "Q7259")
                   "marriage:wd:Q123:wd:Q7259"))
    (should (null (org-chronicle-sources--event-key
                   (list :kind "marriage") "ABC" wd "Q7259")))
    (should (null (org-chronicle-sources--event-key
                   (list :kind "position") "ABC" wd "Q7259")))))

(ert-deftest org-chronicle-sources-test-event-key-curie ()
  "Position/marriage keys use the source curie; birth keys do not."
  (let ((wd (org-chronicle-sources--get 'wikidata))
        (fg (org-chronicle-sources--get 'factgrid)))
    (should (equal (org-chronicle-sources--event-key
                    '(:kind "birth") "orgid-1" wd "Q7259")
                   "birth:orgid-1"))
    (should (equal (org-chronicle-sources--event-key
                    '(:kind "position" :object-qid "Q30") "orgid-1" wd "Q7259")
                   "position:orgid-1:wd:Q30"))
    (should (equal (org-chronicle-sources--event-key
                    '(:kind "position" :object-qid "Q30") "orgid-1" fg "Q7259")
                   "position:orgid-1:fg:Q30"))
    (should (equal (org-chronicle-sources--event-key
                    '(:kind "marriage" :object-qid "Q123") "orgid-1" wd "Q7259")
                   "marriage:wd:Q123:wd:Q7259"))))


(ert-deftest org-chronicle-sources-test-review-rows ()
  "Test that review rows carry selected state from change defaults."
  (let* ((changes
          (list (list :target 'entity :group 'vitals :property "BORN"
                      :value "1815-12-10" :default t :status 'new)
                (list :target 'event :group 'events :default nil :status 'new
                      :event (list :title "Countess of Lovelace" :date "1838"))))
         (rows (org-chronicle-sources--review-rows changes)))
    (should (= (length rows) 2))
    (should (plist-get (nth 0 (car rows)) :selected))
    (should-not (plist-get (nth 0 (cadr rows)) :selected))))

(ert-deftest org-chronicle-sources-test-selected-changes ()
  "Test that only rows with :selected non-nil are returned."
  (let ((rows (list (list (list :selected t) (list :property "BORN"))
                    (list (list :selected nil) (list :property "DIED")))))
    (let ((sel (org-chronicle-sources--selected-changes rows)))
      (should (= (length sel) 1))
      (should (equal (plist-get (car sel) :property) "BORN")))))

(ert-deftest org-chronicle-sources-test-events-file ()
  (let ((org-chronicle-root "/tmp/octw/"))
    (let ((org-chronicle-sources-events-file nil))
      (should (equal (org-chronicle-sources--events-file)
                     "/tmp/octw/imported/events.org")))
    (let ((org-chronicle-sources-events-file "/tmp/elsewhere.org"))
      (should (equal (org-chronicle-sources--events-file)
                     "/tmp/elsewhere.org")))))

(ert-deftest org-chronicle-sources-test-events-index-and-append ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root)))
    (unwind-protect
        (let ((index (org-chronicle-sources--events-index)))
          (should (= (hash-table-count index) 0))
          (org-chronicle-sources--append-to-events-file
           "* Birth of X\n:PROPERTIES:\n:DATE: <1815-12-10>\n:END:\n"
           "birth:ABC" index)
          (should (gethash "birth:ABC" index))
          (let ((fresh (org-chronicle-sources--events-index)))
            (should (gethash "birth:ABC" fresh))
            (org-with-point-at (gethash "birth:ABC" fresh)
              (should (equal (org-entry-get nil "IMPORT-KEY") "birth:ABC")))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-classify-event ()
  (with-temp-buffer
    (org-mode)
    (insert "* Birth of X\n:PROPERTIES:\n:DATE: <1815-12-10>\n:LOCATION: London\n:END:\n")
    (goto-char (point-min))
    (let ((m (point-marker))
          (same (list :event (list :date "1815-12-10" :location "London")))
          (confl (list :event (list :date "1900-01-01" :location "London"))))
      (should (eq (org-chronicle-sources--classify-event same m) 'same))
      (should (eq (org-chronicle-sources--classify-event confl m) 'conflict))
      (should (eq (org-chronicle-sources--classify-event same nil) 'new)))))

(ert-deftest org-chronicle-sources-test-event-change-string ()
  "Life events use life-event-string; position events use event-string."
  (let ((life (org-chronicle-sources--event-change-string
               (list :provenance "u"
                     :event (list :life-event "birth" :title "Birth of X"
                                  :date "1815-12-10" :subject (list "X")))))
        (pos (org-chronicle-sources--event-change-string
              (list :provenance "u"
                    :event (list :title "Countess of Lovelace" :date "1838")))))
    (should (string-match-p ":LIFE-EVENT: birth" life))
    (should (string-match-p ":SUBJECT:" life))
    (should-not (string-match-p "LIFE-EVENT" pos))
    (should-not (string-match-p "nil" pos))))

(ert-deftest org-chronicle-sources-test-sources-append ()
  "Applying a change appends to SOURCES rather than overwriting it."
  (with-temp-buffer
    (org-mode)
    (insert "* X\n:PROPERTIES:\n:SOURCES: my-book p.12\n:END:\n")
    (goto-char (point-min))
    (org-chronicle-sources--apply-entity-change
     (list :target 'entity :property "BORN" :value "<1815-12-10>"
           :provenance "https://www.wikidata.org/wiki/Q7259"))
    (goto-char (point-min))
    (let ((s (org-entry-get nil "SOURCES")))
      (should (string-match-p "my-book p.12" s))
      (should (string-match-p "Q7259" s)))))

(ert-deftest org-chronicle-sources-test-apply-changes ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n")
          (goto-char (point-min))
          (let ((changes
                 (list (list :target 'entity :property "BORN" :value "<1815-12-10>"
                             :provenance "https://www.wikidata.org/wiki/Q7259")
                       (list :target 'entity :property "WIKIDATA" :value "Q7259"
                             :provenance "https://www.wikidata.org/wiki/Q7259")
                       (list :target 'event :key "birth:ABC"
                             :provenance "https://www.wikidata.org/wiki/Q7259"
                             :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                          :date "1815-12-10" :subject (list "Ada Lovelace")
                                          :location "London")))))
            (org-chronicle-sources--apply-changes
             changes (org-chronicle-sources--events-index)))
          (goto-char (point-min))
          (should (equal (org-entry-get nil "BORN") "<1815-12-10>"))
          (should (equal (org-entry-get nil "WIKIDATA") "Q7259"))
          (should (equal (org-entry-get nil "SOURCES")
                         "https://www.wikidata.org/wiki/Q7259"))
          (let ((events (with-temp-buffer
                          (insert-file-contents org-chronicle-sources-events-file)
                          (buffer-string))))
            (should (string-match-p "Birth of Ada Lovelace" events))
            (should (string-match-p ":LIFE-EVENT: birth" events))
            (should (string-match-p ":IMPORT-KEY: birth:ABC" events))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-apply-event-idempotent ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (change (list :target 'event :key "birth:ABC"
                       :provenance "https://www.wikidata.org/wiki/Q7259"
                       :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                    :date "1815-12-10" :subject (list "Ada Lovelace")
                                    :location "London"))))
    (unwind-protect
        (let ((index (org-chronicle-sources--events-index)))
          (org-chronicle-sources--apply-event-change change index)
          (org-chronicle-sources--apply-event-change change index)
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-sources-events-file)
                        (buffer-string))))
            (should (= 1 (cl-count ?* body)))
            (should (string-match-p ":IMPORT-KEY: birth:ABC" body))
            (should (string-match-p "Birth of Ada Lovelace" body)))
          (let ((change2 (copy-sequence change)))
            (plist-put change2 :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                            :date "1816-01-01" :subject (list "Ada Lovelace")))
            (org-chronicle-sources--apply-event-change change2 index))
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-sources-events-file)
                        (buffer-string))))
            (should (= 1 (cl-count ?* body)))
            (should (string-match-p "1816-01-01" body))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-apply-event-preserves-unmanaged ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (change (list :target 'event :key "birth:ABC"
                       :provenance "https://www.wikidata.org/wiki/Q7259"
                       :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                    :date "1815-12-10" :subject (list "Ada Lovelace")))))
    (unwind-protect
        (let ((index (org-chronicle-sources--events-index)))
          (org-chronicle-sources--apply-event-change change index)
          ;; Author edits the heading: renames it and adds an unmanaged property.
          (org-with-point-at (gethash "birth:ABC" index)
            (org-back-to-heading t)
            (org-edit-headline "Ada's birth (my note)")
            (org-set-property "NOTES" "hand-written")
            (save-buffer))
          ;; Re-apply with a changed date.
          (let ((change2 (copy-sequence change)))
            (plist-put change2 :event (list :life-event "birth" :title "Birth of Ada Lovelace"
                                            :date "1816-02-02" :subject (list "Ada Lovelace")))
            (org-chronicle-sources--apply-event-change change2 index))
          (let ((body (with-temp-buffer
                        (insert-file-contents org-chronicle-sources-events-file)
                        (buffer-string))))
            (should (string-match-p "1816-02-02" body))          ; managed field updated
            (should (string-match-p "Ada's birth (my note)" body)) ; title preserved
            (should (string-match-p ":NOTES: *hand-written" body)))) ; unmanaged prop preserved
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-classify-changes ()
  (with-temp-buffer
    (org-mode)
    (insert "* Ada\n:PROPERTIES:\n:KIND: person\n:BORN: <1815-12-10>\n:END:\n")
    (insert "* Birth of Ada\n:PROPERTIES:\n:IMPORT-KEY: birth:ADA\n:DATE: <1815-12-10>\n:END:\n")
    (goto-char (point-min))
    (let* ((marker (point-marker))
           (index (make-hash-table :test 'equal))
           (wd (org-chronicle-sources--get 'wikidata)))
      (save-excursion
        (goto-char (point-min))
        (search-forward "* Birth of Ada")
        (beginning-of-line)
        (puthash "birth:ADA" (point-marker) index))
      (let* ((changes
              (list
               (list :target 'entity :property "BORN" :value "<1815-12-10>")
               (list :target 'entity :property "DIED" :value "<1852-11-27>")
               (list :target 'event :provenance "u"
                     :event (list :kind "birth" :life-event "birth" :date "1815-12-10"))
               (list :target 'event :provenance "u"
                     :event (list :kind "marriage" :life-event "marriage" :date "1835"))))
             (result (org-chronicle-sources--classify-changes changes marker "ADA" wd "Q7259" index)))
        (cl-flet ((by-prop (p) (cl-find p result
                                        :key (lambda (c) (plist-get c :property))
                                        :test (lambda (a b) (and b (equal a b)))))
                  (event-of (k) (cl-find k result
                                         :key (lambda (c) (and (eq (plist-get c :target) 'event)
                                                               (plist-get (plist-get c :event) :kind)))
                                         :test (lambda (a b) (equal a b)))))
          (should (= (length result) 3))
          (should (eq (plist-get (by-prop "BORN") :status) 'same))
          (should (eq (plist-get (by-prop "DIED") :status) 'new))
          (let ((be (event-of "birth")))
            (should (equal (plist-get be :key) "birth:ADA"))
            (should (eq (plist-get be :status) 'same))))))))

(ert-deftest org-chronicle-sources-test-import ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (people-file (expand-file-name "people.org" root)))
    (unwind-protect
        (progn
          (with-temp-file people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (cond ((string-prefix-p "Source" prompt) "wikidata")
                             ((string-prefix-p "Kind" prompt) "person")
                             (t ""))))
                    ((symbol-function 'org-chronicle-wikibase--resolve)
                     (lambda (&rest _) "Q7259"))
                    ((symbol-function 'org-chronicle-wikibase--fetch-record)
                     (lambda (source _qid _kind)
                       (list :source source :qid "Q7259" :kind 'person
                             :born (org-chronicle--date-parse "1815-12-10")
                             :birthplace "London")))
                    ((symbol-function 'org-chronicle-sources--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (with-current-buffer (find-file-noselect people-file)
              (goto-char (point-min))
              (org-chronicle-import))
            (let ((content (with-temp-buffer
                             (insert-file-contents people-file)
                             (buffer-string))))
              (should (string-match-p ":BORN:.*1815-12-10" content))
              (should (string-match-p ":WIKIDATA:.*Q7259" content)))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-import-create ()
  "Non-entity heading triggers entity creation then enriches the new entity."
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Charles Babbage\nsome prose\n")
          (goto-char (point-min))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (cond ((string-prefix-p "Source" prompt) "wikidata")
                             ((string-prefix-p "Kind" prompt) "person")
                             (t ""))))
                    ((symbol-function 'org-chronicle-wikibase--resolve)
                     (lambda (&rest _) "Q46633"))
                    ((symbol-function 'org-chronicle-wikibase--fetch-record)
                     (lambda (source _qid _kind)
                       (list :source source :qid "Q46633" :kind 'person
                             :born (org-chronicle--date-parse "1791-12-26"))))
                    ((symbol-function 'org-chronicle-sources--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (org-chronicle-import))
          (let ((people (with-temp-buffer
                          (insert-file-contents org-chronicle-people-file)
                          (buffer-string))))
            (should (string-match-p "Charles Babbage" people))
            (should (string-match-p ":KIND:" people))
            (should (string-match-p ":WIKIDATA:.*Q46633" people))
            (should (string-match-p ":BORN:" people))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-import-promote ()
  "Point on SPOUSE property creates a new entity seeded from the value."
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:SPOUSE: William King-Noel\n:END:\n")
          (goto-char (point-min))
          (search-forward ":SPOUSE:")
          (beginning-of-line)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (cond ((string-prefix-p "Source" prompt) "wikidata")
                             ((string-prefix-p "Kind" prompt) "person")
                             (t ""))))
                    ((symbol-function 'org-chronicle-wikibase--resolve)
                     (lambda (_source seed)
                       (should (string-match-p "William" seed))
                       "Q123"))
                    ((symbol-function 'org-chronicle-wikibase--fetch-record)
                     (lambda (source _qid _kind)
                       (list :source source :qid "Q123" :kind 'person
                             :born (org-chronicle--date-parse "1805"))))
                    ((symbol-function 'org-chronicle-sources--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (org-chronicle-import))
          (let ((people (with-temp-buffer
                          (insert-file-contents org-chronicle-people-file)
                          (buffer-string))))
            (should (string-match-p "William King-Noel" people))
            (should (string-match-p ":WIKIDATA:.*Q123" people))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-import-events-idempotent ()
  (let* ((root (make-temp-file "octs-root" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (people-file (expand-file-name "people.org" root)))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt &rest _)
                     (cond ((string-prefix-p "Source" prompt) "wikidata")
                           ((string-prefix-p "Kind" prompt) "person")
                           (t ""))))
                  ((symbol-function 'org-chronicle-wikibase--resolve)
                   (lambda (&rest _) "Q7259"))
                  ((symbol-function 'org-chronicle-wikibase--fetch-record)
                   (lambda (source _qid _kind)
                     (list :source source :qid "Q7259" :kind 'person
                           :born (org-chronicle--date-parse "1815-12-10")
                           :birthplace "London")))
                  ((symbol-function 'org-chronicle-sources--review)
                   (lambda (changes on-confirm) (funcall on-confirm changes))))
          (with-temp-file people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:END:\n"))
          (cl-flet ((run ()
                      (with-current-buffer (find-file-noselect people-file)
                        (goto-char (point-min))
                        (org-chronicle-import))))
            (run)
            (run)
            (let ((body (with-temp-buffer
                          (insert-file-contents org-chronicle-sources-events-file)
                          (buffer-string))))
              (should (= 1 (cl-count-if
                            (lambda (l) (string-match-p "Birth of Ada Lovelace" l))
                            (split-string body "\n")))))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-import-create-place ()
  "Importing with kind=place writes to places file with BUILT and WIKIDATA."
  (let* ((root (make-temp-file "octs-place" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-places-file (expand-file-name "places.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (org-id-locations-file (expand-file-name ".org-id-locations" root)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-chronicle-sources--label-at-point) (lambda () nil))
                  ((symbol-function 'completing-read)
                   (lambda (prompt &rest _)
                     (cond ((string-prefix-p "Source" prompt) "wikidata")
                           ((string-prefix-p "Kind" prompt) "place")
                           (t ""))))
                  ((symbol-function 'org-chronicle-wikibase--resolve) (lambda (&rest _) "Q3505806"))
                  ((symbol-function 'org-chronicle-wikibase--fetch-record)
                   (lambda (source _qid _kind)
                     (list :source source :qid "Q3505806" :kind 'place :label "Sutro Baths"
                           :start (org-chronicle--date-parse "1896") :end nil)))
                  ((symbol-function 'org-chronicle-sources--review)
                   (lambda (changes on-confirm) (funcall on-confirm changes))))
          (with-temp-file org-chronicle-people-file (insert "* placeholder\n"))
          (with-current-buffer (find-file-noselect org-chronicle-people-file)
            (goto-char (point-max))
            (insert "* Sutro Baths\nprose\n")
            (goto-char (point-max)) (forward-line -2)
            (org-chronicle-import))
          (let ((places (with-temp-buffer (insert-file-contents org-chronicle-places-file) (buffer-string))))
            (should (string-match-p ":KIND: *place" places))
            (should (string-match-p ":BUILT:" places))
            (should (string-match-p ":WIKIDATA: *Q3505806" places))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-import-writes-source-key ()
  "Import with source=factgrid writes a FACTGRID key alongside existing WIKIDATA."
  (let* ((root (make-temp-file "octs-key" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (people-file (expand-file-name "people.org" root)))
    (unwind-protect
        (progn
          (with-temp-file people-file
            (insert "* Ada\n:PROPERTIES:\n:KIND: person\n:WIKIDATA: Q7259\n:END:\n"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (if (string-prefix-p "Source" prompt) "factgrid" "person")))
                    ((symbol-function 'org-chronicle-wikibase--resolve)
                     (lambda (_source _seed) "Q123"))
                    ((symbol-function 'org-chronicle-wikibase--fetch-record)
                     (lambda (source qid kind)
                       (list :source source :qid qid :kind kind :label "Ada")))
                    ((symbol-function 'org-chronicle-sources--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes))))
            (with-current-buffer (find-file-noselect people-file)
              (goto-char (point-min))
              (org-chronicle-import)))
          (with-current-buffer (find-file-noselect people-file)
            (goto-char (point-min))
            (should (equal (org-entry-get nil "WIKIDATA") "Q7259"))
            (should (equal (org-entry-get nil "FACTGRID") "Q123"))))
      (delete-directory root t))))


(ert-deftest org-chronicle-sources-test-reconcile-event-drift ()
  (let* ((root (make-temp-file "octs-rec" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (org-id-locations-file (expand-file-name ".org-id-locations" root))
         captured)
    (unwind-protect
        (progn
          (with-temp-file org-chronicle-people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:WIKIDATA: Q7259\n:ID: ADA-ID\n:BORN: <1815-12-10>\n:END:\n"))
          (make-directory (file-name-directory org-chronicle-sources-events-file) t)
          (with-temp-file org-chronicle-sources-events-file
            (insert "* Birth of Ada Lovelace\n:PROPERTIES:\n:IMPORT-KEY: birth:ADA-ID\n:LIFE-EVENT: birth\n:DATE: <1800-01-01>\n:END:\n"))
          (cl-letf (((symbol-function 'org-chronicle-wikibase--fetch-record)
                     (lambda (source _qid _kind)
                       (list :source source :qid "Q7259" :kind 'person
                             :born (org-chronicle--date-parse "1815-12-10")
                             :died (org-chronicle--date-parse "1852-11-27"))))
                    ((symbol-function 'org-chronicle-sources--review)
                     (lambda (drift on-confirm) (setq captured drift) (funcall on-confirm drift))))
            (with-current-buffer (find-file-noselect org-chronicle-people-file)
              (goto-char (point-min))
              (org-chronicle-reconcile)))
          (cl-flet ((event-of (k) (cl-find k captured
                                           :key (lambda (c) (and (eq (plist-get c :target) 'event)
                                                                 (plist-get (plist-get c :event) :kind)))
                                           :test (lambda (a b) (equal a b)))))
            (should (eq (plist-get (event-of "birth") :status) 'conflict))
            (should (eq (plist-get (event-of "death") :status) 'new))
            (should-not (cl-find 'same captured :key (lambda (c) (plist-get c :status)))))
          (let ((events (with-temp-buffer
                          (insert-file-contents org-chronicle-sources-events-file)
                          (buffer-string))))
            (should (= 1 (cl-count-if (lambda (l) (string-match-p "Birth of Ada Lovelace" l))
                                      (split-string events "\n"))))
            (should (string-match-p "1815-12-10" events))
            (should-not (string-match-p "1800-01-01" events))
            (should (string-match-p "Death of Ada Lovelace" events))))
      (delete-directory root t))))

(ert-deftest org-chronicle-sources-test-reconcile-rejects-unknown-kind ()
  (with-temp-buffer
    (org-mode)
    (insert "* A topic\n:PROPERTIES:\n:KIND: topic\n:WIKIDATA: Q1\n:END:\n")
    (goto-char (point-min))
    (should-error (org-chronicle-reconcile) :type 'user-error)))

(ert-deftest org-chronicle-sources-test-alternates-surface ()
  (let* ((rec (list :source (org-chronicle-sources--get 'wikidata)
                    :qid "Q935" :label "Isaac Newton"
                    :born (org-chronicle--date-parse "1643-01-04")
                    :born-alternates '("1642")))
         (changes (org-chronicle-wikibase--record->changes rec "Isaac Newton"))
         (born (cl-find "BORN" changes
                        :key (lambda (c) (plist-get c :property))
                        :test (lambda (a b) (and b (equal a b))))))
    (should (equal (plist-get born :alternates) '("1642")))
    (should (string-match-p "also lists: 1642"
                            (org-chronicle-sources--change-label born)))))

(ert-deftest org-chronicle-sources-test-registry-lookup ()
  "The registry resolves a source plist and its fields by id."
  (let ((s (org-chronicle-sources--get 'wikidata)))
    (should (eq (plist-get s :adapter) 'wikibase))
    (should (equal (plist-get s :key-property) "WIKIDATA"))
    (should (equal (plist-get s :curie) "wd:"))
    (should (equal (org-chronicle-sources--pid s :birthplace) "P19"))
    (should (equal (org-chronicle-sources--span-pids s 'person)
                   (cons "P569" "P570")))
    (should (equal (org-chronicle-sources--span-pids s 'place)
                   (cons "P571" "P576")))))

(ert-deftest org-chronicle-sources-test-default-source ()
  "The default source is a registered id."
  (should (org-chronicle-sources--get org-chronicle-default-source)))

(ert-deftest org-chronicle-sources-test-kind-profile ()
  "Kind span-prop names and kind support are source-independent."
  (should (equal (org-chronicle-sources--kind-span-props 'person) '("BORN" . "DIED")))
  (should (equal (org-chronicle-sources--kind-span-props 'group) '("FOUNDED" . "DISBANDED")))
  (should (equal (org-chronicle-sources--kind-span-props 'place) '("BUILT" . "RAZED")))
  (should-error (org-chronicle-sources--check-kind 'topic) :type 'user-error)
  (should-not (org-chronicle-sources--check-kind 'person)))

(ert-deftest org-chronicle-sources-test-factgrid-entry ()
  "FactGrid is registered as a wikibase source with its probe-confirmed PIDs."
  (let ((s (org-chronicle-sources--get 'factgrid)))
    (should (eq (plist-get s :adapter) 'wikibase))
    (should (equal (plist-get s :key-property) "FACTGRID"))
    (should (equal (plist-get s :curie) "fg:"))
    (should (equal (plist-get s :base-uri) "https://database.factgrid.de"))
    (should (equal (plist-get s :sparql-endpoint)
                   "https://database.factgrid.de/sparql"))
    (should (equal (plist-get s :item-url-format)
                   "https://database.factgrid.de/wiki/Item:%s"))
    (should (equal (org-chronicle-sources--pid s :birthplace) "P82"))
    (should (equal (org-chronicle-sources--pid s :deathplace) "P168"))
    (should (equal (org-chronicle-sources--pid s :spouse) "P84"))
    (should (equal (org-chronicle-sources--span-pids s 'person)
                   (cons "P77" "P38")))
    (should (equal (org-chronicle-sources--span-pids s 'group)
                   (cons "P49" "P50")))))

(provide 'org-chronicle-sources-tests)
;;; org-chronicle-sources-tests.el ends here
