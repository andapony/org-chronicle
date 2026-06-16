;;; org-chronicle-wikibase-live-tests.el --- Live Wikidata tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Integration tests that hit the live Wikidata endpoint.  Run explicitly with
;; `make test-live'.  Never part of `make' / `make all'.  Each test skips (not
;; fails) when Wikidata is unreachable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-chronicle-wikibase)

(require 'org-chronicle-sources)


(defun org-chronicle-wikibase-live--fetch-or-skip (thunk)
  "Call THUNK; on a Wikidata transport error, skip the test instead of failing.
Assertion failures in the caller still fail normally."
  (condition-case err
      (funcall thunk)
    (org-chronicle-wikibase-error
     (ert-skip (format "Wikidata unreachable — skipping: %S" err)))))

(ert-deftest org-chronicle-wikibase-test-live-search ()
  "Live `wbsearchentities' returns Ada Lovelace (Q7259)."
  :tags '(:wikidata-live)
  (let* ((cands (org-chronicle-wikibase-live--fetch-or-skip
                 (lambda () (org-chronicle-wikibase--search-request
                             (org-chronicle-sources--get 'wikidata) "Ada Lovelace"))))
         (hit (cl-find "Q7259" cands
                       :key (lambda (c) (plist-get c :qid)) :test #'equal)))
    (should hit)
    (should (stringp (plist-get hit :label)))
    (should (> (length (plist-get hit :label)) 0))
    (should (stringp (plist-get hit :description)))))

(ert-deftest org-chronicle-wikibase-test-live-fetch-ada ()
  "Live fetch of Ada Lovelace (Q7259) parses vitals, a spouse, and aliases."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-person "Q7259"))))
         (born (plist-get rec :born))
         (died (plist-get rec :died))
         (spouse (car (plist-get rec :spouses)))
         (aliases (plist-get rec :aliases)))
    (should (equal (plist-get born :year) 1815))
    (should (equal (plist-get born :month) 12))
    (should (equal (plist-get born :day) 10))
    (should (equal (plist-get died :year) 1852))
    (should (equal (plist-get died :month) 11))
    (should (equal (plist-get died :day) 27))
    (should (equal (plist-get rec :birthplace) "London"))
    (should (equal (plist-get rec :deathplace) "Marylebone"))
    (should (equal (plist-get rec :father) "Lord Byron"))
    (should (equal (plist-get rec :mother) "Anne Isabella Byron"))
    (should (equal (plist-get rec :label) "Ada Lovelace"))
    (should spouse)
    (should (string-match-p "\\`Q[0-9]+\\'" (plist-get spouse :qid)))
    (should (>= (length aliases) 1))
    (should (cl-every #'stringp aliases))))

(ert-deftest org-chronicle-wikibase-test-live-position-brannan ()
  "Live fetch of Samuel Brannan (Q936075) parses a position-held event."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-person "Q936075"))))
         (event (car (plist-get rec :events))))
    (should (equal (plist-get (plist-get rec :born) :year) 1819))
    (should event)
    (should (equal (plist-get event :kind) "position"))
    (should (string-match-p "\\`Q[0-9]+\\'" (plist-get event :qid)))))

(ert-deftest org-chronicle-wikibase-test-live-import-capstone ()
  "Live end-to-end import of Ada Lovelace into a sandbox writes entity + event."
  :tags '(:wikidata-live)
  (let* ((root (make-temp-file "octw-live" t))
         (org-chronicle-root (file-name-as-directory root))
         (org-chronicle-people-file (expand-file-name "people.org" root))
         (org-chronicle-sources-events-file (expand-file-name "imported/events.org" root))
         (org-chronicle-timeline-file (expand-file-name "timeline.org" root))
         (org-id-locations-file (expand-file-name ".org-id-locations" root))
         ;; Keep org-id out of the global locations file so the sandbox import
         ;; does not log "Could not read org-id-locations" for the temp dir.
         (org-id-track-globally nil))
    (unwind-protect
        (progn
          (with-temp-file org-chronicle-people-file
            (insert "* Ada Lovelace\n:PROPERTIES:\n:KIND: person\n:WIKIDATA: Q7259\n:END:\n"))
          (cl-letf (((symbol-function 'org-chronicle-sources--review)
                     (lambda (changes on-confirm) (funcall on-confirm changes)))
                    ((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (if (string-prefix-p "Source" prompt) "wikidata" ""))))
            (org-chronicle-wikibase-live--fetch-or-skip
             (lambda ()
               (with-current-buffer (find-file-noselect org-chronicle-people-file)
                 (goto-char (point-min))
                 (org-chronicle-import)))))
          (let ((people (with-temp-buffer
                          (insert-file-contents org-chronicle-people-file)
                          (buffer-string))))
            (should (string-match-p ":BORN: *<?1815-12-10" people))
            (should (string-match-p ":WIKIDATA: *Q7259" people)))
          (let ((events (with-temp-buffer
                          (insert-file-contents org-chronicle-sources-events-file)
                          (buffer-string))))
            (should (string-match-p "Birth of Ada Lovelace" events))
            (should (string-match-p ":IMPORT-KEY: *birth:" events))
            (should (string-match-p ":DATE: *<?1815-12-10" events))))
      (delete-directory root t))))

(ert-deftest org-chronicle-wikibase-test-live-newton-ranks ()
  "Live: Newton (Q935) resolves competing date statements by rank and precision."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-person "Q935"))))
         (born (plist-get rec :born))
         (died (plist-get rec :died)))
    (should (equal (plist-get born :year) 1643))
    (should (equal (plist-get born :month) 1))
    (should (equal (plist-get born :day) 4))
    (should (member "1642" (plist-get rec :born-alternates)))
    (should (equal (plist-get died :year) 1727))
    (should (equal (plist-get died :month) 3))
    (should (equal (plist-get died :day) 31))
    (should (member "1727" (plist-get rec :died-alternates)))))

(ert-deftest org-chronicle-wikibase-test-live-homer-coarse ()
  "Live: Homer (Q6691) has a century-precision BCE birth, dropped as unrepresentable."
  :tags '(:wikidata-live)
  (let ((rec (org-chronicle-wikibase-live--fetch-or-skip
              (lambda () (org-chronicle-wikibase--fetch-person "Q6691")))))
    (should (null (plist-get rec :born)))))

(ert-deftest org-chronicle-wikibase-test-live-place-sutro ()
  "Live: Sutro Baths (Q3505806) imports as a place with a BUILT span."
  :tags '(:wikidata-live)
  (let ((rec (org-chronicle-wikibase-live--fetch-or-skip
              (lambda () (org-chronicle-wikibase--fetch-record
                          (org-chronicle-sources--get 'wikidata) "Q3505806" 'place)))))
    (should (eq (plist-get rec :kind) 'place))
    (should (equal (plist-get (plist-get rec :start) :year) 1896))
    (should (null (plist-get rec :end)))))

(ert-deftest org-chronicle-wikibase-test-live-group-panam ()
  "Live: Pan Am (Q8681) imports as a group with a FOUNDED/DISBANDED span."
  :tags '(:wikidata-live)
  (let* ((rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-record
                           (org-chronicle-sources--get 'wikidata) "Q8681" 'group))))
         (start (plist-get rec :start)) (end (plist-get rec :end)))
    (should (eq (plist-get rec :kind) 'group))
    (should (equal (plist-get start :year) 1927))
    (should (equal (plist-get start :month) 3))
    (should (equal (plist-get start :day) 14))
    (should (equal (plist-get end :year) 1991))
    (should (equal (plist-get end :month) 12))
    (should (equal (plist-get end :day) 4))))

(ert-deftest org-chronicle-wikibase-test-live-factgrid-search ()
  "Live FactGrid `wbsearchentities' returns Adam Weishaupt (Q1308)."
  :tags '(:wikidata-live)
  (let* ((fg (org-chronicle-sources--get 'factgrid))
         (cands (org-chronicle-wikibase-live--fetch-or-skip
                 (lambda () (org-chronicle-wikibase--search-request fg "Adam Weishaupt")))))
    (should cands)
    (should (cl-some (lambda (c) (equal (plist-get c :qid) "Q1308")) cands))))

(ert-deftest org-chronicle-wikibase-test-live-factgrid-fetch-person ()
  "Live FactGrid fetch of Weishaupt (Q1308) parses a label and a birth date."
  :tags '(:wikidata-live)
  (let* ((fg (org-chronicle-sources--get 'factgrid))
         (rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-record fg "Q1308" 'person)))))
    (should (stringp (plist-get rec :label)))
    (should (plist-get rec :born))))

(ert-deftest org-chronicle-wikibase-test-live-factgrid-fetch-group ()
  "Live FactGrid fetch of the Illuminati (Q10677) parses an existence-span start."
  :tags '(:wikidata-live)
  (let* ((fg (org-chronicle-sources--get 'factgrid))
         (rec (org-chronicle-wikibase-live--fetch-or-skip
               (lambda () (org-chronicle-wikibase--fetch-record fg "Q10677" 'group)))))
    (should (eq (plist-get rec :kind) 'group))
    (should (stringp (plist-get rec :label)))
    (should (plist-get rec :start))))




(provide 'org-chronicle-wikibase-live-tests)
;;; org-chronicle-wikibase-live-tests.el ends here
