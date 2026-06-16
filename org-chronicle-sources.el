;;; org-chronicle-sources.el --- Multi-source import for org-chronicle -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: org-chronicle contributors
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; Source-neutral import layer: the source registry, the interactive
;; `org-chronicle-import' / `org-chronicle-reconcile' commands, the review
;; buffer, IMPORT-KEY idempotency, change classification/application, and the
;; source-agnostic date-candidate selector.  Per-source fetching lives in an
;; adapter (see `org-chronicle-wikibase.el').

;;; Code:

(require 'org-chronicle)
(require 'org-chronicle-wikibase)
(require 'cl-lib)
(require 'subr-x)
(require 'org-element)

(define-obsolete-variable-alias 'org-chronicle-wikidata-file
  'org-chronicle-sources-events-file "org-chronicle 0.5")

(defcustom org-chronicle-sources-events-file nil
  "File where imported events are filed.
When nil, defaults to \"imported/events.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle)

(defcustom org-chronicle-default-source 'wikidata
  "Default import source id, used as the prompt default in `org-chronicle-import'."
  :type 'symbol
  :group 'org-chronicle)


(defun org-chronicle-sources--events-file ()
  "Return the file imported events are written to.
Defaults to \"imported/events.org\" under `org-chronicle-root'."
  (or org-chronicle-sources-events-file
      (expand-file-name "imported/events.org" org-chronicle-root)))

(defun org-chronicle-sources--dates-equal-p (a b)
  "Non-nil when date strings A and B denote the same Y/M/D after parsing."
  (let ((da (org-chronicle--date-parse a))
        (db (org-chronicle--date-parse b)))
    (and da db
         (equal (plist-get da :year) (plist-get db :year))
         (equal (plist-get da :month) (plist-get db :month))
         (equal (plist-get da :day) (plist-get db :day)))))

(defun org-chronicle-sources--classify (change current)
  "Classify CHANGE against the CURRENT local value string (or nil).
Return `new' when CURRENT is empty, `same' when it matches the change value
\(dates compared by parsed value), or `conflict' otherwise."
  (let ((value (plist-get change :value))
        (prop (plist-get change :property)))
    (cond
     ((or (null current) (string-empty-p current)) 'new)
     ((if (member prop '("BORN" "DIED" "BUILT" "RAZED" "FOUNDED" "DISBANDED"))
          (org-chronicle-sources--dates-equal-p current value)
        (equal (string-trim current) (string-trim value)))
      'same)
     (t 'conflict))))

(defun org-chronicle-sources--add-source (url)
  "Add URL to the SOURCES property at point unless already present."
  (let* ((existing (org-chronicle--split (org-entry-get nil "SOURCES")))
         (merged (if (member url existing) existing (append existing (list url)))))
    (org-set-property "SOURCES" (org-chronicle--join merged))))

(defun org-chronicle-sources--apply-entity-change (change)
  "Set the entity property named in CHANGE at the heading at point.
Also records the provenance URL in SOURCES and marks TRUTH historical."
  (org-set-property (plist-get change :property) (plist-get change :value))
  (org-chronicle-sources--add-source (plist-get change :provenance))
  (unless (org-entry-get nil "TRUTH")
    (org-set-property "TRUTH" "historical")))

(defun org-chronicle-sources--event-change-string (change)
  "Return the Org heading text for the event in CHANGE.
Life events (those with a :life-event kind) go through
`org-chronicle--life-event-string'; other events use the generic
`org-chronicle--event-string'."
  (let* ((ev (plist-get change :event))
         (kind (plist-get ev :life-event)))
    (if kind
        (org-chronicle--life-event-string
         :title (plist-get ev :title)
         :kind kind
         :truth "historical"
         :date (plist-get ev :date)
         :subject (plist-get ev :subject)
         :people (plist-get ev :people)
         :location (plist-get ev :location)
         :sources (plist-get change :provenance))
      (org-chronicle--event-string
       :title (plist-get ev :title)
       :truth "historical"
       :date (plist-get ev :date)
       :date-end (plist-get ev :date-end)
       :people (plist-get ev :people)
       :location (plist-get ev :location)
       :sources (plist-get change :provenance)))))

(defun org-chronicle-sources--event-key (event subject-orgid source subject-qid)
  "Return the IMPORT-KEY for EVENT, or nil when a required id is missing.
SUBJECT-ORGID is the chronicle entity's org id; SOURCE is the import source and
SUBJECT-QID its id within that source.  Birth and death key on the chronicle
subject (source-independent); positions add the office id; marriage keys on the
sorted pair of both participants' CURIE-prefixed ids."
  (let ((kind (plist-get event :kind))
        (obj (plist-get event :object-qid))
        (curie (plist-get source :curie)))
    (pcase kind
      ("marriage"
       (and subject-qid obj
            (format "marriage:%s"
                    (mapconcat #'identity
                               (sort (list (concat curie subject-qid)
                                           (concat curie obj))
                                     #'string<)
                               ":"))))
      ("position"
       (and obj (format "position:%s:%s%s" subject-orgid curie obj)))
      (_ (and subject-orgid (format "%s:%s" kind subject-orgid))))))

(defun org-chronicle-sources--events-index ()
  "Return a hash mapping each IMPORT-KEY to a marker in the events file.
Scans `org-chronicle-sources--events-file'; an absent file yields an empty
table."
  (let ((file (org-chronicle-sources--events-file))
        (index (make-hash-table :test 'equal)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((key (org-entry-get nil "IMPORT-KEY")))
              (when key (puthash key (point-marker) index))))))))
    index))

(defun org-chronicle-sources--append-to-events-file (text key index)
  "Append heading TEXT to the events file, stamp KEY, and register it in INDEX.
Creates the file's directory if needed, assigns an id, normalizes, and saves."
  (let ((file (org-chronicle-sources--events-file)))
    (make-directory (file-name-directory file) t)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert text)
      (forward-line -1)
      (org-back-to-heading t)
      (when key (org-set-property "IMPORT-KEY" key))
      (org-id-get-create)
      (org-chronicle-normalize)
      (save-buffer)
      (when key (puthash key (point-marker) index)))))

(defun org-chronicle-sources--field-equal-p (prop proposed date-p)
  "Non-nil if PROP at point equals PROPOSED (a string or nil).
Empty and nil are equal; compare as dates when DATE-P is non-nil."
  (let* ((cur (org-entry-get nil prop))
         (cur (and cur (not (string-empty-p cur)) cur))
         (proposed (and proposed (not (string-empty-p proposed)) proposed)))
    (cond
     ((and (null cur) (null proposed)) t)
     ((or (null cur) (null proposed)) nil)
     (date-p (org-chronicle-sources--dates-equal-p cur proposed))
     (t (equal (string-trim cur) (string-trim proposed))))))

(defun org-chronicle-sources--classify-event (change existing)
  "Classify event CHANGE against the EXISTING marker (or nil).
Return `new' when EXISTING is nil, `same' when the managed date and location
fields match, or `conflict' otherwise."
  (if (null existing)
      'new
    (let ((ev (plist-get change :event)))
      (org-with-point-at existing
        (if (and (org-chronicle-sources--field-equal-p "DATE" (plist-get ev :date) t)
                 (org-chronicle-sources--field-equal-p "DATE-END" (plist-get ev :date-end) t)
                 (org-chronicle-sources--field-equal-p "LOCATION" (plist-get ev :location) nil))
            'same
          'conflict)))))

(defun org-chronicle-sources--apply-event-change (change index)
  "Apply event CHANGE idempotently using INDEX (IMPORT-KEY -> marker).
Update the keyed heading's managed properties in place when it exists,
otherwise append a new heading carrying the key."
  (let* ((key (plist-get change :key))
         (existing (and key (gethash key index)))
         (ev (plist-get change :event)))
    (if existing
        (org-with-point-at existing
          (org-set-property "DATE" (org-chronicle--ts (plist-get ev :date)))
          (if (plist-get ev :date-end)
              (org-set-property "DATE-END" (org-chronicle--ts (plist-get ev :date-end)))
            (org-delete-property "DATE-END"))
          (if (and (plist-get ev :location)
                   (not (string-empty-p (plist-get ev :location))))
              (org-set-property "LOCATION" (plist-get ev :location))
            (org-delete-property "LOCATION"))
          (org-chronicle-sources--add-source (plist-get change :provenance))
          (unless (org-entry-get nil "TRUTH")
            (org-set-property "TRUTH" "historical"))
          (save-buffer))
      (org-chronicle-sources--append-to-events-file
       (org-chronicle-sources--event-change-string change) key index))))

(defun org-chronicle-sources--apply-changes (changes index)
  "Write approved change plists to the chronicle at the heading at point.
CHANGES is a list of change plists; INDEX maps IMPORT-KEY to markers in the
events file.  Entity changes set properties on the heading; event changes are
written idempotently to the events file."
  (org-back-to-heading t)
  (dolist (change changes)
    (pcase (plist-get change :target)
      ('entity (org-chronicle-sources--apply-entity-change change))
      ('event (org-chronicle-sources--apply-event-change change index)))))

(defun org-chronicle-sources--change-label (change)
  "Return a one-line human label describing CHANGE."
  (pcase (plist-get change :target)
    ('entity (let ((alts (plist-get change :alternates)))
               (format "%-12s %s%s"
                       (plist-get change :property)
                       (plist-get change :value)
                       (if alts
                           (format "  (source also lists: %s)"
                                   (mapconcat #'identity alts "; "))
                         ""))))
    ('event (let ((ev (plist-get change :event)))
              (format "event       %s [%s]" (plist-get ev :title)
                      (plist-get ev :date))))))

(defun org-chronicle-sources--review-rows (changes)
  "Build review rows from proposed change plists.
CHANGES is a list of change plists.  Each row is a two-element list
\(STATE plist): STATE has :selected (default proposals that are new) and
:status (carried through)."
  (mapcar (lambda (c)
            (list (list :selected (and (plist-get c :default)
                                       (eq (plist-get c :status) 'new))
                        :status (plist-get c :status))
                  c))
          changes))

(defun org-chronicle-sources--selected-changes (rows)
  "Return the change plists from ROWS whose STATE has :selected non-nil."
  (delq nil (mapcar (lambda (row)
                      (when (plist-get (nth 0 row) :selected)
                        (nth 1 row)))
                    rows)))

(defvar-local org-chronicle-sources--rows nil
  "Review rows for the current review buffer.")

(defvar-local org-chronicle-sources--on-confirm nil
  "Function called with the selected changes when the review is confirmed.")

(defun org-chronicle-sources-review-toggle ()
  "Toggle selection of the row at point."
  (interactive)
  (let ((idx (- (line-number-at-pos) 3)))
    (when (and (>= idx 0) (< idx (length org-chronicle-sources--rows)))
      (let ((state (nth 0 (nth idx org-chronicle-sources--rows))))
        (plist-put state :selected (not (plist-get state :selected))))
      (org-chronicle-sources--render-review)
      (forward-line (+ idx 3)))))

(defun org-chronicle-sources-review-confirm ()
  "Invoke the confirm callback with selected rows and close the review."
  (interactive)
  (let ((selected (org-chronicle-sources--selected-changes
                   org-chronicle-sources--rows))
        (cb org-chronicle-sources--on-confirm))
    (quit-window)
    (when cb (funcall cb selected))))

(defvar org-chronicle-sources-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'org-chronicle-sources-review-toggle)
    (define-key map (kbd "RET") #'org-chronicle-sources-review-confirm)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-chronicle-sources-review-mode'.")

(define-derived-mode org-chronicle-sources-review-mode special-mode
  "Source-Review"
  "Major mode for reviewing proposed import changes.")

(defun org-chronicle-sources--render-review ()
  "Render `org-chronicle-sources--rows' into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Import review — TAB toggles, RET applies selected, q cancels\n\n")
    (dolist (row org-chronicle-sources--rows)
      (let* ((state (nth 0 row)) (change (nth 1 row)))
        (insert (format "[%s] %-8s %s\n"
                        (if (plist-get state :selected) "x" " ")
                        (plist-get state :status)
                        (org-chronicle-sources--change-label change)))))))

(defun org-chronicle-sources--review (changes on-confirm)
  "Open a review buffer; call ON-CONFIRM with the selected change plists.
CHANGES is the list of proposed change plists to present."
  (let ((buf (get-buffer-create "*Import Review*")))
    (with-current-buffer buf
      (org-chronicle-sources-review-mode)
      (setq org-chronicle-sources--rows
            (org-chronicle-sources--review-rows changes))
      (setq org-chronicle-sources--on-confirm on-confirm)
      (org-chronicle-sources--render-review)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun org-chronicle-sources--label-at-point ()
  "Return a person label when point is on a PARENTS or SPOUSE property value.
Return nil otherwise."
  (let ((el (org-element-at-point)))
    (when (eq (org-element-type el) 'node-property)
      (let ((key (org-element-property :key el)))
        (when (member key '("PARENTS" "SPOUSE"))
          (car (org-chronicle--split (org-element-property :value el))))))))

(defun org-chronicle-sources--classify-changes (changes marker subject-orgid source subject-qid index)
  "Stamp each proposed change with :status (and event :key); drop keyless events.
CHANGES is the raw change list.  Entity changes classify against the heading at
MARKER; event changes classify against INDEX by their IMPORT-KEY derived from
SUBJECT-ORGID, SOURCE, and SUBJECT-QID."
  (delq nil
        (mapcar
         (lambda (c)
           (pcase (plist-get c :target)
             ('entity
              (plist-put c :status
                         (org-chronicle-sources--classify
                          c (org-with-point-at marker
                                               (org-entry-get nil (plist-get c :property)))))
              c)
             ('event
              (let ((key (org-chronicle-sources--event-key
                          (plist-get c :event) subject-orgid source subject-qid)))
                (when key
                  (plist-put c :key key)
                  (plist-put c :status
                             (org-chronicle-sources--classify-event
                              c (gethash key index)))
                  c)))))
         changes)))

;;;###autoload
(defun org-chronicle-import ()
  "Import historical facts into the chronicle from a configured source.
Prompt for a source, resolve-or-create the entity, review proposed edits, and
write the approved set.  A heading accretes one key property per source."
  (interactive)
  (let* ((source-id (intern (completing-read
                             "Source: " (mapcar #'symbol-name
                                                (org-chronicle-sources--ids))
                             nil t nil nil
                             (symbol-name org-chronicle-default-source))))
         (source (org-chronicle-sources--get source-id))
         (key-prop (plist-get source :key-property))
         (promote (org-chronicle-sources--label-at-point))
         (heading-kind (unless promote
                         (save-excursion (org-back-to-heading t)
                                         (org-entry-get nil "KIND"))))
         (kind (cond (promote 'person)
                     (heading-kind (intern heading-kind))
                     (t (intern (completing-read "Kind: "
                                                 '("person" "place" "group")
                                                 nil t nil nil "person")))))
         (seed (or promote (save-excursion (org-back-to-heading t)
                                           (org-get-heading t t t t))))
         (stored (and heading-kind
                      (save-excursion (org-back-to-heading t)
                                      (org-entry-get nil key-prop)))))
    (org-chronicle-sources--check-kind kind)
    (let* ((qid (or stored (org-chronicle-wikibase--resolve source seed)))
           (marker (if heading-kind
                       (save-excursion (org-back-to-heading t) (point-marker))
                     (org-chronicle-wikibase--create-entity seed kind)))
           (rec (org-chronicle-wikibase--fetch-record source qid kind))
           (changes (org-chronicle-wikibase--record->changes rec seed)))
      (when (seq-empty-p changes)
        (user-error "Nothing to import for %s (%s)" seed qid))
      (let* ((subject-orgid (org-with-point-at marker (org-id-get-create)))
             (index (org-chronicle-sources--events-index)))
        (setq changes (org-chronicle-sources--classify-changes
                       changes marker subject-orgid source qid index))
        (org-chronicle-sources--review
         changes
         (lambda (selected)
           (org-with-point-at marker
			      (org-chronicle-sources--apply-changes selected index)
			      (when (buffer-file-name) (save-buffer))
			      (message "Imported %d change(s) for %s" (length selected) seed))))))))

(define-obsolete-function-alias 'org-chronicle-wikidata-import
  'org-chronicle-import "org-chronicle 0.5")

;;;###autoload
(defun org-chronicle-reconcile ()
  "Re-query a source linked from the heading and present drift as opt-in pulls.
When several source keys are present, prompt for which to reconcile."
  (interactive)
  (org-back-to-heading t)
  (let* ((present (cl-remove-if-not
                   (lambda (id)
                     (org-entry-get nil (plist-get (org-chronicle-sources--get id)
                                                   :key-property)))
                   (org-chronicle-sources--ids))))
    (unless present
      (user-error "No source key on this heading; run org-chronicle-import first"))
    (let* ((source-id (if (= (length present) 1) (car present)
                        (intern (completing-read
                                 "Source: " (mapcar #'symbol-name present)
                                 nil t))))
           (source (org-chronicle-sources--get source-id))
           (qid (org-entry-get nil (plist-get source :key-property)))
           (name (org-get-heading t t t t))
           (kind (let ((k (org-entry-get nil "KIND"))) (if k (intern k) 'person))))
      (org-chronicle-sources--check-kind kind)
      (let* ((marker (point-marker))
             (subject-orgid (org-with-point-at marker (org-id-get-create)))
             (index (org-chronicle-sources--events-index))
             (rec (org-chronicle-wikibase--fetch-record source qid kind))
             (changes (org-chronicle-sources--classify-changes
                       (org-chronicle-wikibase--record->changes rec name)
                       marker subject-orgid source qid index))
             (drift (cl-remove-if (lambda (c) (eq (plist-get c :status) 'same))
                                  changes)))
        (when (seq-empty-p drift)
          (user-error "No drift from %s for %s (%s)"
                      (plist-get source :label) name qid))
        (org-chronicle-sources--review
         drift
         (lambda (selected)
           (org-with-point-at marker
			      (org-chronicle-sources--apply-changes selected index)
			      (when (buffer-file-name) (save-buffer))
			      (message "Reconciled %d change(s) for %s" (length selected) name))))))))

(define-obsolete-function-alias 'org-chronicle-wikidata-reconcile
  'org-chronicle-reconcile "org-chronicle 0.5")

(defconst org-chronicle-sources--registry
  '((wikidata
     :label "Wikidata"
     :adapter wikibase
     :base-uri "http://www.wikidata.org"
     :sparql-endpoint "https://query.wikidata.org/sparql"
     :api-endpoint "https://www.wikidata.org/w/api.php"
     :key-property "WIKIDATA"
     :curie "wd:"
     :item-url-format "https://www.wikidata.org/wiki/%s"
     :label-language ("en")
     :property-map ( :span  ((person "P569" . "P570")
                             (place  "P571" . "P576")
                             (group  "P571" . "P576"))
                     :birthplace "P19" :deathplace "P20"
                     :father "P22" :mother "P25"
                     :spouse "P26" :position "P39"
                     :qual-start "P580" :qual-end "P582" ))
    (factgrid
     :label "FactGrid"
     :adapter wikibase
     :base-uri "https://database.factgrid.de"
     :sparql-endpoint "https://database.factgrid.de/sparql"
     :api-endpoint "https://database.factgrid.de/w/api.php"
     :key-property "FACTGRID"
     :curie "fg:"
     :item-url-format "https://database.factgrid.de/wiki/Item:%s"
     :label-language ("en")
     :property-map ( :span  ((person "P77" . "P38")     ; date of birth / death
                             (place  "P49" . "P50")     ; Begin date / End date
                             (group  "P49" . "P50"))    ; Begin date / End date
                     :birthplace "P82" :deathplace "P168"
                     :father "P141" :mother "P142"
                     :spouse "P84" :position "P165"
                     :qual-start "P49" :qual-end "P50" )))
  "Registry of import sources, keyed by source id symbol.")

(defun org-chronicle-sources--get (id)
  "Return the source plist for ID, or nil when unregistered."
  (alist-get id org-chronicle-sources--registry))

(defun org-chronicle-sources--ids ()
  "Return the list of registered source id symbols."
  (mapcar #'car org-chronicle-sources--registry))

(defun org-chronicle-sources--pid (source role)
  "Return the property id string for ROLE in SOURCE's property map."
  (plist-get (plist-get source :property-map) role))

(defun org-chronicle-sources--span-pids (source kind)
  "Return (START-PID . END-PID) for KIND from SOURCE's property map."
  (let ((cell (assq kind (plist-get (plist-get source :property-map) :span))))
    (cons (cadr cell) (cddr cell))))

(defconst org-chronicle-sources--kind-profiles
  '((person :start-prop "BORN"    :end-prop "DIED")
    (place  :start-prop "BUILT"   :end-prop "RAZED")
    (group  :start-prop "FOUNDED" :end-prop "DISBANDED"))
  "Per-kind chronicle span property names (source-independent).")

(defun org-chronicle-sources--kind-span-props (kind)
  "Return (START-PROP . END-PROP) chronicle property names for KIND."
  (let ((p (alist-get kind org-chronicle-sources--kind-profiles)))
    (cons (plist-get p :start-prop) (plist-get p :end-prop))))

(defun org-chronicle-sources--kind-file (kind)
  "Return the file new KIND entities are written to."
  (if (eq kind 'place) (org-chronicle--places-file) (org-chronicle--people-file)))

(defun org-chronicle-sources--check-kind (kind)
  "Signal a `user-error' unless KIND is supported."
  (unless (assq kind org-chronicle-sources--kind-profiles)
    (user-error "Cannot import for kind `%s' (supported: person, place, group)" kind)))

(provide 'org-chronicle-sources)
;;; org-chronicle-sources.el ends here
