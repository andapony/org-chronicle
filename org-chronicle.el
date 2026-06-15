;;; org-chronicle.el --- Event timeline for historical fiction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/andapony/org-chronicle
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: outlines, calendar
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Capture, maintain, and view an event timeline for a historical-fiction
;; project that mingles real and invented events.  Events are Org headings
;; with property drawers in a timeline file; people, places, and groups can
;; be promoted to entity headings with aliases, existence spans, and kinship.
;; See doc/chronicle-schema.org for the full schema.
;;
;; Entry points:
;;
;;   `org-chronicle-add-event'    Capture a new event interactively into
;;                                the timeline file.
;;   `org-chronicle-capture'      Return an event heading string for an
;;                                org-capture template.
;;   `org-chronicle-normalize'    Tidy the event at point: mirror TRUTH to
;;                                a tag, canonicalize names, check the date.
;;   `org-chronicle-timeline'     Show the swimlane timeline filtered by
;;                                people, places, truth, and date range.
;;   `org-chronicle-add-person'   Create a person entity with aliases and
;;                                birth/death.
;;   `org-chronicle-add-place'    Create a place entity with an optional
;;                                build/raze span.
;;   `org-chronicle-add-group'    Create a group entity.
;;   `org-chronicle-promote'      Promote a recurring name into a person
;;                                entity.
;;   `org-chronicle-add-source'   Add a source (reading-list link or free
;;                                text) to the event at point.
;;   `org-chronicle-lint'         Report events that fall outside a
;;                                participant or place existence span.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)

(require 'crm)

(defgroup org-chronicle nil
  "Event timeline for historical fiction."
  :group 'org
  :prefix "org-chronicle-")

;;;; Date module
;;
;; All parsing, comparison, and formatting of dates goes through this one
;; module so a future fuzzy/BCE engine can replace its internals without
;; touching callers.  A date is a plist:
;;   (:year Y :month M|nil :day D|nil :precision day|month|year :sortkey TIME)

(defun org-chronicle--date-parse (s)
  "Parse string S into an internal date plist, or nil if it has no date.
S may be an Org timestamp with brackets and trailing tokens
\(e.g. \"<1863-07-04 Sat>\"); only the leading Y[-M[-D]] is read."
  (when (and (stringp s)
             (string-match
              "\\([0-9]\\{4\\}\\)\\(?:-\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-9]\\{1,2\\}\\)\\)?\\)?"
              s))
    (let* ((y (string-to-number (match-string 1 s)))
           (mo (and (match-string 2 s) (string-to-number (match-string 2 s))))
           (d (and (match-string 3 s) (string-to-number (match-string 3 s))))
           (precision (cond (d 'day) (mo 'month) (t 'year)))
           (sortkey (encode-time (list 0 0 0 (or d 1) (or mo 1) y nil -1 nil))))
      (list :year y :month mo :day d :precision precision :sortkey sortkey))))

(defun org-chronicle--date-lessp (a b)
  "Non-nil if date plist A sort strictly before date plist B."
  (time-less-p (plist-get a :sortkey) (plist-get b :sortkey)))

(defun org-chronicle--date-format (d)
  "Format internal date plist D as a string honoring its precision."
  (pcase (plist-get d :precision)
    ('year  (format "%04d" (plist-get d :year)))
    ('month (format "%04d-%02d" (plist-get d :year) (plist-get d :month)))
    (_      (format "%04d-%02d-%02d" (plist-get d :year)
                    (plist-get d :month) (plist-get d :day)))))

(defun org-chronicle--date-in-span-p (date from until)
  "Non-nil if DATE lies within [FROM, UNTIL]; nil bounds are open.
All arguments are date plists (or nil for FROM/UNTIL)."
  (and date
       (or (null from) (not (org-chronicle--date-lessp date from)))
       (or (null until) (not (org-chronicle--date-lessp until date)))))

;;;; Customization

(defcustom org-chronicle-timeline-file nil
  "File where new events are filed.
When nil, defaults to \"timeline.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle)

(defcustom org-chronicle-root "~/org/chronicle/"
  "Root directory of the chronicle project.
Every *.org file under this directory (recursively) is scanned for events
and entities, except files matching `org-chronicle-exclude' or carrying a
`#+CHRONICLE: ignore' keyword."
  :type 'directory
  :group 'org-chronicle
  :safe #'stringp)

(defcustom org-chronicle-exclude nil
  "List of regexps of paths to exclude from the chronicle scan.
Each candidate file path, taken relative to `org-chronicle-root', is tested
against these regexps; a match drops the file before it is opened.  Example:
\\='(\"/drafts/\" \"\\\\.draft\\\\.org\\\\='\")."
  :type '(repeat regexp)
  :group 'org-chronicle
  :safe (lambda (v) (and (listp v) (cl-every #'stringp v))))

(defcustom org-chronicle-multi-value-separator "; "
  "Separator written between multiple values in a property."
  :type 'string
  :group 'org-chronicle)

;;;; Value helpers

(defun org-chronicle--split (s)
  "Split `;'-separated property value S into a list of trimmed strings.
Return nil for nil or blank input."
  (when (and (stringp s) (not (string-blank-p s)))
    (mapcar #'string-trim (split-string s ";" t))))

(defun org-chronicle--join (values)
  "Join VALUES with `org-chronicle-multi-value-separator'."
  (mapconcat #'identity values org-chronicle-multi-value-separator))

;;;; Store: reading events

(defun org-chronicle--event-at-point ()
  "Return the event plist for the Org heading at point."
  (list :id (org-id-get)
        :title (org-get-heading t t t t)
        :truth (org-entry-get nil "TRUTH")
        :life-event (org-entry-get nil "LIFE-EVENT")
        :subject (org-chronicle--split (org-entry-get nil "SUBJECT"))
        :new-name (org-entry-get nil "NEW-NAME")
        :date (org-chronicle--date-parse (org-entry-get nil "DATE"))
        :date-end (org-chronicle--date-parse (org-entry-get nil "DATE-END"))
        :people (org-chronicle--split (org-entry-get nil "PEOPLE"))
        :topics (org-chronicle--split (org-entry-get nil "TOPICS"))
        :location (org-entry-get nil "LOCATION")
        :sources (org-entry-get nil "SOURCES")
        :marker (point-marker)))

(defun org-chronicle--buffer-events ()
  "Return a list of event plists for every dated heading in this buffer.
A heading is an event iff it has a non-empty DATE property."
  (org-with-wide-buffer
   (let (events)
     (org-map-entries
      (lambda ()
        (when (org-entry-get nil "DATE")
          (push (org-chronicle--event-at-point) events))))
     (nreverse events))))

(defun org-chronicle--file-events (file)
  "Return event plists from FILE, or nil if FILE is missing or ignored.
A missing file yields nil rather than opening a fresh buffer, which would
otherwise drop into Org's \"non-existent agenda file\" prompt."
  (let ((path (expand-file-name file)))
    (when (file-exists-p path)
      (with-current-buffer (find-file-noselect path)
        (unless (org-chronicle--file-ignored-p)
          (org-chronicle--buffer-events))))))

(defun org-chronicle--file-entities (file)
  "Return entity plists from FILE, or nil if FILE is missing or ignored.
A missing file yields nil rather than opening a fresh buffer, which would
otherwise drop into Org's \"non-existent agenda file\" prompt."
  (let ((path (expand-file-name file)))
    (when (file-exists-p path)
      (with-current-buffer (find-file-noselect path)
        (unless (org-chronicle--file-ignored-p)
          (org-chronicle--buffer-entities))))))

(defun org-chronicle--all-events ()
  "Return event plists from every source file under `org-chronicle-root'."
  (cl-loop for file in (org-chronicle--source-files)
           append (org-chronicle--file-events file)))

(defun org-chronicle--source-files ()
  "Return the Org files to gather from under `org-chronicle-root'.
Lists *.org recursively under the root (symlinks not followed), dropping any
whose path relative to the root matches a regexp in `org-chronicle-exclude'."
  (let ((root (expand-file-name org-chronicle-root)))
    (when (file-directory-p root)
      (cl-remove-if
       (lambda (f)
         ;; Match against a leading-slash-prefixed relative path so a pattern
         ;; like "/drafts/" anchors a top-level subtree.
         (let ((rel (concat "/" (file-relative-name f root))))
           (cl-some (lambda (re) (string-match-p re rel)) org-chronicle-exclude)))
       (directory-files-recursively root "\\.org\\'")))))

(defvar org-chronicle-people-file)

(defvar org-chronicle-places-file)

(defvar org-chronicle-topics-file)

(defun org-chronicle--timeline-file ()
  "Return the file new events are filed into (under the root by default)."
  (or org-chronicle-timeline-file
      (expand-file-name "timeline.org" org-chronicle-root)))

(defun org-chronicle--people-file ()
  "Return the file new person and group entities are filed into."
  (or org-chronicle-people-file
      (expand-file-name "people.org" org-chronicle-root)))

(defun org-chronicle--places-file ()
  "Return the file new place entities are filed into."
  (or org-chronicle-places-file
      (expand-file-name "places.org" org-chronicle-root)))

(defun org-chronicle--topics-file ()
  "Return the file new topic entities are filed into."
  (or org-chronicle-topics-file
      (expand-file-name "topics.org" org-chronicle-root)))

(defun org-chronicle--file-ignored-p ()
  "Non-nil if the current buffer opts out of the scan via `#+CHRONICLE: ignore'."
  (member "ignore"
          (cdr (assoc "CHRONICLE" (org-collect-keywords '("CHRONICLE"))))))

;;;; Entities

(defconst org-chronicle--span-props
  '((person "BORN" . "DIED")
    (place  "BUILT" . "RAZED")
    (group  "FOUNDED" . "DISBANDED"))
  "Map of entity KIND to its (START-PROPERTY . END-PROPERTY) span names.")

(defun org-chronicle--entity-at-point ()
  "Return the entity plist for the Org heading at point, or nil if no KIND."
  (let ((kind-s (org-entry-get nil "KIND")))
    (when kind-s
      (let* ((kind (intern kind-s))
             (span (alist-get kind org-chronicle--span-props))
             (from (and span (org-chronicle--date-parse
                              (org-entry-get nil (car span)))))
             (to (and span (org-chronicle--date-parse
                            (org-entry-get nil (cdr span))))))
        (list :id (org-id-get)
              :name (org-get-heading t t t t)
              :kind kind
              :aliases (org-chronicle--split (org-entry-get nil "ALIASES"))
              :member-of (org-chronicle--split (org-entry-get nil "MEMBER-OF"))
              :part-of (org-entry-get nil "PART-OF")
              :parents (org-chronicle--split (org-entry-get nil "PARENTS"))
              :spouse (org-chronicle--split (org-entry-get nil "SPOUSE"))
              :birthplace (org-entry-get nil "BIRTHPLACE")
              :deathplace (org-entry-get nil "DEATHPLACE")
              :span-from from
              :span-to to)))))

(defun org-chronicle--buffer-entities ()
  "Return entity plists for every KIND-bearing heading in this buffer."
  (org-with-wide-buffer
   (let (ents)
     (org-map-entries
      (lambda ()
        (when (org-entry-get nil "KIND")
          (push (org-chronicle--entity-at-point) ents))))
     (nreverse ents))))

(defun org-chronicle--all-entities ()
  "Return entity plists from every source file under `org-chronicle-root'."
  (cl-loop for file in (org-chronicle--source-files)
           append (org-chronicle--file-entities file)))

(defun org-chronicle--alias-index (entities)
  "Return a hash mapping each downcased name/alias to its canonical name.
ENTITIES is a list of entity plists; canonical name is `:name'."
  (let ((idx (make-hash-table :test #'equal)))
    (dolist (e entities idx)
      (let ((name (plist-get e :name)))
        (puthash (downcase name) name idx)
        (dolist (a (plist-get e :aliases))
          (puthash (downcase a) name idx))))))

(defun org-chronicle--canonical (name idx)
  "Resolve NAME to its canonical form via alias index IDX, else NAME itself."
  (or (and name (gethash (downcase name) idx)) name))

;;;; Relations

(defun org-chronicle--entity-by-id (id entities)
  "Return the entity plist in ENTITIES whose `:id' is ID, or nil."
  (cl-find id entities :key (lambda (e) (plist-get e :id)) :test #'equal))

(defun org-chronicle--group-member-names (group-id entities)
  "Return canonical names of ENTITIES that list GROUP-ID in `:member-of'."
  (cl-loop for e in entities
           when (member group-id (plist-get e :member-of))
           collect (plist-get e :name)))

(defun org-chronicle--place-descendant-names (place-id entities)
  "Return names of PLACE-ID and all places that are PART-OF it, transitively.
ENTITIES is the full entity list to search."
  (let ((acc '()) (frontier (list place-id)))
    (while frontier
      (let ((id (pop frontier)))
        (let ((e (org-chronicle--entity-by-id id entities)))
          (when (and e (not (member (plist-get e :name) acc)))
            (push (plist-get e :name) acc)
            (dolist (child entities)
              (when (equal (plist-get child :part-of) id)
                (push (plist-get child :id) frontier)))))))
    acc))

(defun org-chronicle--children-names (person-id entities)
  "Return names of ENTITIES that list PERSON-ID in `:parents'."
  (cl-loop for e in entities
           when (member person-id (plist-get e :parents))
           collect (plist-get e :name)))

;;;; Query

(cl-defun org-chronicle--filter-events (events _idx &key truth from until)
  "Filter EVENTS by TRUTH and date range; sorted ascending by date.
TRUTH is a list of allowed truth strings (nil = all).  FROM and UNTIL are
date plists bounding `:date' inclusively (nil = open).  The IDX argument
\(an alias index) is accepted for call-site symmetry but unused here."
  (let ((out (cl-remove-if-not
              (lambda (e)
                (and (plist-get e :date)
                     (or (null truth) (member (plist-get e :truth) truth))
                     (org-chronicle--date-in-span-p (plist-get e :date) from until)))
              events)))
    (sort out (lambda (a b)
                (org-chronicle--date-lessp (plist-get a :date) (plist-get b :date))))))

(defun org-chronicle--lane-names-for (name domain entities)
  "Return canonical names that entity NAME contributes to a lane.
DOMAIN selects expansion: `people' expands groups to their members;
`location' expands places to their descendants.  ENTITIES is the full
entity list."
  (let* ((idx (org-chronicle--alias-index entities))
         (canon (org-chronicle--canonical name idx))
         (ent (cl-find canon entities
                       :key (lambda (e) (plist-get e :name)) :test #'equal)))
    (cond
     ((and (eq domain 'people) ent (eq (plist-get ent :kind) 'group))
      (org-chronicle--group-member-names (plist-get ent :id) entities))
     ((and (eq domain 'location) ent (eq (plist-get ent :kind) 'place))
      (org-chronicle--place-descendant-names (plist-get ent :id) entities))
     (t (list canon)))))

(defun org-chronicle--build-lane (name domain entities _mode)
  "Build a single collapsed lane plist for NAME in DOMAIN over ENTITIES."
  (let* ((idx (org-chronicle--alias-index entities))
         (canon (org-chronicle--canonical name idx)))
    (list :label canon
          :domain domain
          :names (org-chronicle--lane-names-for name domain entities))))

(defun org-chronicle--build-lanes-for (name domain entities mode)
  "Build a list of lane plists for NAME in DOMAIN over ENTITIES.
MODE `:expand' yields one lane per resolved member/descendant of a
group or parent place; `:collapse' yields a single lane (see
`org-chronicle--build-lane')."
  (if (eq mode :expand)
      (mapcar (lambda (n) (list :label n :domain domain :names (list n)))
              (org-chronicle--lane-names-for name domain entities))
    (list (org-chronicle--build-lane name domain entities mode))))

(defun org-chronicle--event-in-lane-p (event lane idx)
  "Non-nil if EVENT belongs in LANE, resolving names via alias index IDX."
  (let ((names (plist-get lane :names)))
    (pcase (plist-get lane :domain)
      ('people
       (cl-some (lambda (p) (member (org-chronicle--canonical p idx) names))
                (plist-get event :people)))
      ('location
       (and (plist-get event :location)
            (member (org-chronicle--canonical (plist-get event :location) idx)
                    names)))
      ('topic
       (cl-some (lambda (tp) (member (org-chronicle--canonical tp idx) names))
                (plist-get event :topics))))))

;;;; Render
;;
;; Pure: turns EVENTS + LANES into a swimlane string (time vertical, lanes
;; as columns).  No buffer side effects; the view command (Task 8) wraps it.

(defface org-chronicle-historical '((t :inherit default))
  "Face for historical events in the timeline view.")
(defface org-chronicle-fictionalized '((t :inherit warning))
  "Face for fictionalized events in the timeline view.")
(defface org-chronicle-fictional '((t :inherit font-lock-keyword-face))
  "Face for fictional events in the timeline view.")

(defconst org-chronicle--date-col-width 12)

(defun org-chronicle--truth-marker (truth)
  "Return the short marker string for TRUTH."
  (pcase truth
    ("historical" "[H]")
    ("fictionalized" "[~]")
    ("fictional" "[F]")
    (_ "[?]")))

(defun org-chronicle--truth-face (truth)
  "Return the face symbol for TRUTH."
  (pcase truth
    ("fictionalized" 'org-chronicle-fictionalized)
    ("fictional" 'org-chronicle-fictional)
    (_ 'org-chronicle-historical)))

(defun org-chronicle--pad (s width)
  "Pad or truncate string S to exactly WIDTH columns."
  (truncate-string-to-width (concat s (make-string width ?\s)) width))

(defconst org-chronicle--life-event-kinds
  '("birth" "death" "marriage" "name-change")
  "Recognized values of the LIFE-EVENT property.")

(defcustom org-chronicle-life-event-glyphs
  '(("birth" . "⊕") ("death" . "⊗") ("marriage" . "⚭") ("name-change" . "↦"))
  "Alist mapping a life-event kind to its glyph in the timeline view."
  :type '(alist :key-type string :value-type string)
  :group 'org-chronicle)

(defun org-chronicle--life-event-glyph (kind)
  "Return the configured glyph for life-event KIND, or nil if KIND is unknown."
  (and kind (cdr (assoc kind org-chronicle-life-event-glyphs))))

(defcustom org-chronicle-topic-glyphs nil
  "Alist mapping a topic name to its glyph in the timeline view.
Keys are canonical topic names; values are short display strings."
  :type '(alist :key-type string :value-type string)
  :group 'org-chronicle)

(defcustom org-chronicle-topic-faces nil
  "Alist mapping a topic name to a face for that topic's timeline column.
When a topic has no entry, its cells fall back to the truth face."
  :type '(alist :key-type string :value-type face)
  :group 'org-chronicle)

(defun org-chronicle--topic-glyph (topic)
  "Return the configured glyph for TOPIC, or nil when none is set."
  (and topic (cdr (assoc topic org-chronicle-topic-glyphs))))

(defun org-chronicle--topic-face (topic)
  "Return the configured face for TOPIC, or nil when none is set."
  (and topic (cdr (assoc topic org-chronicle-topic-faces))))

(defun org-chronicle--topic-cell-text (event topic)
  "Return propertized cell text for EVENT in a TOPIC lane.
Prefixes the topic glyph (when set) and uses the topic face, falling back
to the event's truth face."
  (let* ((glyph (org-chronicle--topic-glyph topic))
         (face (or (org-chronicle--topic-face topic)
                   (org-chronicle--truth-face (plist-get event :truth))))
         (s (format "%s%s %s"
                    (if glyph (concat glyph " ") "")
                    (plist-get event :title)
                    (org-chronicle--truth-marker (plist-get event :truth)))))
    (propertize s 'face face
                'org-chronicle-marker (plist-get event :marker))))

(defun org-chronicle--cell-text-for-lane (event lane)
  "Return cell text for EVENT as shown in LANE.
Topic lanes use `org-chronicle--topic-cell-text'; all other domains use
the default `org-chronicle--cell-text'."
  (if (eq (plist-get lane :domain) 'topic)
      (org-chronicle--topic-cell-text event (plist-get lane :label))
    (org-chronicle--cell-text event)))

(defun org-chronicle--cell-text (event)
  "Return propertized cell text (kind glyph, title, truth marker) for EVENT."
  (let* ((glyph (org-chronicle--life-event-glyph (plist-get event :life-event)))
         (s (format "%s%s %s"
                    (if glyph (concat glyph " ") "")
                    (plist-get event :title)
                    (org-chronicle--truth-marker (plist-get event :truth)))))
    (propertize s 'face (org-chronicle--truth-face (plist-get event :truth))
                'org-chronicle-marker (plist-get event :marker))))

(defun org-chronicle--render (events lanes idx col-width)
  "Render EVENTS into a swimlane string across LANES.
IDX is an alias index; COL-WIDTH is the width of each lane column.  Rows
are dates (ascending); each lane column shows that lane's events on that
date.  EVENTS are assumed already filtered and sorted ascending."
  (let* ((dcw org-chronicle--date-col-width)
         (header (concat (org-chronicle--pad "DATE" dcw)
                         (mapconcat (lambda (l) (org-chronicle--pad
                                                 (upcase (plist-get l :label)) col-width))
                                    lanes "")))
         (rule (make-string (string-width header) ?-))
         (lines (list rule header)))
    (let ((by-date '()))
      (dolist (e events)
        (let ((key (org-chronicle--date-format (plist-get e :date))))
          (push e (alist-get key by-date nil nil #'equal))))
      (setq by-date (nreverse by-date))
      (dolist (cell by-date)
        (let* ((date (car cell))
               (day-events (reverse (cdr cell)))
               (row (org-chronicle--pad date dcw)))
          (dolist (lane lanes)
            (let* ((hits (cl-remove-if-not
                          (lambda (e) (org-chronicle--event-in-lane-p e lane idx))
                          day-events))
                   (txt (mapconcat (lambda (e) (org-chronicle--cell-text-for-lane e lane)) hits " / ")))
              (setq row (concat row (org-chronicle--pad txt col-width)))))
          (push row lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; View

(defcustom org-chronicle-lane-column-width 22
  "Width in columns of each lane in the timeline view."
  :type 'integer
  :group 'org-chronicle)

(defun org-chronicle--lanes-from-params (people locations topics entities mode)
  "Build the list of lane plists from PEOPLE, LOCATIONS, and TOPICS name lists.
MODE is `:collapse' or `:expand'; ENTITIES is the entity list."
  (append
   (cl-loop for n in people
            append (org-chronicle--build-lanes-for n 'people entities mode))
   (cl-loop for n in locations
            append (org-chronicle--build-lanes-for n 'location entities mode))
   (cl-loop for n in topics
            append (org-chronicle--build-lanes-for n 'topic entities mode))))

(cl-defun org-chronicle--compose (&key people locations topics truth from until
                                       (mode :collapse)
                                       (root nil root-p)
                                       (exclude nil exclude-p))
  "Return the rendered timeline string for the given filters.
PEOPLE/LOCATIONS/TOPICS are name lists naming lanes; TRUTH a list of
allowed truth strings; FROM/UNTIL date strings; MODE `:collapse' or
`:expand'.  ROOT and EXCLUDE, when supplied, override
`org-chronicle-root' and `org-chronicle-exclude' for this gather
\(used to keep view refresh consistent with dir-local values)."
  (let ((org-chronicle-root (if root-p root org-chronicle-root))
        (org-chronicle-exclude (if exclude-p exclude org-chronicle-exclude)))
    (let* ((entities (org-chronicle--all-entities))
           (idx (org-chronicle--alias-index entities))
           (lanes (org-chronicle--lanes-from-params people locations topics entities mode))
           (events (org-chronicle--filter-events
                    (org-chronicle--all-events) idx
                    :truth truth
                    :from (and from (org-chronicle--date-parse from))
                    :until (and until (org-chronicle--date-parse until)))))
      (org-chronicle--render events lanes idx org-chronicle-lane-column-width))))

(defvar org-chronicle-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-chronicle-view-goto)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'org-chronicle-view-refresh)
    map)
  "Keymap for `org-chronicle-view-mode'.")

(define-derived-mode org-chronicle-view-mode special-mode "Chronicle"
  "Major mode for the read-only swimlane timeline view.")

(defvar-local org-chronicle--view-args nil
  "The plist of arguments that produced the current view, for refresh.")

(defun org-chronicle-view-goto ()
  "Jump to the event heading for the cell at point."
  (interactive)
  (let ((m (get-text-property (point) 'org-chronicle-marker)))
    (if (and m (marker-buffer m))
        (progn (pop-to-buffer (marker-buffer m))
               (goto-char m)
               (org-reveal))
      (message "No event at point"))))

(defun org-chronicle-view-refresh ()
  "Recompute the current timeline view."
  (interactive)
  (when org-chronicle--view-args
    (apply #'org-chronicle-timeline org-chronicle--view-args)))

;;;###autoload
(cl-defun org-chronicle-timeline (&key people locations topics truth from until
                                       (mode :collapse)
                                       (root nil root-p)
                                       (exclude nil exclude-p))
  "Display a swimlane timeline of events arranged into lanes.
PEOPLE, LOCATIONS, and TOPICS are lists of names that become lanes;
results are filtered by TRUTH and the FROM/UNTIL date range.  MODE is
`:collapse' (default) or `:expand' for groups/parent places.  ROOT and
EXCLUDE, when supplied, override `org-chronicle-root' and
`org-chronicle-exclude' (used to preserve dir-local values on refresh).
Interactively, prompts for people, locations, topics, and a truth
subset, completing against the names used in events and promoted
entities."
  (interactive
   (list :people (org-chronicle--read-names
                  "People/groups (lanes): "
                  (org-chronicle--known-people) 'org-chronicle-person)
         :locations (org-chronicle--read-names
                     "Places (lanes): "
                     (org-chronicle--known-locations) 'org-chronicle-place)
         :topics (org-chronicle--read-names
                  "Topics (lanes): "
                  (org-chronicle--known-topics) 'org-chronicle-topic)
         :truth (let ((v (completing-read-multiple
                          "Truth (blank=all): "
                          '("historical" "fictionalized" "fictional"))))
                  (and v (delete "" v)))
         :mode (if (y-or-n-p "Expand groups into member lanes? ") :expand :collapse)))
  (let* ((root (if root-p root org-chronicle-root))
         (exclude (if exclude-p exclude org-chronicle-exclude))
         (args (list :people people :locations locations :topics topics
                     :truth truth :from from :until until :mode mode
                     :root root :exclude exclude))
         (text (org-chronicle--compose :people people :locations locations :topics topics
                                       :truth truth :from from :until until :mode mode
                                       :root root :exclude exclude)))
    (with-current-buffer (get-buffer-create "*org-chronicle*")
      (org-chronicle-view-mode)
      (setq org-chronicle--view-args args)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun org-dblock-write:chronicle (params)
  "Org dynamic-block writer: fill the block with a timeline per PARAMS.
PARAMS keys mirror `org-chronicle-timeline' (:people :topics :locations :truth
:from :until :mode)."
  (insert (apply #'org-chronicle--compose params)))

;; Test-facing alias for the dynamic-block writer.
(defalias 'org-chronicle-dblock-write 'org-dblock-write:chronicle)

;;;; Capture

(defconst org-chronicle--truth-values '("historical" "fictionalized" "fictional"))

(defun org-chronicle--ts (date-string)
  "Wrap DATE-STRING in Org active-timestamp brackets if not already bracketed."
  (if (string-match-p "\\`[<\\[]" date-string)
      date-string
    (format "<%s>" date-string)))

(cl-defun org-chronicle--event-string (&key title truth date date-end
                                            people topics location sources)
  "Return the Org heading text for a new event with the given fields.
TITLE, TRUTH, DATE are strings; PEOPLE and TOPICS are optional lists of strings.
DATE-END, LOCATION, and SOURCES are optional strings."
  (concat
   (format "* %s\n" title)
   ":PROPERTIES:\n"
   (format ":TRUTH:    %s\n" (or truth "historical"))
   (format ":DATE:     %s\n" (org-chronicle--ts date))
   (when (and date-end (not (string-blank-p date-end)))
     (format ":DATE-END: %s\n" (org-chronicle--ts date-end)))
   (when people (format ":PEOPLE:   %s\n" (org-chronicle--join people)))
   (when topics (format ":TOPICS:   %s\n" (org-chronicle--join topics)))
   (when (and location (not (string-blank-p location)))
     (format ":LOCATION: %s\n" location))
   (when (and sources (not (string-blank-p sources)))
     (format ":SOURCES:  %s\n" sources))
   ":END:\n"))

(defun org-chronicle--collect-names (event-key entity-kinds)
  "Return a sorted, de-duplicated list of names for completion.
Gathers the EVENT-KEY value of every event (a string, or a list of
strings) and the name plus aliases of every entity whose `:kind' is in
ENTITY-KINDS."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (e (ignore-errors (org-chronicle--all-events)))
      (let ((v (plist-get e event-key)))
        (cond ((listp v) (dolist (x v) (when x (puthash x t names))))
              (v (puthash v t names)))))
    (dolist (e (ignore-errors (org-chronicle--all-entities)))
      (when (memq (plist-get e :kind) entity-kinds)
        (puthash (plist-get e :name) t names)
        (dolist (a (plist-get e :aliases)) (puthash a t names))))
    (sort (hash-table-keys names) #'string<)))

(defun org-chronicle--known-people ()
  "Return known person and group names from events and entities."
  (org-chronicle--collect-names :people '(person group)))

(defun org-chronicle--known-locations ()
  "Return known location names from event locations and place entities."
  (org-chronicle--collect-names :location '(place)))

(defun org-chronicle--known-topics ()
  "Return known topic names from event topics and topic entities."
  (org-chronicle--collect-names :topics '(topic)))

(defun org-chronicle--completion-table (candidates category)
  "Return a completion table over CANDIDATES tagged with completion CATEGORY.
Tagging the table with a category lets the user's completion framework
annotate and configure it via `completion-category-overrides', keeping
all behaviour inside the standard `completing-read' machinery."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string predicate))))

(defun org-chronicle--read-names (prompt candidates category)
  "Read multiple names with completion; return a list (nil when blank).
PROMPT is the minibuffer prompt, CANDIDATES the completion set, CATEGORY
the completion category.  Entries are separated by
`org-chronicle-multi-value-separator', so names that themselves contain a
comma (e.g. \"Vicksburg, Mississippi\") are not split, and new names not
in CANDIDATES are still accepted."
  (let ((crm-separator
         (concat "[ \t]*"
                 (regexp-quote (string-trim org-chronicle-multi-value-separator))
                 "[ \t]*")))
    (delete "" (completing-read-multiple
                prompt
                (org-chronicle--completion-table candidates category)
                nil nil))))

(defun org-chronicle--read-location ()
  "Read a single location with completion; return nil when blank."
  (let ((loc (completing-read
              "Location (blank to skip): "
              (org-chronicle--completion-table
               (org-chronicle--known-locations) 'org-chronicle-place)
              nil nil)))
    (and (not (string-blank-p loc)) loc)))

(defun org-chronicle--read-date (prompt)
  "Read a required date string with PROMPT; signal a `user-error' if blank."
  (let ((date (read-string prompt)))
    (if (string-blank-p date)
        (user-error "A date is required")
      date)))




(defun org-chronicle--read-people ()
  "Prompt for participants with completion against known people; return a list."
  (org-chronicle--read-names
   (concat "People (" (string-trim org-chronicle-multi-value-separator)
           "-separated, blank to skip): ")
   (org-chronicle--known-people) 'org-chronicle-person))

(defun org-chronicle--read-topics (&optional prompt)
  "Prompt for topics with completion against known topics; return a list.
PROMPT defaults to \"Topics (blank to skip): \"."
  (org-chronicle--read-names
   (or prompt "Topics (blank to skip): ")
   (org-chronicle--known-topics) 'org-chronicle-topic))

(defun org-chronicle--append-event (text)
  "Append event heading TEXT to the timeline file; add an id, normalize, save."
  (with-current-buffer (find-file-noselect (org-chronicle--timeline-file))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert text)
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-chronicle-normalize)
    (save-buffer)))

;;;###autoload
(defun org-chronicle-add-event ()
  "Interactively capture a new event into the chronicle timeline file."
  (interactive)
  (let* ((title (read-string "Event title: "))
         (date (org-chronicle--read-date "Date (YYYY-MM-DD): "))
         (date-end (read-string "End date (blank for none): "))
         (truth (completing-read "Truth: " org-chronicle--truth-values nil t
                                 nil nil "historical"))
         (people (org-chronicle--read-people))
         (location (org-chronicle--read-location))
         (topics (org-chronicle--read-topics))
         (text (org-chronicle--event-string
                :title title :truth truth :date date :date-end date-end
                :people people :location location :topics topics)))
    (org-chronicle--append-event text)
    (message "Captured: %s" title)))

;;;###autoload
(defun org-chronicle-capture ()
  "Return a new event heading string for use in `org-capture' templates.
Prompts for the same fields as `org-chronicle-add-event'."
  (let ((title (read-string "Event title: "))
        (date (org-chronicle--read-date "Date (YYYY-MM-DD): "))
        (truth (completing-read "Truth: " org-chronicle--truth-values nil t
                                nil nil "historical"))
        (people (org-chronicle--read-people))
        (location (org-chronicle--read-location))
        (topics (org-chronicle--read-topics)))
    (org-chronicle--event-string :title title :truth truth :date date
                                 :people people :location location :topics topics)))

;;;###autoload
(defun org-chronicle-normalize ()
  "Validate and tidy the event heading at point.
Mirror TRUTH and LIFE-EVENT to tags, canonicalize PEOPLE/SUBJECT/TOPICS/LOCATION
names via aliases, accrue a name-change's NEW-NAME as a subject alias, and
warn if DATE does not parse."
  (interactive)
  (org-back-to-heading t)
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (truth (org-entry-get nil "TRUTH"))
         (life (org-entry-get nil "LIFE-EVENT"))
         (date (org-entry-get nil "DATE")))
    (when (and date (null (org-chronicle--date-parse date)))
      (message "org-chronicle: DATE %S does not parse" date))
    (dolist (prop '("PEOPLE" "SUBJECT" "TOPICS"))
      (let ((vals (org-chronicle--split (org-entry-get nil prop))))
        (when vals
          (org-set-property
           prop (org-chronicle--join
                 (mapcar (lambda (p) (org-chronicle--canonical p idx)) vals))))))
    (let ((loc (org-entry-get nil "LOCATION")))
      (when loc
        (org-set-property "LOCATION" (org-chronicle--canonical loc idx))))
    (let* ((managed (append org-chronicle--truth-values
                            (mapcar #'org-chronicle--life-event-tag
                                    org-chronicle--life-event-kinds)))
           (kept (cl-remove-if (lambda (tg) (member tg managed)) (org-get-tags nil t)))
           (added (append (and (member truth org-chronicle--truth-values) (list truth))
                          (and (member life org-chronicle--life-event-kinds)
                               (list (org-chronicle--life-event-tag life))))))
      (org-set-tags (append added kept)))
    (when (equal life "name-change")
      (let ((subject (car (org-chronicle--split (org-entry-get nil "SUBJECT"))))
            (new-name (org-entry-get nil "NEW-NAME")))
        (when (and subject new-name)
          (org-chronicle--accrue-alias subject new-name))))))

;;;; Entity creation

(defcustom org-chronicle-people-file nil
  "File where new person and group entities are filed.
When nil, defaults to \"people.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle)

(defcustom org-chronicle-places-file nil
  "File where new place entities are filed.
When nil, defaults to \"places.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle)

(defcustom org-chronicle-topics-file nil
  "File where new topic entities are filed.
When nil, defaults to \"topics.org\" under `org-chronicle-root'."
  :type '(choice (const :tag "Default under root" nil) file)
  :group 'org-chronicle)

(cl-defun org-chronicle--entity-string (&key name kind aliases props)
  "Return the Org heading text for a new entity named NAME.
KIND is a symbol; ALIASES a list of strings; PROPS an alist of
\(PROP . VALUE) extra properties (only non-blank values are written)."
  (concat
   (format "* %s\n" name)
   ":PROPERTIES:\n"
   (format ":KIND:    %s\n" kind)
   (when aliases (format ":ALIASES: %s\n" (org-chronicle--join aliases)))
   (mapconcat (lambda (pv)
                (if (and (cdr pv) (not (string-blank-p (cdr pv))))
                    (format ":%s:%s%s\n" (car pv)
                            (make-string (max 1 (- 8 (length (car pv)))) ?\s)
                            (cdr pv))
                  ""))
              props "")
   ":END:\n"))

(defun org-chronicle--file-entity (file text)
  "Append entity TEXT to FILE, create an ID, and save."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert text)
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (save-buffer)
    (org-id-get)))

(defun org-chronicle--find-entity-by-name (name entities idx)
  "Return an existing entity in ENTITIES matching NAME by name or alias, else nil.
IDX is the alias index for ENTITIES; matching is case-insensitive."
  (let ((canon (org-chronicle--canonical name idx)))
    (cl-find canon entities
             :key (lambda (e) (plist-get e :name)) :test #'equal)))

(defun org-chronicle--groups (entities)
  "Return the entities in ENTITIES whose `:kind' is `group'."
  (cl-remove-if-not (lambda (e) (eq (plist-get e :kind) 'group)) entities))

(defun org-chronicle--group-id-for-name (name entities idx)
  "Return the id of the group named NAME in ENTITIES, or nil if none.
NAME, resolved through alias index IDX, must name an entity of kind
`group'."
  (let ((ent (org-chronicle--find-entity-by-name name entities idx)))
    (and ent (eq (plist-get ent :kind) 'group) (plist-get ent :id))))

(defun org-chronicle--read-groups (entities idx)
  "Prompt for groups by name; return a list of their ids for `:MEMBER-OF'.
Completes against the group names in ENTITIES (resolved via alias index
IDX); entries that do not name a known group are skipped.  Returns nil
without prompting when ENTITIES has no groups."
  (let ((names (mapcar (lambda (e) (plist-get e :name))
                       (org-chronicle--groups entities))))
    (when names
      (delq nil
            (mapcar (lambda (n) (org-chronicle--group-id-for-name n entities idx))
                    (org-chronicle--read-names
                     "Member of groups (blank to skip): "
                     names 'org-chronicle-group))))))

(defun org-chronicle--check-duplicate (name entities idx)
  "Guard against creating a duplicate entity for NAME.
If an entity in ENTITIES already matches NAME (by name or alias, via the
alias index IDX), ask whether to create another; declining signals a
`user-error'.  Return t when it is safe to proceed."
  (let ((dup (org-chronicle--find-entity-by-name name entities idx)))
    (when (and dup
               (not (yes-or-no-p
                     (format "Entity \"%s\" already exists; create \"%s\" anyway? "
                             (plist-get dup :name) name))))
      (user-error "Not creating duplicate of \"%s\"" (plist-get dup :name))))
  t)




;;;###autoload
(defun org-chronicle-add-person (name)
  "Create a person entity NAME in the chronicle people file.
Prompts for aliases, birth, death, and group membership; refuses to
create a duplicate of an existing entity without confirmation; and offers
to capture a birth life event."
  (interactive "sPerson name: ")
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities)))
    (org-chronicle--check-duplicate name entities idx)
    (let ((aliases (completing-read-multiple "Aliases (blank to skip): " nil))
          (born (read-string "Born (YYYY-MM-DD, blank to skip): "))
          (died (read-string "Died (YYYY-MM-DD, blank to skip): "))
          (groups (org-chronicle--read-groups entities idx)))
      (org-chronicle--file-entity
       (org-chronicle--people-file)
       (org-chronicle--entity-string
        :name name :kind 'person :aliases aliases
        :props `(("BORN" . ,(and (not (string-blank-p born)) (org-chronicle--ts born)))
                 ("DIED" . ,(and (not (string-blank-p died)) (org-chronicle--ts died)))
                 ("MEMBER-OF" . ,(and groups (org-chronicle--join groups))))))
      (message "Added person: %s" name))
    (when (y-or-n-p "Create a birth event now? ")
      (org-chronicle-add-life-event "birth" (list name)))))

;;;###autoload
(defun org-chronicle-add-place (name)
  "Create a place entity NAME in the chronicle places file.
Prompts for an optional build/raze span.  Refuses to create a duplicate
of an existing entity without confirmation."
  (interactive "sPlace name: ")
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities)))
    (org-chronicle--check-duplicate name entities idx)
    (let ((built (read-string "Built (blank to skip): "))
          (razed (read-string "Razed (blank to skip): ")))
      (org-chronicle--file-entity
       (org-chronicle--places-file)
       (org-chronicle--entity-string
        :name name :kind 'place
        :props `(("BUILT" . ,(and (not (string-blank-p built)) (org-chronicle--ts built)))
                 ("RAZED" . ,(and (not (string-blank-p razed)) (org-chronicle--ts razed))))))
      (message "Added place: %s" name))))

;;;###autoload
(defun org-chronicle-add-topic (name)
  "Create a topic entity NAME in the chronicle topics file.
Prompts for optional aliases and a description.  Refuses to create a
duplicate of an existing entity without confirmation."
  (interactive "sTopic name: ")
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities)))
    (org-chronicle--check-duplicate name entities idx)
    (let ((aliases (completing-read-multiple "Aliases (blank to skip): " nil))
          (description (read-string "Description (blank to skip): ")))
      (org-chronicle--file-entity
       (org-chronicle--topics-file)
       (org-chronicle--entity-string
        :name name :kind 'topic :aliases aliases
        :props `(("DESCRIPTION" . ,(and (not (string-blank-p description)) description)))))
      (message "Added topic: %s" name))))

;;;###autoload
(defun org-chronicle-add-group (name)
  "Create a group entity NAME in the chronicle people file.
Refuses to create a duplicate of an existing entity without confirmation."
  (interactive "sGroup name: ")
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities)))
    (org-chronicle--check-duplicate name entities idx)
    (org-chronicle--file-entity
     (org-chronicle--people-file)
     (org-chronicle--entity-string :name name :kind 'group))
    (message "Added group: %s" name)))

;;;###autoload
(defun org-chronicle-promote ()
  "Promote the symbol/name at point (or a prompted name) into a person entity.
Seeds ALIASES with the literal text promoted.  Refuses to create a
duplicate of an existing entity without confirmation."
  (interactive)
  (let* ((variant (or (thing-at-point 'symbol t)
                      (read-string "Promote name: ")))
         (canonical (read-string "Canonical name: " variant))
         (entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities)))
    (org-chronicle--check-duplicate canonical entities idx)
    (org-chronicle--file-entity
     (org-chronicle--people-file)
     (org-chronicle--entity-string
      :name canonical :kind 'person
      :aliases (unless (equal canonical variant) (list variant))))
    (message "Promoted %S as entity %S" variant canonical)))

;;;; Life events
;;
;; Life events (birth, death, marriage, name-change) are ordinary timeline
;; events tagged with LIFE-EVENT and SUBJECT.  A person's existence span,
;; places, and spouses are derived from them, with entity properties as a
;; fallback.

(defun org-chronicle--life-index (events idx)
  "Build a hash from a canonical subject name to its derived life facts.
EVENTS is the event list; IDX the alias index.  Each value is a plist
\(:birth (DATE . PLACE) :death (DATE . PLACE) :spouses (NAME ...)), where
DATE is a date plist and PLACE a location string."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (e events index)
      (let ((kind (plist-get e :life-event))
            (subjects (mapcar (lambda (s) (org-chronicle--canonical s idx))
                              (plist-get e :subject))))
        (pcase kind
          ("birth"
           (let ((name (car subjects)))
             (when name
               (puthash name
                        (plist-put (gethash name index) :birth
                                   (cons (plist-get e :date) (plist-get e :location)))
                        index))))
          ("death"
           (let ((name (car subjects)))
             (when name
               (puthash name
                        (plist-put (gethash name index) :death
                                   (cons (plist-get e :date) (plist-get e :location)))
                        index))))
          ("marriage"
           (dolist (s subjects)
             (let ((cell (gethash s index)))
               (puthash s
                        (plist-put cell :spouses
                                   (append (plist-get cell :spouses)
                                           (remove s subjects)))
                        index)))))))))

(defun org-chronicle--life-event-tag (kind)
  "Return an Org-tag-safe form of life-event KIND, or nil.
Org tags disallow hyphens, so they are replaced with underscores."
  (and kind (replace-regexp-in-string "-" "_" kind)))

(defun org-chronicle--alias-list-with (aliases new-name canonical)
  "Return ALIASES extended with NEW-NAME.
NEW-NAME is dropped when blank, equal to CANONICAL, or already in ALIASES."
  (if (or (null new-name) (string-blank-p new-name)
          (equal new-name canonical)
          (member new-name aliases))
      aliases
    (append aliases (list new-name))))

(defun org-chronicle--accrue-alias (subject new-name)
  "Add NEW-NAME to the ALIASES of the promoted entity named SUBJECT.
Does nothing but message when SUBJECT is not a promoted entity."
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (ent (org-chronicle--find-entity-by-name subject entities idx)))
    (if (null ent)
        (message "org-chronicle: %S is not a promoted entity; alias %S not accrued"
                 subject new-name)
      (let ((id (plist-get ent :id))
            (canon (plist-get ent :name)))
        (cl-loop for file in (org-chronicle--source-files) do
                 (with-current-buffer (find-file-noselect file)
                   (org-with-wide-buffer
                    (let ((pos (org-find-property "ID" id)))
                      (when pos
                        (goto-char pos)
                        (org-set-property
                         "ALIASES"
                         (org-chronicle--join
                          (org-chronicle--alias-list-with
                           (org-chronicle--split (org-entry-get nil "ALIASES"))
                           new-name canon)))
                        (save-buffer))))))))))

(defun org-chronicle--life-event-title (kind subjects new-name)
  "Suggest a title for a life event of KIND with SUBJECTS and optional NEW-NAME."
  (pcase kind
    ("birth" (format "Birth of %s" (car subjects)))
    ("death" (format "Death of %s" (car subjects)))
    ("marriage" (format "Marriage of %s" (string-join subjects " and ")))
    ("name-change" (format "%s becomes %s" (car subjects) new-name))
    (_ (or (car subjects) "Life event"))))

(cl-defun org-chronicle--life-event-string (&key title kind truth date subject
                                                 people location sources new-name)
  "Return the Org heading text for a life event.
KIND is the LIFE-EVENT value; SUBJECT and PEOPLE are name lists; TITLE,
DATE, LOCATION, SOURCES, NEW-NAME, and TRUTH are strings (some optional)."
  (concat
   (format "* %s\n" title)
   ":PROPERTIES:\n"
   (format ":TRUTH:      %s\n" (or truth "historical"))
   (format ":LIFE-EVENT: %s\n" kind)
   (format ":DATE:       %s\n" (org-chronicle--ts date))
   (format ":SUBJECT:    %s\n" (org-chronicle--join subject))
   (when (and new-name (not (string-blank-p new-name)))
     (format ":NEW-NAME:   %s\n" new-name))
   (when people (format ":PEOPLE:     %s\n" (org-chronicle--join people)))
   (when (and location (not (string-blank-p location)))
     (format ":LOCATION:   %s\n" location))
   (when (and sources (not (string-blank-p sources)))
     (format ":SOURCES:    %s\n" sources))
   ":END:\n"))

;;;###autoload
(cl-defun org-chronicle-add-life-event (&optional kind subjects)
  "Capture a birth, death, marriage, or name-change as a timeline event.
KIND and SUBJECTS may be supplied non-interactively (used by
`org-chronicle-add-person'); otherwise they are prompted for."
  (interactive)
  (let* ((kind (or kind (completing-read "Life event kind: "
                                         org-chronicle--life-event-kinds nil t)))
         (count (if (equal kind "marriage") 2 1))
         (subjects (or subjects
                       (let (acc)
                         (dotimes (i count)
                           (push (car (org-chronicle--read-names
                                       (format "Subject %d: " (1+ i))
                                       (org-chronicle--known-people)
                                       'org-chronicle-person))
                                 acc))
                         (nreverse acc))))
         (new-name (and (equal kind "name-change") (read-string "New name: ")))
         (date (org-chronicle--read-date "Date (YYYY-MM-DD): "))
         (truth (completing-read "Truth: " org-chronicle--truth-values nil t
                                 nil nil "historical"))
         (parents (and (equal kind "birth")
                       (org-chronicle--read-names "Parents (blank to skip): "
                                                  (org-chronicle--known-people)
                                                  'org-chronicle-person)))
         (location (org-chronicle--read-location))
         (sources (read-string "Sources (blank to skip): "))
         (people (delete-dups (append subjects parents)))
         (title (read-string "Title: "
                             (org-chronicle--life-event-title kind subjects new-name))))
    (org-chronicle--append-event
     (org-chronicle--life-event-string
      :title title :kind kind :truth truth :date date :subject subjects
      :people people :location location :sources sources :new-name new-name))
    (when (and (equal kind "marriage")
               (y-or-n-p "Did a spouse take a new name? "))
      (let ((who (completing-read "Who changed name: " subjects nil t))
            (nn (read-string "New name: ")))
        (org-chronicle--append-event
         (org-chronicle--life-event-string
          :title (org-chronicle--life-event-title "name-change" (list who) nn)
          :kind "name-change" :truth truth :date date :subject (list who)
          :people (list who) :location location :new-name nn :sources sources))))
    (message "Added %s life event" kind)))





;;;; Sources

(declare-function org-reading-list-entries "org-reading-list" ())

(defun org-chronicle--source-link (id description &optional locator)
  "Return an Org id link to a source: [[id:ID][DESCRIPTION]] optional LOCATOR."
  (concat (format "[[id:%s][%s]]" id description)
          (when (and locator (not (string-blank-p locator)))
            (concat " " locator))))

(defun org-chronicle--read-source (&optional free-text)
  "Return a source string.
If `org-reading-list' is loaded, offer to pick an entry and build an id
link with an optional locator; otherwise (or on blank pick) return
FREE-TEXT or a prompted free-text citation."
  (if (and (featurep 'org-reading-list)
           (fboundp 'org-reading-list-entries))
      (let* ((entries (org-reading-list-entries))
             (pick (completing-read "Source (blank for free text): "
                                    (mapcar #'car entries) nil nil)))
        (if (string-blank-p pick)
            (or free-text (read-string "Free-text source: "))
          (let ((id (cdr (assoc pick entries))))
            (if id
                (org-chronicle--source-link
                 id pick (read-string "Locator (e.g. p.412, blank to skip): "))
              pick))))
    (or free-text (read-string "Free-text source: "))))

;;;###autoload
(defun org-chronicle-add-source ()
  "Append a source to the SOURCES property of the event heading at point."
  (interactive)
  (org-back-to-heading t)
  (let* ((existing (org-entry-get nil "SOURCES"))
         (new (org-chronicle--read-source))
         (combined (if (and existing (not (string-blank-p existing)))
                       (concat existing org-chronicle-multi-value-separator new)
                     new)))
    (org-set-property "SOURCES" combined)))

;;;; Lint

(defun org-chronicle--span-for-name (name entities idx index)
  "Return (FROM . TO) existence span for canonical NAME, or nil if unknown.
Prefer birth/death dates from the life-event INDEX; fall back to the
entity's BORN/DIED span in ENTITIES.  IDX is the alias index."
  (let* ((canon (org-chronicle--canonical name idx))
         (facts (gethash canon index))
         (ent (cl-find canon entities
                       :key (lambda (e) (plist-get e :name)) :test #'equal))
         (from (or (car (plist-get facts :birth))
                   (and ent (plist-get ent :span-from))))
         (to (or (car (plist-get facts :death))
                 (and ent (plist-get ent :span-to)))))
    (when (or from to)
      (cons from to))))

(defun org-chronicle--event-anachronisms (event entities idx index)
  "Return a list of human-readable anachronism messages for EVENT.
An anachronism is a participant or location in ENTITIES whose existence
span (from the life-event INDEX, else entity properties) does not contain
the event's date.  IDX is the alias index.  Empty list means clean."
  (let ((date (plist-get event :date))
        (msgs '()))
    (when date
      (dolist (name (cons (plist-get event :location) (plist-get event :people)))
        (when name
          (let ((span (org-chronicle--span-for-name name entities idx index)))
            (when (and span
                       (not (org-chronicle--date-in-span-p date (car span) (cdr span))))
              (push (format "%s outside existence span of %s"
                            (org-chronicle--date-format date)
                            (org-chronicle--canonical name idx))
                    msgs))))))
    (nreverse msgs)))

;;;###autoload
(defun org-chronicle-lint ()
  "Report timeline events that fall outside a participant or place span."
  (interactive)
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (events (org-chronicle--all-events))
         (index (org-chronicle--life-index events idx))
         (findings '()))
    (dolist (e events)
      (dolist (m (org-chronicle--event-anachronisms e entities idx index))
        (push (cons e m) findings)))
    (with-current-buffer (get-buffer-create "*org-chronicle-lint*")
      (org-chronicle-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null findings)
            (insert "No anachronisms found.\n")
          (insert (format "%d anachronism(s):\n\n" (length findings)))
          (dolist (f (nreverse findings))
            (insert (propertize (format "- %s: %s\n"
                                        (plist-get (car f) :title) (cdr f))
                                'org-chronicle-marker (plist-get (car f) :marker))))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))




















;;;; (sections added by later tasks)

(provide 'org-chronicle)

;;; org-chronicle.el ends here
