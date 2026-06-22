;;; org-chronicle.el --- Event timeline for historical fiction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/andapony/org-chronicle
;; Version: 0.9.0
;; Package-Requires: ((emacs "27.2") (org "9.4"))
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

(require 'calendar)

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

(defun org-chronicle--age-years (birth at)
  "Return whole years from BIRTH to AT (date plists), or nil.
Exact when both dates carry a month; year-difference for coarse dates.
Returns nil when either date is missing or AT precedes BIRTH."
  (when (and birth at)
    (let ((age (- (plist-get at :year) (plist-get birth :year))))
      (when (and (plist-get at :month) (plist-get birth :month)
                 (let ((am (plist-get at :month)) (bm (plist-get birth :month))
                       (ad (or (plist-get at :day) 1))
                       (bd (or (plist-get birth :day) 1)))
                   (or (< am bm) (and (= am bm) (< ad bd)))))
        (setq age (1- age)))
      (and (>= age 0) age))))


(defun org-chronicle--date-lower-bound (d)
  "Return date plist D expanded to the earliest instant of its precision.
A year-only date becomes its January 1, a month date its first day; a day
date is returned unchanged.  The result has day precision."
  (when d
    (let ((y (plist-get d :year))
          (mo (or (plist-get d :month) 1))
          (day (or (plist-get d :day) 1)))
      (org-chronicle--date-parse (format "%04d-%02d-%02d" y mo day)))))

(defun org-chronicle--date-upper-bound (d)
  "Return date plist D expanded to the latest instant of its precision.
A year-only date becomes its December 31, a month date its last day; a day
date is returned unchanged.  The result has day precision."
  (when d
    (let* ((y (plist-get d :year))
           (mo (or (plist-get d :month) 12))
           (day (or (plist-get d :day)
                    (calendar-last-day-of-month mo y))))
      (org-chronicle--date-parse (format "%04d-%02d-%02d" y mo day)))))

(defun org-chronicle--date-ordinal (d)
  "Return the absolute Gregorian day number for date plist D.
Missing month/day default to 1, so callers expand coarse dates with
`org-chronicle--date-lower-bound' / `org-chronicle--date-upper-bound' first."
  (calendar-absolute-from-gregorian
   (list (or (plist-get d :month) 1) (or (plist-get d :day) 1)
         (plist-get d :year))))

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
Lists *.org recursively under the root (symlinks not followed), dropping
Emacs lock files (.#*) and any whose path relative to the root matches a
regexp in `org-chronicle-exclude'."
  (let ((root (expand-file-name org-chronicle-root)))
    (when (file-directory-p root)
      (cl-remove-if
       (lambda (f)
         ;; Match against a leading-slash-prefixed relative path so a pattern
         ;; like "/drafts/" anchors a top-level subtree.
         (let ((rel (concat "/" (file-relative-name f root))))
           (or (string-prefix-p ".#" (file-name-nondirectory f))
               (cl-some (lambda (re) (string-match-p re rel)) org-chronicle-exclude))))
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
  "Non-nil if EVENT belongs in LANE, resolving names via alias index IDX.
A person lane matches when one of LANE's names is among the event's
participants (:people) or, for a life event, its principal(s) (:subject),
so a person's own birth/death/marriage appears in their lane."
  (let ((names (plist-get lane :names)))
    (pcase (plist-get lane :domain)
      ('people
       (cl-some (lambda (p) (member (org-chronicle--canonical p idx) names))
                (append (plist-get event :people) (plist-get event :subject))))
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

(defface org-chronicle-historical '((t :inherit org-link))
  "Face for historical events in the timeline view.")
(defface org-chronicle-fictionalized '((t :inherit org-link))
  "Face for fictionalized events in the timeline view.")
(defface org-chronicle-fictional '((t :inherit org-link))
  "Face for fictional events in the timeline view.")

(defconst org-chronicle--date-col-width 12)

(defun org-chronicle--truth-marker (truth)
  "Return the short single-character marker string for TRUTH."
  (pcase truth
    ("historical" "H")
    ("fictionalized" "~")
    ("fictional" "F")
    (_ "?")))

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
Leads with the truth marker so it survives column truncation, then the
topic glyph (when set), and uses the topic face, falling back to the
event's truth face."
  (let* ((glyph (org-chronicle--topic-glyph topic))
         (face (or (org-chronicle--topic-face topic)
                   (org-chronicle--truth-face (plist-get event :truth))))
         (s (format "%s %s%s"
                    (org-chronicle--truth-marker (plist-get event :truth))
                    (if glyph (concat glyph " ") "")
                    (plist-get event :title))))
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
  "Return propertized cell text (truth marker, kind glyph, title) for EVENT.
The truth marker leads so it survives column truncation."
  (let* ((glyph (org-chronicle--life-event-glyph (plist-get event :life-event)))
         (s (format "%s %s%s"
                    (org-chronicle--truth-marker (plist-get event :truth))
                    (if glyph (concat glyph " ") "")
                    (plist-get event :title))))
    (propertize s 'face (org-chronicle--truth-face (plist-get event :truth))
                'org-chronicle-marker (plist-get event :marker))))

(defun org-chronicle--render (events lanes idx col-width)
  "Render EVENTS into a swimlane string across LANES.
IDX is an alias index; COL-WIDTH is the width of each lane column.  Rows
are dates (ascending); each lane column shows that lane's events on that
date.  A date whose events fall in none of the LANES is omitted, so the
view never shows an empty row.  A cell in a person lane (one carrying a
:birth date) gets a `help-echo' tooltip with the person's age at that date.
EVENTS are assumed already filtered and sorted ascending."
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
               (cells '())
               (any nil))
          (dolist (lane lanes)
            (let* ((hits (cl-remove-if-not
                          (lambda (e) (org-chronicle--event-in-lane-p e lane idx))
                          day-events))
                   (txt (mapconcat (lambda (e) (org-chronicle--cell-text-for-lane e lane))
                                   hits " / "))
                   (birth (plist-get lane :birth)))
              (unless (string-empty-p txt) (setq any t))
              (when (and birth hits)
                (let ((age (org-chronicle--age-years
                            birth (plist-get (car hits) :date))))
                  (when age
                    (setq txt (copy-sequence txt))
                    (put-text-property 0 (length txt) 'help-echo
                                       (format "%s: age %d" (plist-get lane :label) age)
                                       txt))))
              (push (org-chronicle--pad txt col-width) cells)))
          (when any
            (push (concat (org-chronicle--pad date dcw)
                          (apply #'concat (nreverse cells)))
                  lines)))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; View

(defcustom org-chronicle-lane-column-width 22
  "Minimum width in columns of each lane in the timeline view.
The interactive view divides the available window width evenly among the
lanes; this value is the floor below which a lane column will not shrink.
It is also the fixed width used when no window width is available, such
as when rendering a dynamic block."
  :type 'integer
  :group 'org-chronicle)

(defun org-chronicle--lane-width (total nlanes)
  "Return the per-lane column width for NLANES lanes within TOTAL columns.
Splits the space remaining after the date column evenly among the lanes,
never returning less than `org-chronicle-lane-column-width'."
  (if (<= nlanes 0)
      org-chronicle-lane-column-width
    (max org-chronicle-lane-column-width
         (/ (- total org-chronicle--date-col-width) nlanes))))

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

(defun org-chronicle--lanes-with-birth (lanes entities idx index)
  "Return LANES with a :birth date added to each single-person lane.
ENTITIES, IDX, and the life-event INDEX resolve the lane's person via
`org-chronicle--person-birth'."
  (mapcar (lambda (lane)
            (let ((birth (and (eq (plist-get lane :domain) 'people)
                              (org-chronicle--person-birth
                               (plist-get lane :label) entities idx index))))
              (if birth (append lane (list :birth birth)) lane)))
          lanes))


(cl-defun org-chronicle--compose (&key people locations topics truth from until
                                       (mode :collapse)
                                       width
                                       (root nil root-p)
                                       (exclude nil exclude-p))
  "Return the rendered timeline string for the given filters.
PEOPLE/LOCATIONS/TOPICS are name lists naming lanes; TRUTH a list of
allowed truth strings; FROM/UNTIL date strings; MODE `:collapse' or
`:expand'.  WIDTH, when non-nil, is the total display width to divide
among the lanes (see `org-chronicle--lane-width'); otherwise each lane
uses `org-chronicle-lane-column-width'.  ROOT and EXCLUDE, when supplied,
override `org-chronicle-root' and `org-chronicle-exclude' for this gather
\(used to keep view refresh consistent with dir-local values)."
  (let ((org-chronicle-root (if root-p root org-chronicle-root))
        (org-chronicle-exclude (if exclude-p exclude org-chronicle-exclude)))
    (let* ((entities (org-chronicle--all-entities))
           (idx (org-chronicle--alias-index entities))
           (all-events (org-chronicle--all-events))
           (index (org-chronicle--life-index all-events idx))
           (lanes (org-chronicle--lanes-with-birth
                   (org-chronicle--lanes-from-params people locations topics entities mode)
                   entities idx index))
           (col-width (if width
                          (org-chronicle--lane-width width (length lanes))
                        org-chronicle-lane-column-width))
           (events (org-chronicle--filter-events
                    all-events idx
                    :truth truth
                    :from (and from (org-chronicle--date-parse from))
                    :until (and until (org-chronicle--date-parse until)))))
      (org-chronicle--render events lanes idx col-width))))

(defvar org-chronicle-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-chronicle-view-goto)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'org-chronicle-view-refresh)
    (define-key map [mouse-8] #'org-chronicle-history-back)
    (define-key map [mouse-9] #'org-chronicle-history-forward)
    (define-key map (kbd "C-c <left>") #'org-chronicle-history-back)
    (define-key map (kbd "C-c <right>") #'org-chronicle-history-forward)
    (define-key map "l" #'org-chronicle-history-back)
    (define-key map "r" #'org-chronicle-history-forward)
    map)
  "Keymap for `org-chronicle-view-mode'.")

(define-derived-mode org-chronicle-view-mode special-mode "Chronicle"
  "Major mode for the read-only swimlane timeline view.")

(defvar-local org-chronicle--view-args nil
  "The plist of arguments that produced the current view, for refresh.")

(defun org-chronicle-view-goto ()
  "Jump to the event heading for the cell at point.
Records the jump in the chronicle link history."
  (interactive)
  (let ((m (get-text-property (point) 'org-chronicle-marker)))
    (if (and m (marker-buffer m))
        (let ((origin (org-chronicle--history-location)))
          (pop-to-buffer (marker-buffer m))
          (goto-char m)
          (org-reveal)
          (org-chronicle--history-record origin (org-chronicle--history-location)))
      (message "No event at point"))))

(defun org-chronicle-view-refresh ()
  "Recompute the current timeline view."
  (interactive)
  (when org-chronicle--view-args
    (apply #'org-chronicle-timeline org-chronicle--view-args)))

(defun org-chronicle--lane-arg-key (domain)
  "Return the `org-chronicle--view-args' key for lane DOMAIN.
DOMAIN is one of the lane domain symbols `people', `location', or
`topic'; the corresponding key is `:people', `:locations', or `:topics'."
  (pcase domain
    ('people :people)
    ('location :locations)
    ('topic :topics)))

(defun org-chronicle--view-args-add (args pairs)
  "Return ARGS with each lane in PAIRS appended to its domain key.
PAIRS is a list of (NAME . DOMAIN) conses.  Names already present under a
key are not duplicated.  ARGS is copied; the original is left unchanged."
  (let ((out (copy-sequence args)))
    (dolist (pair pairs out)
      (let* ((key (org-chronicle--lane-arg-key (cdr pair)))
             (cur (plist-get out key)))
        (unless (member (car pair) cur)
          (setq out (plist-put out key (append cur (list (car pair))))))))))

(defun org-chronicle--view-args-remove (args identities)
  "Return ARGS with each lane in IDENTITIES removed from its domain key.
IDENTITIES is a list of (DOMAIN . NAME) conses; NAME is removed by
`equal'.  ARGS is copied; the original is left unchanged."
  (let ((out (copy-sequence args)))
    (dolist (id identities out)
      (let* ((key (org-chronicle--lane-arg-key (car id)))
             (cur (plist-get out key)))
        (setq out (plist-put out key (remove (cdr id) cur)))))))

(defun org-chronicle--current-lane-identities (args)
  "Return (DOMAIN . NAME) identities for every lane named in ARGS.
Covers the `:people', `:locations', and `:topics' keys in that order."
  (append (mapcar (lambda (n) (cons 'people n)) (plist-get args :people))
          (mapcar (lambda (n) (cons 'location n)) (plist-get args :locations))
          (mapcar (lambda (n) (cons 'topic n)) (plist-get args :topics))))

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
Lane width is computed from the window width and the lane count, so
refreshing with \\[org-chronicle-view-refresh] re-fits the view after a
resize.  Interactively, prompts for people, locations, and topics (where
entering \"all\", `org-chronicle--all-lanes-token', selects every known
name for that lane and a blank entry selects none), then for a FROM and
UNTIL date bounding the range (each blank for no bound), a truth subset
(\"all\" or blank for every truth), and whether to expand groups."
  (interactive
   (list :people (org-chronicle--read-names
                  "People/groups (lanes): "
                  (org-chronicle--known-people) 'org-chronicle-person t)
         :locations (org-chronicle--read-names
                     "Places (lanes): "
                     (org-chronicle--known-locations) 'org-chronicle-place t)
         :topics (org-chronicle--read-names
                  "Topics (lanes): "
                  (org-chronicle--known-topics) 'org-chronicle-topic t)
         :from (let ((s (string-trim
                         (read-string
                          "From date (YYYY[-MM[-DD]], blank = no start bound): "))))
                 (unless (string-empty-p s) s))
         :until (let ((s (string-trim
                          (read-string
                           "Until date (YYYY[-MM[-DD]], blank = no end bound): "))))
                  (unless (string-empty-p s) s))
         :truth (org-chronicle--read-truth)
         :mode (if (y-or-n-p "Expand groups into member lanes? ") :expand :collapse)))
  (let* ((root (if root-p root org-chronicle-root))
         (exclude (if exclude-p exclude org-chronicle-exclude))
         (args (list :people people :locations locations :topics topics
                     :truth truth :from from :until until :mode mode
                     :root root :exclude exclude))
         (buf (get-buffer-create "*org-chronicle*")))
    (with-current-buffer buf
      (org-chronicle-view-mode)
      (setq org-chronicle--view-args args))
    (pop-to-buffer buf)
    (let* ((width (window-body-width (get-buffer-window buf)))
           (text (org-chronicle--compose :people people :locations locations :topics topics
                                         :truth truth :from from :until until :mode mode
                                         :width width :root root :exclude exclude)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (goto-char (point-min))))))

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

(defun org-chronicle--collect-names (event-name-key entity-kinds &optional preferred)
  "Return a sorted, de-duplicated list of names for completion.
Gathers the EVENT-NAME-KEY value of every event (a string, or a list of
strings) and the name plus aliases of every entity whose `:kind' is in
ENTITY-KINDS.  When PREFERRED is non-nil, resolve every gathered name to
its canonical (preferred) form via the alias index, so aliases fold into
the owning entity's preferred name and only preferred names are returned."
  (let ((names (make-hash-table :test #'equal))
        (entities (ignore-errors (org-chronicle--all-entities))))
    (dolist (e (ignore-errors (org-chronicle--all-events)))
      (let ((v (plist-get e event-name-key)))
        (cond ((listp v) (dolist (x v) (when x (puthash x t names))))
              (v (puthash v t names)))))
    (dolist (e entities)
      (when (memq (plist-get e :kind) entity-kinds)
        (puthash (plist-get e :name) t names)
        (dolist (a (plist-get e :aliases)) (puthash a t names))))
    (let ((keys (hash-table-keys names)))
      (when preferred
        (let ((idx (org-chronicle--alias-index entities)))
          (setq keys (delete-dups
                      (mapcar (lambda (n) (org-chronicle--canonical n idx))
                              keys)))))
      (sort keys #'string<))))

(defun org-chronicle--known-people ()
  "Return preferred names of known people and groups from events and entities."
  (org-chronicle--collect-names :people '(person group) t))

(defun org-chronicle--known-locations ()
  "Return preferred names of known locations from event locations and places."
  (org-chronicle--collect-names :location '(place) t))

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

(defconst org-chronicle--all-lanes-token "all"
  "Pseudo-name selecting everything at a timeline lane or truth prompt.
Offered as a completion candidate by `org-chronicle--read-names' (with
ALLOW-ALL) and by `org-chronicle--read-truth'; entering it expands to all
candidates for that prompt.  Blank input also selects all.")


(defun org-chronicle--expand-all (selected candidates)
  "Return CANDIDATES when SELECTED contains the all-lanes token, else SELECTED.
SELECTED is the list of chosen names; CANDIDATES the full completion set.
See `org-chronicle--all-lanes-token'."
  (if (member org-chronicle--all-lanes-token selected)
      (copy-sequence candidates)
    selected))


(defun org-chronicle--read-names (prompt candidates category &optional allow-all)
  "Read multiple names with completion; return a list (nil when blank).
PROMPT is the minibuffer prompt, CANDIDATES the completion set, CATEGORY
the completion category.  Entries are separated by
`org-chronicle-multi-value-separator', so names that themselves contain a
comma (e.g. \"Vicksburg, Mississippi\") are not split, and new names not
in CANDIDATES are still accepted.  When ALLOW-ALL is non-nil the
`org-chronicle--all-lanes-token' pseudo-name is also offered, and choosing
it expands the result to all of CANDIDATES."
  (let* ((crm-separator
          (concat "[ \t]*"
                  (regexp-quote (string-trim org-chronicle-multi-value-separator))
                  "[ \t]*"))
         (offered (if allow-all
                      (cons org-chronicle--all-lanes-token candidates)
                    candidates))
         (selected (delete "" (completing-read-multiple
                               prompt
                               (org-chronicle--completion-table offered category)
                               nil nil))))
    (if allow-all
        (org-chronicle--expand-all selected candidates)
      selected)))

(defun org-chronicle--read-truth ()
  "Read a truth filter for the timeline interactively.
Offers the three truth values plus an \"all\" option (the value of
`org-chronicle--all-lanes-token').  Returns the chosen subset, or nil to
mean no filter (every truth) when the entry is blank or \"all\"."
  (let* ((choices '("historical" "fictionalized" "fictional"))
         (v (delete "" (completing-read-multiple
                        "Truth (\"all\" or subset; blank = all): "
                        (cons org-chronicle--all-lanes-token choices)))))
    (unless (or (null v) (member org-chronicle--all-lanes-token v)) v)))


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

(defun org-chronicle--entity-link-segments (value entities idx)
  "Return resolving name segments of property VALUE as (BEG END NAME ID).
BEG and END are zero-based offsets into VALUE bounding a trimmed name,
NAME the segment text, and ID the matched entity's org-id.  Segments are
split on `org-chronicle-multi-value-separator'; segments that do not
resolve to an entity (via ENTITIES and alias index IDX) are omitted."
  (when (and (stringp value) (not (string-blank-p value)))
    (let ((sep (regexp-quote (string-trim org-chronicle-multi-value-separator)))
          (len (length value))
          (start 0)
          (out '())
          (done nil))
      (while (not done)
        (let* ((mb (string-match sep value start))
               ;; Capture the separator's end now: string-trim below calls
               ;; string-match internally and would clobber the match data.
               (next (and mb (match-end 0)))
               (seg-end (or mb len))
               (raw (substring value start seg-end))
               (lead (- (length raw) (length (string-trim-left raw))))
               (trimmed (string-trim raw))
               (beg (+ start lead))
               (ent (and (not (string-empty-p trimmed))
                         (org-chronicle--find-entity-by-name trimmed entities idx)))
               (id (and ent (plist-get ent :id))))
          (when id
            (push (list beg (+ beg (length trimmed)) trimmed id) out))
          (if mb (setq start next) (setq done t))))
      (nreverse out))))

(defvar org-chronicle--entity-cache nil
  "Cached (ENTITIES . IDX) for entity-link fontification, or nil when stale.")

(defvar org-chronicle--entity-link-buffers nil
  "Live buffers with `org-chronicle-entity-links-mode' enabled.")

(defvar org-chronicle--history nil
  "Global trail of visited locations for chronicle link back/forward.
Each element is a location plist (:marker M :file F :pos P).")

(defvar org-chronicle--history-position -1
  "Index into `org-chronicle--history' of the current location, or -1.")

(defun org-chronicle--entity-cache ()
  "Return cached (ENTITIES . IDX), building it from disk when stale."
  (or org-chronicle--entity-cache
      (let* ((entities (org-chronicle--all-entities))
             (idx (org-chronicle--alias-index entities)))
        (setq org-chronicle--entity-cache (cons entities idx)))))

(defun org-chronicle--invalidate-entity-cache ()
  "Drop the entity cache and refontify entity-link buffers."
  (setq org-chronicle--entity-cache nil)
  (dolist (buf org-chronicle--entity-link-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when font-lock-mode (font-lock-flush))))))

(defun org-chronicle--file-under-root-p (file)
  "Non-nil when FILE is within `org-chronicle-root'."
  (and file org-chronicle-root
       (string-prefix-p
        (file-name-as-directory (expand-file-name org-chronicle-root))
        (expand-file-name file))))

(defun org-chronicle--maybe-invalidate-entity-cache ()
  "Invalidate the entity cache when the buffer's file is under the root.
Used on `after-save-hook' and `after-revert-hook' so the cache tracks
both local saves and external changes picked up by auto-revert."
  (when (org-chronicle--file-under-root-p buffer-file-name)
    (org-chronicle--invalidate-entity-cache)))

(defface org-chronicle-entity-link '((t :inherit org-link))
  "Face for clickable entity names in event property values.")

(defun org-chronicle-visit-entity-at-point ()
  "Visit the entity named by the entity-link button at point.
Records the jump in the chronicle link history."
  (interactive)
  (let ((id (get-text-property (point) 'org-chronicle-entity-id)))
    (if id
        (let ((origin (org-chronicle--history-location)))
          (org-id-goto id)
          (org-chronicle--history-record origin (org-chronicle--history-location)))
      (user-error "No entity link at point"))))

(defvar org-chronicle-entity-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-chronicle-visit-entity-at-point)
    (define-key map [mouse-2] #'org-chronicle-visit-entity-at-point)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap active on entity-link buttons in event property values.")

(defconst org-chronicle--entity-link-prop-regexp
  "^[ \t]*:\\(?:PEOPLE\\|LOCATION\\|TOPICS\\|SUBJECT\\|BIRTHPLACE\\|DEATHPLACE\\):[ \t]*\\(.*\\)$"
  "Match an in-scope property line; group 1 is the value region.
Covers event participant/place/topic properties and entity
birthplace/deathplace vitals.")

(defun org-chronicle--fontify-entity-value (beg end)
  "Buttonize resolving entity names in buffer region BEG..END.
Always returns nil; faces are applied per segment, not over the region."
  (let* ((cache (org-chronicle--entity-cache))
         (value (buffer-substring-no-properties beg end)))
    (dolist (seg (org-chronicle--entity-link-segments value (car cache) (cdr cache)))
      (add-text-properties
       (+ beg (nth 0 seg)) (+ beg (nth 1 seg))
       (list 'face 'org-chronicle-entity-link
             'mouse-face 'highlight
             'help-echo "mouse-2, RET: visit entity"
             'keymap org-chronicle-entity-link-keymap
             'org-chronicle-entity-id (nth 3 seg)))))
  nil)

(defun org-chronicle--entity-link-keywords ()
  "Return font-lock keywords that buttonize entity names in event properties."
  `((,org-chronicle--entity-link-prop-regexp
     (1 (progn (org-chronicle--fontify-entity-value
                (match-beginning 1) (match-end 1))
               nil)))))

(defvar org-chronicle-entity-links-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-8] #'org-chronicle-history-back)
    (define-key map [mouse-9] #'org-chronicle-history-forward)
    (define-key map (kbd "C-c <left>") #'org-chronicle-history-back)
    (define-key map (kbd "C-c <right>") #'org-chronicle-history-forward)
    map)
  "Keymap for `org-chronicle-entity-links-mode'.")

(define-minor-mode org-chronicle-entity-links-mode
  "Buttonize event property values that name promoted entities.
Names in PEOPLE/LOCATION/TOPICS/SUBJECT/BIRTHPLACE/DEATHPLACE that resolve
to an entity become
clickable links to that entity's heading; unresolved names stay plain.
The entity set is cached and invalidated on save or revert of files under
`org-chronicle-root', so external changes picked up by auto-revert keep
the links current."
  :lighter " OCLink"
  :keymap org-chronicle-entity-links-mode-map
  (if org-chronicle-entity-links-mode
      (progn
        (font-lock-add-keywords nil (org-chronicle--entity-link-keywords) 'append)
        (setq-local font-lock-extra-managed-props
                    (append '(mouse-face help-echo keymap org-chronicle-entity-id)
                            font-lock-extra-managed-props))
        (cl-pushnew (current-buffer) org-chronicle--entity-link-buffers)
        (add-hook 'after-save-hook #'org-chronicle--maybe-invalidate-entity-cache)
        (add-hook 'after-revert-hook #'org-chronicle--maybe-invalidate-entity-cache)
        (font-lock-flush))
    (font-lock-remove-keywords nil (org-chronicle--entity-link-keywords))
    (setq org-chronicle--entity-link-buffers
          (delq (current-buffer) org-chronicle--entity-link-buffers))
    (unless org-chronicle--entity-link-buffers
      (remove-hook 'after-save-hook #'org-chronicle--maybe-invalidate-entity-cache)
      (remove-hook 'after-revert-hook #'org-chronicle--maybe-invalidate-entity-cache))
    (font-lock-flush)))

(defun org-chronicle-entity-links-refresh ()
  "Rebuild entity links across `org-chronicle-entity-links-mode' buffers.
Drops the cached entity set and refontifies, so event buffers pick up
entities added, renamed, or re-aliased since the cache was built.  Useful
after editing entity files outside the running Emacs."
  (interactive)
  (org-chronicle--invalidate-entity-cache))

(defun org-chronicle--turn-on-entity-links ()
  "Enable `org-chronicle-entity-links-mode' in chronicle org buffers.
Used by `org-chronicle-global-entity-links-mode'; a no-op unless the
current buffer is an `org-mode' file under `org-chronicle-root'."
  (when (and (derived-mode-p 'org-mode)
             buffer-file-name
             (org-chronicle--file-under-root-p buffer-file-name))
    (org-chronicle-entity-links-mode 1)))

;;;###autoload
(define-globalized-minor-mode org-chronicle-global-entity-links-mode
  org-chronicle-entity-links-mode
  org-chronicle--turn-on-entity-links
  :group 'org-chronicle)

(defun org-chronicle--history-location ()
  "Return a history location plist for point in the current buffer."
  (list :marker (point-marker)
        :file (buffer-file-name)
        :pos (point)))

(defun org-chronicle--location= (a b)
  "Return non-nil when locations A and B denote the same place."
  (and a b
       (let ((ma (plist-get a :marker))
             (mb (plist-get b :marker)))
         (or (and ma mb (marker-buffer ma) (marker-buffer mb)
                  (eq (marker-buffer ma) (marker-buffer mb))
                  (= (marker-position ma) (marker-position mb)))
             (and (plist-get a :file)
                  (equal (plist-get a :file) (plist-get b :file))
                  (eql (plist-get a :pos) (plist-get b :pos)))))))

(defun org-chronicle--history-record (origin target)
  "Record a follow from ORIGIN to TARGET in the chronicle link history.
Drop any forward entries, ensure ORIGIN is the current entry, append
TARGET, and make it current.  ORIGIN and TARGET are location plists."
  (setq org-chronicle--history
        (cl-subseq org-chronicle--history 0
                   (min (length org-chronicle--history)
                        (1+ org-chronicle--history-position))))
  (unless (and org-chronicle--history
               (org-chronicle--location=
                (nth org-chronicle--history-position org-chronicle--history)
                origin))
    (setq org-chronicle--history (append org-chronicle--history (list origin))))
  (setq org-chronicle--history (append org-chronicle--history (list target)))
  (setq org-chronicle--history-position (1- (length org-chronicle--history))))

(defun org-chronicle--history-go (delta)
  "Move the history position by DELTA and return the new location.
Signal a `user-error' when moving past either end."
  (let ((new (+ org-chronicle--history-position delta)))
    (when (or (< new 0) (>= new (length org-chronicle--history)))
      (user-error "No %s in chronicle history"
                  (if (< delta 0) "further back" "further forward")))
    (setq org-chronicle--history-position new)
    (nth new org-chronicle--history)))

(defun org-chronicle--history-visit (loc)
  "Switch to LOC's buffer (or file) and move point to its position."
  (let ((m (plist-get loc :marker)))
    (if (and m (marker-buffer m))
        (progn (switch-to-buffer (marker-buffer m))
               (goto-char m))
      (let ((file (plist-get loc :file)))
        (if (and file (file-exists-p file))
            (progn (switch-to-buffer (find-file-noselect file))
                   (goto-char (or (plist-get loc :pos) (point-min))))
          (user-error "That chronicle history location is no longer available"))))))

(defun org-chronicle-history-back ()
  "Return to the previous location in the chronicle link history."
  (interactive)
  (org-chronicle--history-visit (org-chronicle--history-go -1)))

(defun org-chronicle-history-forward ()
  "Advance to the next location in the chronicle link history."
  (interactive)
  (org-chronicle--history-visit (org-chronicle--history-go 1)))

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

(defun org-chronicle--source-link (citekey description &optional locator)
  "Return an Org orl: link to a reading-list book.
CITEKEY is the book's :CUSTOM_ID:, DESCRIPTION the display text, and
LOCATOR an optional trailing reference such as a page number.
Square brackets in DESCRIPTION are rewritten to parentheses so they
cannot terminate the bracket link."
  (let ((safe (replace-regexp-in-string
               "\\[" "("
               (replace-regexp-in-string "\\]" ")" description))))
    (concat (format "[[orl:%s][%s]]" citekey safe)
            (when (and locator (not (string-blank-p locator)))
              (concat " " locator)))))

(defun org-chronicle--read-source (&optional free-text)
  "Return a source string.
If `org-reading-list' is loaded, offer to pick an entry and build an
orl: citekey link with an optional locator; otherwise (or on a blank
pick) return FREE-TEXT or a prompted free-text citation."
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

(defun org-chronicle--scan-citations (citekey)
  "Return one plist per chronicle heading whose SOURCES cite CITEKEY.
Each plist has keys :file, :title, :date, and :marker.  Multiple cites of
CITEKEY within one heading collapse to a single entry.  CITEKEY is a
reading-list :CUSTOM_ID:."
  (let ((needle (format "[[orl:%s]" citekey))
        (seen (make-hash-table :test #'equal))
        hits)
    (dolist (file (org-chronicle--source-files))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (while (search-forward needle nil t)
            (save-excursion
              (org-back-to-heading t)
              (let ((key (cons file (point))))
                (unless (gethash key seen)
                  (puthash key t seen)
                  (push (list :file file
                              :title (org-get-heading t t t t)
                              :date (org-entry-get nil "DATE")
                              :marker (point-marker))
                        hits))))))))
    (nreverse hits)))

(defun org-chronicle--read-citation-key ()
  "Return a reading-list citekey to look up citations for.
Default to the :CUSTOM_ID: of the entry at point (e.g. when invoked in
the reading-list buffer); otherwise complete over `org-reading-list-entries'
when available, else read a key as free text."
  (or (and (derived-mode-p 'org-mode) (org-entry-get nil "CUSTOM_ID"))
      (if (and (featurep 'org-reading-list)
               (fboundp 'org-reading-list-entries))
          (let* ((entries (org-reading-list-entries))
                 (pick (completing-read "Book: " (mapcar #'car entries)
                                        nil t)))
            (cdr (assoc pick entries)))
        (read-string "Citekey: "))))

(defvar org-chronicle-citations-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-chronicle-citations-goto)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `org-chronicle-citations-mode'.")

(define-derived-mode org-chronicle-citations-mode special-mode
  "Chronicle-Citations"
  "Major mode for the chronicle book-citations list.")

(defun org-chronicle-citations-goto ()
  "Jump to the chronicle event named on the current line."
  (interactive)
  (let ((m (get-text-property (line-beginning-position)
                              'org-chronicle-marker)))
    (if (and m (marker-buffer m))
        (progn (pop-to-buffer (marker-buffer m))
               (goto-char m)
               (org-reveal))
      (user-error "No event on this line"))))

(defun org-chronicle--show-citations (citekey hits)
  "Display HITS citing CITEKEY in a read-only *Chronicle citations* buffer.
HITS is the value of `org-chronicle--scan-citations'."
  (if (null hits)
      (message "No chronicle events cite %s" citekey)
    (let ((buf (get-buffer-create "*Chronicle citations*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Events citing %s:\n\n" citekey))
          (dolist (h hits)
            (let ((start (point)))
              (insert (format "%s  %s\n"
                              (or (plist-get h :date) "(undated)")
                              (plist-get h :title)))
              (put-text-property start (point) 'org-chronicle-marker
                                 (plist-get h :marker)))))
        (goto-char (point-min))
        (org-chronicle-citations-mode)
        (set-buffer-modified-p nil))
      (pop-to-buffer buf))))

;;;###autoload
(defun org-chronicle-book-citations (&optional citekey)
  "List chronicle events whose SOURCES cite the reading-list book CITEKEY.
Interactively, CITEKEY defaults to the :CUSTOM_ID: of the entry at point
\(handy from the reading-list buffer); otherwise you are prompted.  Opens
a read-only buffer where RET jumps to a citing event and q quits."
  (interactive)
  (let* ((citekey (or citekey (org-chronicle--read-citation-key)))
         (hits (org-chronicle--scan-citations citekey)))
    (org-chronicle--show-citations citekey hits)))





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

(defun org-chronicle--person-birth (name entities idx index)
  "Return the birth date for the person named NAME, or nil.
NAME is resolved through alias index IDX.  Returns nil for place, group, or
topic entities (whose span start is not a birth) and when no birth is known;
otherwise the existence-span start, taken from a birth life event in INDEX or
a person entity's BORN (so people known only through life events are covered)."
  (let* ((canon (org-chronicle--canonical name idx))
         (ent (cl-find canon entities
                       :key (lambda (e) (plist-get e :name)) :test #'equal)))
    (unless (and ent (memq (plist-get ent :kind) '(place group topic)))
      (car (org-chronicle--span-for-name canon entities idx index)))))


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

;;;; Scenes: the chronicle link type

(defface org-chronicle-reference '((t :inherit org-link))
  "Face for inline chronicle: scene references."
  :group 'org-chronicle)

(defun org-chronicle--reference-title (id)
  "Return the heading title for ID among events, entities, and scenes, or nil."
  (or (cl-loop for e in (org-chronicle--all-events)
               when (equal (plist-get e :id) id) return (plist-get e :title))
      (cl-loop for e in (org-chronicle--all-entities)
               when (equal (plist-get e :id) id) return (plist-get e :name))
      (cdr (assoc id (mapcar (lambda (c) (cons (cdr c) (car c)))
                             (org-chronicle--scene-targets
                              (org-chronicle--all-scenes)))))))

(defun org-chronicle--reference-targets ()
  "Return an alist of (DISPLAY . ID) for events, entities, and scenes with an id."
  (append
   (cl-loop for e in (org-chronicle--all-entities)
            for id = (plist-get e :id)
            when id collect (cons (plist-get e :name) id))
   (cl-loop for e in (org-chronicle--all-events)
            for id = (plist-get e :id)
            when id collect (cons (plist-get e :title) id))
   (org-chronicle--scene-targets (org-chronicle--all-scenes))))

(defun org-chronicle--scene-targets (scenes)
  "Return an alist of (TITLE . ID) for SCENES that carry an Org id."
  (cl-loop for s in scenes
           for id = (org-entry-get (plist-get s :marker) "ID")
           when (and id (plist-get s :title))
           collect (cons (plist-get s :title) id)))

(defun org-chronicle--all-scenes ()
  "Return scene plists across all source files, in document order."
  (let ((out '()))
    (dolist (file (org-chronicle--source-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (unless (org-chronicle--file-ignored-p)
            (dolist (scene (org-chronicle--buffer-scenes))
              (push scene out))))))
    (nreverse out)))

(defun org-chronicle--link-follow (path &optional _arg)
  "Follow a chronicle: link by visiting the heading whose id is PATH.
Records the jump in the chronicle link history."
  (let ((origin (org-chronicle--history-location)))
    (org-id-goto path)
    (org-chronicle--history-record origin (org-chronicle--history-location))))

(defun org-chronicle--link-export (path desc &optional _backend _info)
  "Export a chronicle: link as DESC, else the target title, else PATH."
  (or (and desc (not (string-blank-p desc)) desc)
      (org-chronicle--reference-title path)
      path))

(defun org-chronicle--link-complete (&optional _arg)
  "Return a chronicle: link string for an event or entity chosen with completion."
  (let ((targets (org-chronicle--reference-targets)))
    (unless targets
      (user-error "No chronicle reference targets are defined"))
    (concat "chronicle:"
            (cdr (assoc (completing-read "Reference: " targets nil t) targets)))))

(org-link-set-parameters
 "chronicle"
 :follow #'org-chronicle--link-follow
 :export #'org-chronicle--link-export
 :complete #'org-chronicle--link-complete
 :face 'org-chronicle-reference)

;;;; Scenes: parsing references

(defun org-chronicle--extract-ids (value)
  "Return the ids referenced by id: links in property VALUE.
A bare (unlinked) id value is split on the multi-value separator instead."
  (when (and (stringp value) (not (string-blank-p value)))
    (if (string-match-p "\\[\\[id:" value)
        (let ((ids '()) (start 0))
          (while (string-match "\\[\\[id:\\([^]]+?\\)\\]" value start)
            (push (match-string 1 value) ids)
            (setq start (match-end 0)))
          (nreverse ids))
      (org-chronicle--split value))))

(defconst org-chronicle--reference-regexp
  "\\[\\[chronicle:\\([^]]+?\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
  "Regexp matching an inline chronicle: link.
Group 1 is the id, group 2 the optional description.")

(defun org-chronicle--scan-references ()
  "Return inline chronicle references in the current buffer, in order.
Each element is a plist (:id ID :name DESC-or-nil :pos POS :marker MARKER)."
  (let ((refs '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-chronicle--reference-regexp nil t)
        (let ((desc (match-string-no-properties 2)))
          (push (list :id (match-string-no-properties 1)
                      :name (and desc (not (string-blank-p desc)) desc)
                      :pos (match-beginning 0)
                      :marker (copy-marker (match-beginning 0)))
                refs))))
    (nreverse refs)))

;;;; Scenes: gathering

(defun org-chronicle--heading-scene-props ()
  "Return scene-relevant fields of the Org heading at point as a plist."
  (list :title (org-get-heading t t t t)
        :marker (point-marker)
        :begin (point)
        :end (save-excursion (org-end-of-subtree t t) (point))
        :truth (org-entry-get nil "TRUTH")
        :own-date (org-chronicle--date-parse (org-entry-get nil "DATE"))
        :own-date-end (org-chronicle--date-parse (org-entry-get nil "DATE-END"))
        :event-ids (org-chronicle--extract-ids (org-entry-get nil "EVENT"))
        :after-ids (org-chronicle--extract-ids (org-entry-get nil "AFTER"))
        :before-ids (org-chronicle--extract-ids (org-entry-get nil "BEFORE"))
        :earliest (org-chronicle--date-parse (org-entry-get nil "EARLIEST"))
        :latest (org-chronicle--date-parse (org-entry-get nil "LATEST"))))

(defun org-chronicle--structural-scene-p (h)
  "Non-nil if heading plist H is a scene by its own properties."
  (or (plist-get h :event-ids)
      (plist-get h :after-ids)
      (plist-get h :before-ids)
      (plist-get h :earliest)
      (plist-get h :latest)
      (and (plist-get h :own-date)
           (equal (plist-get h :truth) "fictional"))))

(defun org-chronicle--buffer-scenes ()
  "Return scene plists for the current buffer in document order.
A scene is a heading carrying EVENT/AFTER/BEFORE, a fictional own DATE, or
directly containing an inline chronicle: link.  Each scene plist gains a
:refs field: the references it owns, attributed to the nearest enclosing
scene (the deepest heading whose subtree contains the link)."
  (org-with-wide-buffer
   (let ((headings '())
         (refs (org-chronicle--scan-references))
         (owned (make-hash-table :test #'eq)))
     (org-map-entries
      (lambda () (push (org-chronicle--heading-scene-props) headings)))
     (setq headings (nreverse headings))
     (dolist (ref refs)
       (let ((pos (plist-get ref :pos)) (best nil))
         (dolist (h headings)
           (when (and (<= (plist-get h :begin) pos)
                      (< pos (plist-get h :end))
                      (or (null best)
                          (> (plist-get h :begin) (plist-get best :begin))))
             (setq best h)))
         (when best
           (puthash best (cons ref (gethash best owned)) owned))))
     (cl-loop for h in headings
              for hrefs = (nreverse (gethash h owned))
              when (or (org-chronicle--structural-scene-p h) hrefs)
              collect (append h (list :refs hrefs))))))

(defun org-chronicle--scene-at-point ()
  "Return the scene plist for the heading at point, or a bare scene if none.
Falls back to `org-chronicle--heading-scene-props' with no refs so an
unmarked heading can still be treated as a (constraint-free) scene."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((pos (point)))
     (or (cl-find pos (org-chronicle--buffer-scenes)
                  :key (lambda (s) (plist-get s :begin)) :test #'=)
         (append (org-chronicle--heading-scene-props) (list :refs nil))))))

;;;; Scenes: shared context and bounds

(defun org-chronicle--date-max (a b)
  "Return the later of date plists A and B; nil means open (return the other)."
  (cond ((null a) b) ((null b) a)
        ((org-chronicle--date-lessp a b) b) (t a)))

(defun org-chronicle--date-min (a b)
  "Return the earlier of date plists A and B; nil means open (return the other)."
  (cond ((null a) b) ((null b) a)
        ((org-chronicle--date-lessp b a) b) (t a)))

(defun org-chronicle--name-adoption (events idx)
  "Return a hash from (CANON . DOWNCASED-NEW-NAME) to the change date.
Built from name-change life events in EVENTS; CANON is the canonical subject
name resolved through alias index IDX."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (e events h)
      (when (equal (plist-get e :life-event) "name-change")
        (let ((canon (org-chronicle--canonical (car (plist-get e :subject)) idx))
              (new (plist-get e :new-name))
              (date (plist-get e :date)))
          (when (and canon new date)
            (puthash (cons canon (downcase new)) date h)))))))

(defun org-chronicle--scene-context ()
  "Gather the data shared by the scene lint and the date-solving command.
Returns a plist with :entities :idx :events :index :adoption :events-by-id."
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (events (org-chronicle--all-events))
         (index (org-chronicle--life-index events idx))
         (adoption (org-chronicle--name-adoption events idx))
         (by-id (make-hash-table :test #'equal)))
    (dolist (e events)
      (when (plist-get e :id) (puthash (plist-get e :id) e by-id)))
    (list :entities entities :idx idx :events events :index index
          :adoption adoption :events-by-id by-id)))

(defvar org-chronicle--context-cache nil
  "Cached value of `org-chronicle--scene-context', or nil when stale.")

(defun org-chronicle--cached-context ()
  "Return the scene context, computing and caching it when stale."
  (or org-chronicle--context-cache
      (setq org-chronicle--context-cache (org-chronicle--scene-context))))

(defun org-chronicle--invalidate-context ()
  "Drop the cached scene context so the next solve recomputes it."
  (setq org-chronicle--context-cache nil))

(defun org-chronicle--maybe-invalidate-context ()
  "Invalidate the context cache when the buffer's file is under the root.
Used on `after-save-hook' and `after-revert-hook' so the cache stays
consistent with saves and auto-reverted external changes."
  (when (org-chronicle--file-under-root-p buffer-file-name)
    (org-chronicle--invalidate-context)))

(add-hook 'after-save-hook #'org-chronicle--maybe-invalidate-context)

(add-hook 'after-revert-hook #'org-chronicle--maybe-invalidate-context)

;;;; Scenes: window and anchor

(defun org-chronicle--scene-window (scene ctx)
  "Return the feasible window for SCENE as (LO . HI), or :empty.
LO and HI are date plists or nil (open).  Bounds come from referenced entity
existence spans, name-validity onsets, and AFTER/BEFORE constraints, all read
against current dates in CTX.  Undated referents contribute no bound."
  (let ((lo nil) (hi nil)
        (entities (plist-get ctx :entities))
        (idx (plist-get ctx :idx))
        (index (plist-get ctx :index))
        (adoption (plist-get ctx :adoption))
        (by-id (plist-get ctx :events-by-id)))
    (dolist (ref (plist-get scene :refs))
      (let ((ent (org-chronicle--entity-by-id (plist-get ref :id) entities)))
        (when ent
          (let ((span (org-chronicle--span-for-name
                       (plist-get ent :name) entities idx index)))
            (when span
              (setq lo (org-chronicle--date-max lo (car span)))
              (setq hi (org-chronicle--date-min hi (cdr span)))))
          (let* ((name (plist-get ref :name))
                 (adopt (and name (gethash (cons (plist-get ent :name)
                                                 (downcase name))
                                           adoption))))
            (when adopt (setq lo (org-chronicle--date-max lo adopt)))))))
    (dolist (id (plist-get scene :after-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (or (plist-get ev :date-end) (plist-get ev :date))))
            (when d (setq lo (org-chronicle--date-max lo d)))))))
    (dolist (id (plist-get scene :before-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (plist-get ev :date)))
            (when d (setq hi (org-chronicle--date-min hi d)))))))
    (if (and lo hi (org-chronicle--date-lessp hi lo)) :empty (cons lo hi))))

(defun org-chronicle--scene-anchor (scene ctx)
  "Return SCENE's temporal anchor as (START . END) date plists, or nil.
Own DATE (with optional DATE-END) wins; else the span across the EVENT
events resolved through CTX; else nil (floating)."
  (cond
   ((plist-get scene :own-date)
    (cons (plist-get scene :own-date)
          (or (plist-get scene :own-date-end) (plist-get scene :own-date))))
   ((plist-get scene :event-ids)
    (let ((by-id (plist-get ctx :events-by-id)) (starts '()) (ends '()))
      (dolist (id (plist-get scene :event-ids))
        (let ((ev (gethash id by-id)))
          (when (and ev (plist-get ev :date))
            (push (plist-get ev :date) starts)
            (push (or (plist-get ev :date-end) (plist-get ev :date)) ends))))
      (when starts
        (cons (cl-reduce #'org-chronicle--date-min starts)
              (cl-reduce #'org-chronicle--date-max ends)))))
   (t nil)))

;;;; Scenes: findings

(defun org-chronicle--span-string (span)
  "Format existence SPAN (FROM . TO) date plists as \"from..to\"."
  (format "%s..%s"
          (if (car span) (org-chronicle--date-format (car span)) "?")
          (if (cdr span) (org-chronicle--date-format (cdr span)) "?")))

(defun org-chronicle--scene-dangling (scene ctx)
  "Return ((MSG . MARKER) ...) for unresolved references in SCENE.
CTX is the shared scene context."
  (let ((entities (plist-get ctx :entities))
        (by-id (plist-get ctx :events-by-id))
        (out '()))
    (dolist (id (plist-get scene :event-ids))
      (unless (gethash id by-id)
        (push (cons (format "dangling :EVENT: [[id:%s]] — no such event" id)
                    (plist-get scene :marker)) out)))
    (dolist (id (append (plist-get scene :after-ids) (plist-get scene :before-ids)))
      (unless (gethash id by-id)
        (push (cons (format "dangling constraint [[id:%s]] — no such event" id)
                    (plist-get scene :marker)) out)))
    (dolist (ref (plist-get scene :refs))
      (unless (or (org-chronicle--entity-by-id (plist-get ref :id) entities)
                  (gethash (plist-get ref :id) by-id))
        (push (cons (format "dangling reference [[chronicle:%s]] — unresolved"
                            (plist-get ref :id))
                    (plist-get ref :marker)) out)))
    (nreverse out)))

(defun org-chronicle--scene-violation-reasons (scene ctx date)
  "Return ((MSG . MARKER) ...) explaining why DATE violates SCENE's bounds.
CTX is the shared scene context."
  (let ((entities (plist-get ctx :entities))
        (idx (plist-get ctx :idx))
        (index (plist-get ctx :index))
        (adoption (plist-get ctx :adoption))
        (by-id (plist-get ctx :events-by-id))
        (out '()))
    (dolist (ref (plist-get scene :refs))
      (let ((ent (org-chronicle--entity-by-id (plist-get ref :id) entities)))
        (when ent
          (let ((span (org-chronicle--span-for-name
                       (plist-get ent :name) entities idx index)))
            (when (and span (not (org-chronicle--date-in-span-p
                                  date (car span) (cdr span))))
              (push (cons (format "%s not extant at %s (span %s)"
                                  (plist-get ent :name)
                                  (org-chronicle--date-format date)
                                  (org-chronicle--span-string span))
                          (plist-get ref :marker)) out)))
          (let* ((name (plist-get ref :name))
                 (adopt (and name (gethash (cons (plist-get ent :name)
                                                 (downcase name)) adoption))))
            (when (and adopt (org-chronicle--date-lessp date adopt))
              (push (cons (format "name %S not adopted until %s"
                                  name (org-chronicle--date-format adopt))
                          (plist-get ref :marker)) out))))))
    (dolist (id (plist-get scene :after-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (or (plist-get ev :date-end) (plist-get ev :date))))
            (cond ((null d)
                   (push (cons (format ":AFTER: %s unresolved (referent undated)"
                                       (plist-get ev :title))
                               (plist-get scene :marker)) out))
                  ((org-chronicle--date-lessp date d)
                   (push (cons (format "violates :AFTER: %s (must be on/after %s)"
                                       (plist-get ev :title)
                                       (org-chronicle--date-format d))
                               (plist-get scene :marker)) out)))))))
    (dolist (id (plist-get scene :before-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (plist-get ev :date)))
            (cond ((null d)
                   (push (cons (format ":BEFORE: %s unresolved (referent undated)"
                                       (plist-get ev :title))
                               (plist-get scene :marker)) out))
                  ((org-chronicle--date-lessp d date)
                   (push (cons (format "violates :BEFORE: %s (must be on/before %s)"
                                       (plist-get ev :title)
                                       (org-chronicle--date-format d))
                               (plist-get scene :marker)) out)))))))
    (nreverse out)))

(defun org-chronicle--scene-conflict-reasons (scene ctx)
  "Return ((MSG . MARKER) ...) describing the bounds that make SCENE empty.
CTX is the shared scene context."
  (let ((entities (plist-get ctx :entities))
        (idx (plist-get ctx :idx))
        (index (plist-get ctx :index))
        (by-id (plist-get ctx :events-by-id))
        (out '()))
    (dolist (ref (plist-get scene :refs))
      (let ((ent (org-chronicle--entity-by-id (plist-get ref :id) entities)))
        (when ent
          (let ((span (org-chronicle--span-for-name
                       (plist-get ent :name) entities idx index)))
            (when span
              (push (cons (format "%s requires %s"
                                  (plist-get ent :name)
                                  (org-chronicle--span-string span))
                          (plist-get ref :marker)) out))))))
    (dolist (id (plist-get scene :after-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (or (plist-get ev :date-end) (plist-get ev :date))))
            (when d (push (cons (format "after %s (%s)" (plist-get ev :title)
                                        (org-chronicle--date-format d))
                                (plist-get scene :marker)) out))))))
    (dolist (id (plist-get scene :before-ids))
      (let ((ev (gethash id by-id)))
        (when ev
          (let ((d (plist-get ev :date)))
            (when d (push (cons (format "before %s (%s)" (plist-get ev :title)
                                        (org-chronicle--date-format d))
                                (plist-get scene :marker)) out))))))
    (nreverse out)))

(defun org-chronicle--scene-findings (scene ctx)
  "Return a finding plist for SCENE, or nil if it is clean.
The plist is (:scene :verdict :window :anchor :offending :reasons); :verdict
is `dangling', `empty', `out-of-window', or `floating'; :offending is the
anchor endpoint that violates the window (out-of-window only).  CTX is the
shared scene context."
  (let ((dangling (org-chronicle--scene-dangling scene ctx))
        (window (org-chronicle--scene-window scene ctx))
        (anchor (org-chronicle--scene-anchor scene ctx)))
    (cond
     (dangling
      (list :scene scene :verdict 'dangling :window window :anchor anchor
            :reasons dangling))
     ((eq window :empty)
      (list :scene scene :verdict 'empty :window window :anchor anchor
            :reasons (org-chronicle--scene-conflict-reasons scene ctx)))
     ((null anchor)
      (list :scene scene :verdict 'floating :window window :anchor anchor
            :reasons nil))
     (t
      (let* ((lo (car window)) (hi (cdr window))
             (astart (car anchor)) (aend (cdr anchor))
             (bad (cond ((not (org-chronicle--date-in-span-p astart lo hi)) astart)
                        ((not (org-chronicle--date-in-span-p aend lo hi)) aend))))
        (when bad
          (list :scene scene :verdict 'out-of-window :window window :anchor anchor
                :offending bad
                :reasons (org-chronicle--scene-violation-reasons
                          scene ctx bad))))))))

(declare-function org-chronicle--solution "org-chronicle-solve" (scenes ctx))
(declare-function org-chronicle--earliest-placement "org-chronicle-solve" (scenes ctx))

;;;; Scenes: the lint command

(defun org-chronicle--all-scene-findings ()
  "Return (FILE . FINDING) pairs across all source files, in document order."
  (let ((ctx (org-chronicle--scene-context)) (out '()))
    (dolist (file (org-chronicle--source-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (unless (org-chronicle--file-ignored-p)
            (dolist (scene (org-chronicle--buffer-scenes))
              (let ((f (org-chronicle--scene-findings scene ctx)))
                (when f (push (cons file f) out))))))))
    (nreverse out)))

(defun org-chronicle--window-string (window)
  "Format WINDOW (a (LO . HI) cons or :empty) for display."
  (cond ((eq window :empty) "empty")
        ((and (null (car window)) (null (cdr window))) "unbounded")
        (t (format "%s .. %s"
                   (if (car window) (org-chronicle--date-format (car window)) "…")
                   (if (cdr window) (org-chronicle--date-format (cdr window)) "…")))))

(defun org-chronicle--scene-verdict-line (f)
  "Return the one-line verdict summary for finding F."
  (let ((window (plist-get f :window)))
    (pcase (plist-get f :verdict)
      ('dangling "unresolved reference(s)")
      ('empty "empty window — references cannot co-exist")
      ('floating (format "floating — needs placing; valid in %s"
                         (org-chronicle--window-string window)))
      ('out-of-window
       (format "date %s outside feasible window %s"
               (org-chronicle--date-format
                (or (plist-get f :offending) (car (plist-get f :anchor))))
               (org-chronicle--window-string window))))))

(defun org-chronicle--scene-verdict-glyph (verdict)
  "Return the severity glyph for VERDICT."
  (if (eq verdict 'floating) "○" "✗"))

(defvar org-chronicle-scene-lint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-chronicle-view-mode-map)
    (define-key map (kbd "g") #'org-chronicle-lint-scenes)
    (define-key map (kbd "s") #'org-chronicle-set-scene-date)
    (define-key map (kbd "a") #'org-chronicle-accept-placement)
    (define-key map (kbd "A") #'org-chronicle-accept-all-placements)
    (define-key map (kbd "m") #'org-chronicle-mark-scene)
    map)
  "Keymap for `org-chronicle-scene-lint-mode'.")

(defvar-local org-chronicle--lint-placements nil
  "Hash table mapping each floating scene marker to its earliest placement.
Set by `org-chronicle-lint-scenes' and read by the accept commands.")

(define-derived-mode org-chronicle-scene-lint-mode org-chronicle-view-mode
  "Chronicle-Scenes"
  "Major mode for the scene continuity lint buffer.")

(defun org-chronicle-accept-placement ()
  "Accept the solver-suggested earliest date for the scene at point.
Writes the date via `org-chronicle--commit-placement', invalidates the context
cache, and refreshes the lint buffer.  Signals if there is no scene or no
suggested date at point."
  (interactive)
  (require 'org-chronicle-solve)
  (let ((marker (get-text-property (point) 'org-chronicle-marker)))
    (unless (and marker (marker-buffer marker))
      (user-error "No scene at point"))
    (let ((date (and org-chronicle--lint-placements
                     (gethash marker org-chronicle--lint-placements))))
      (unless date
        (user-error "No suggested date for this scene (unbounded or already placed)"))
      (with-current-buffer (marker-buffer marker)
        (org-chronicle--commit-placement marker date))
      (org-chronicle--invalidate-context)
      (org-chronicle-lint-scenes))))

(defun org-chronicle-accept-all-placements ()
  "Accept all solver-suggested earliest dates for floating scenes.
Confirms with `yes-or-no-p' before writing.  Commits only scenes that have a
bounded earliest date.  Invalidates the context cache and refreshes the lint
buffer afterward."
  (interactive)
  (require 'org-chronicle-solve)
  (unless (yes-or-no-p "Accept all suggested placements? ")
    (user-error "Cancelled"))
  (let ((placements org-chronicle--lint-placements)
        (count 0))
    (when placements
      (maphash
       (lambda (marker date)
         (when (and date (marker-buffer marker))
           (with-current-buffer (marker-buffer marker)
             (org-chronicle--commit-placement marker date))
           (cl-incf count)))
       placements))
    (org-chronicle--invalidate-context)
    (org-chronicle-lint-scenes)
    (message "Accepted %d placement(s)." count)))

;;;###autoload
(defun org-chronicle-lint-scenes ()
  "Report scenes whose references or constraints conflict with the timeline.
Uses the constraint solver to propagate bounds across all scenes.
Shows a suggested-placement section for floating scenes, and the conflict
cycle when the network is inconsistent.  Keys: \\[org-chronicle-set-scene-date]
prompts for a date, \\[org-chronicle-accept-placement] accepts the suggested
date for the scene at point, \\[org-chronicle-accept-all-placements] accepts all."
  (interactive)
  (require 'org-chronicle-solve)
  (let* ((root (expand-file-name org-chronicle-root))
         (scenes (org-chronicle--all-scenes))
         (ctx (org-chronicle--cached-context))
         (sol (org-chronicle--solution scenes ctx))
         (consistent (plist-get sol :consistent))
         (windows (plist-get sol :windows))
         (conflict (plist-get sol :conflict))
         (placements (org-chronicle--earliest-placement scenes ctx))
         (issues 0))
    (with-current-buffer (get-buffer-create "*org-chronicle-lint-scenes*")
      (org-chronicle-scene-lint-mode)
      (setq-local org-chronicle--lint-placements placements)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Conflict banner when the network is inconsistent.
        (when (not consistent)
          (insert "⚠ Constraint network is inconsistent — conflict cycle:\n\n")
          (dolist (lbl conflict)
            (let ((desc (plist-get lbl :desc))
                  (m (plist-get lbl :marker)))
              (insert (propertize (format "    ✗ %s\n" desc)
                                  'org-chronicle-marker m))))
          (insert "\n"))
        ;; Per-scene findings.
        (dolist (scene scenes)
          (let* ((m (plist-get scene :marker))
                 (file (buffer-file-name (marker-buffer m)))
                 (dangling (org-chronicle--scene-dangling scene ctx))
                 (win (and windows (gethash m windows)))
                 (own-date (plist-get scene :own-date))
                 (violations (and own-date
                                  (org-chronicle--scene-violation-reasons
                                   scene ctx own-date)))
                 (out-of-win (and own-date win
                                  (not (org-chronicle--date-in-span-p
                                        own-date (car win) (cdr win))))))
            (cond
             ;; Dangling reference(s): unresolved id links.
             (dangling
              (cl-incf issues)
              (insert (propertize
                       (format "✗ %S  (%s)\n"
                               (plist-get scene :title)
                               (file-relative-name file root))
                       'org-chronicle-marker m))
              (insert (propertize "    unresolved reference(s)\n"
                                  'org-chronicle-marker m))
              (dolist (r dangling)
                (insert (propertize (format "    · %s\n" (car r))
                                    'org-chronicle-marker (cdr r))))
              (insert "\n"))
             ;; Inconsistent global network: report each anchored scene.
             ((not consistent)
              (when own-date
                (cl-incf issues)
                (insert (propertize
                         (format "✗ %S  (%s)\n"
                                 (plist-get scene :title)
                                 (file-relative-name file root))
                         'org-chronicle-marker m))
                (insert (propertize "    part of over-constrained network\n"
                                    'org-chronicle-marker m))
                (insert "\n")))
             ;; Out-of-window: solver's propagated window excludes the own date.
             (out-of-win
              (cl-incf issues)
              (insert (propertize
                       (format "✗ %S  (%s)\n"
                               (plist-get scene :title)
                               (file-relative-name file root))
                       'org-chronicle-marker m))
              (insert (propertize
                       (format "    date %s outside feasible window %s\n"
                               (org-chronicle--date-format own-date)
                               (org-chronicle--window-string win))
                       'org-chronicle-marker m))
              (dolist (r violations)
                (insert (propertize (format "    · %s\n" (car r))
                                    'org-chronicle-marker (cdr r))))
              (insert "\n"))
             ;; Semantic violations without STN conflict (e.g. name not adopted).
             (violations
              (cl-incf issues)
              (insert (propertize
                       (format "✗ %S  (%s)\n"
                               (plist-get scene :title)
                               (file-relative-name file root))
                       'org-chronicle-marker m))
              (dolist (r violations)
                (insert (propertize (format "    · %s\n" (car r))
                                    'org-chronicle-marker (cdr r))))
              (insert "\n"))
             ;; Floating: no own date — show the solver window.
             ((null own-date)
              (cl-incf issues)
              (insert (propertize
                       (format "○ %S  (%s)\n"
                               (plist-get scene :title)
                               (file-relative-name file root))
                       'org-chronicle-marker m))
              (insert (propertize
                       (format "    floating — needs placing; valid in %s\n"
                               (if win (org-chronicle--window-string win)
                                 "unbounded"))
                       'org-chronicle-marker m))
              (insert "\n")))))
        ;; Summary header: inserted at the top once we know the count.
        (goto-char (point-min))
        (when consistent
          (insert (if (= issues 0)
                      "No scene issues found.\n"
                    (format "%d scene issue(s):\n\n" issues))))
        ;; Suggested placements section.
        (goto-char (point-max))
        (let ((floats (cl-remove-if
                       (lambda (s) (plist-get s :own-date))
                       scenes)))
          (when (and floats consistent)
            (insert "\nSuggested placements (solver earliest feasible):\n\n")
            (dolist (s floats)
              (let* ((m (plist-get s :marker))
                     (date (gethash m placements)))
                (insert (propertize
                         (format "  %s  →  %s\n"
                                 (plist-get s :title)
                                 (if date
                                     (org-chronicle--date-format date)
                                   "unbounded (no lower limit)"))
                         'org-chronicle-marker m)))))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Scenes: authoring commands

(defun org-chronicle--read-reference ()
  "Read an event/entity target with completion; return (ID . NAME).
Scene candidates are annotated with [chapter · window].  When the
most-recent `org-stored-links' entry resolves to a known target, it
seeds the default (shown as \"(default X)\", returned on empty RET)."
  (require 'org-chronicle-solve)
  (let* ((targets (org-chronicle--reference-targets))
         (scenes (org-chronicle--all-scenes))
         (ctx (org-chronicle--scene-context))
         (sol (org-chronicle--solution scenes ctx))
         (windows (and (plist-get sol :consistent)
                       (plist-get sol :windows)))
         (annotations (make-hash-table :test #'equal))
         (default
          (let* ((entry (car org-stored-links))
                 (link (and entry (car entry)))
                 (id (and link
                          (string-prefix-p "id:" link)
                          (substring link 3)))
                 (title (and id (org-chronicle--reference-title id))))
            (and title (assoc title targets) title))))
    (dolist (s scenes)
      (let* ((title (plist-get s :title))
             (marker (plist-get s :marker))
             (chapter (and marker
                           (buffer-file-name (marker-buffer marker))
                           (file-name-nondirectory
                            (buffer-file-name (marker-buffer marker)))))
             (win (and windows (gethash marker windows)))
             (win-str (if win (org-chronicle--window-string win) "")))
        (when title
          (puthash title
                   (format "[%s · %s]"
                           (or chapter "")
                           win-str)
                   annotations))))
    (let* ((completion-extra-properties
            (list :affixation-function
                  (org-chronicle--affixation-function annotations)))
           (name (completing-read "Reference: " targets nil t nil nil default)))
      (cons (cdr (assoc name targets)) name))))

(defun org-chronicle--affixation-function (annotations)
  "Return a `completing-read' affixation function using ANNOTATIONS.
ANNOTATIONS maps a candidate string to a suffix string."
  (lambda (cands)
    (mapcar (lambda (c)
              (list c "" (concat "  " (or (gethash c annotations) ""))))
            cands)))

(defun org-chronicle-mark-scene ()
  "Mark the scene at point as the default target for the next constraint.
Stores an id link to the heading via `org-store-link'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-id-get-create)
    (call-interactively #'org-store-link)))

;;;###autoload
(defun org-chronicle-insert-reference ()
  "Insert an inline chronicle: link to an event or entity at point."
  (interactive)
  (let ((target (org-chronicle--read-reference)))
    (insert (format "[[chronicle:%s][%s]]" (car target) (cdr target)))))

;;;###autoload
(defun org-chronicle-set-event ()
  "Set the :EVENT: property of the heading at point to a chosen event."
  (interactive)
  (let ((target (org-chronicle--read-reference)))
    (org-set-property "EVENT" (format "[[id:%s]]" (car target)))))

;;;###autoload
(defun org-chronicle-add-constraint (kind)
  "Add an :AFTER: or :BEFORE: ordering constraint to the heading at point.
KIND is the symbol `after' or `before'."
  (interactive
   (list (intern (completing-read "Constraint: " '("after" "before") nil t))))
  (let* ((prop (upcase (symbol-name kind)))
         (target (org-chronicle--read-reference))
         (target-id (car target))
         (existing (org-entry-get nil prop))
         (link (format "[[id:%s]]" target-id)))
    (when (null target-id)
      (let* ((name (cdr target))
             (scene (cl-find name (org-chronicle--all-scenes)
                             :key (lambda (s) (plist-get s :title))
                             :test #'equal)))
        (when scene
          (with-current-buffer (marker-buffer (plist-get scene :marker))
            (goto-char (plist-get scene :marker))
            (setq target-id (org-id-get-create))
            (setq link (format "[[id:%s]]" target-id))))))
    (unless target-id
      (user-error "Could not resolve target %S to an id" (cdr target)))
    (org-set-property
     prop (if (and existing (not (string-blank-p existing)))
              (org-chronicle--join (list existing link))
            link))))

;;;; Scenes: solving the date

(defun org-chronicle--set-scene-date-here ()
  "Set the :DATE: of the scene heading at point, prompting within its window."
  (org-back-to-heading t)
  (let* ((ctx (org-chronicle--scene-context))
         (scene (org-chronicle--scene-at-point))
         (window (org-chronicle--scene-window scene ctx)))
    (when (eq window :empty)
      (user-error "Over-constrained — resolve conflicting references first"))
    (let* ((default (and (car window) (org-chronicle--date-format (car window))))
           (prompt (if (or (car window) (cdr window))
                       (format "Date (window %s): "
                               (org-chronicle--window-string window))
                     "Date: "))
           (value (read-string prompt nil nil default)))
      (when (string-blank-p value)
        (user-error "A date is required"))
      (org-set-property "DATE" (org-chronicle--ts value))
      (unless (or (plist-get scene :event-ids) (org-entry-get nil "TRUTH"))
        (org-set-property "TRUTH" "fictional")))))

(defun org-chronicle--commit-placement (marker date)
  "Write DATE (a date plist) as the :DATE: of the scene heading at MARKER.
Marks a purely-invented scene `fictional', mirroring the prompted commit.
Signals if DATE is nil (an unbounded suggestion cannot be committed)."
  (unless date (user-error "No suggested date to accept"))
  (save-excursion
    (goto-char marker)
    (org-back-to-heading t)
    (org-set-property "DATE" (org-chronicle--ts (org-chronicle--date-format date)))
    (unless (or (org-chronicle--extract-ids (org-entry-get nil "EVENT"))
                (org-entry-get nil "TRUTH"))
      (org-set-property "TRUTH" "fictional"))))

;;;###autoload
(defun org-chronicle-set-scene-date ()
  "Set a scene's :DATE: to a value within its feasible window.
At point in prose, act on the scene heading at point.  In the scene-lint
buffer, act on the scene at the finding under point, then refresh the lint."
  (interactive)
  (let ((marker (and (derived-mode-p 'org-chronicle-scene-lint-mode)
                     (get-text-property (point) 'org-chronicle-marker))))
    (if (and marker (marker-buffer marker))
        (progn
          (with-current-buffer (marker-buffer marker)
            (save-excursion
              (goto-char marker)
              (org-chronicle--set-scene-date-here)))
          (org-chronicle-lint-scenes))
      (org-chronicle--set-scene-date-here))))

(defun org-chronicle--peek-string (verdict window)
  "Return a one-line description of VERDICT and WINDOW for the echo area."
  (format "%s — %s"
          (symbol-name verdict)
          (org-chronicle--window-string window)))

(defvar-local org-chronicle--window-overlays nil
  "List of overlays placed by `org-chronicle-annotate-windows' in this buffer.
All overlays are ephemeral: they carry no file content and are removed when
the command is toggled off or the buffer is killed.")

(defvar-local org-chronicle--window-overlays-active nil
  "Non-nil when `org-chronicle-annotate-windows' is enabled in this buffer.
Tracks enabled state independently of the overlay list so toggling works
correctly even when there are zero floating scenes (overlay list stays nil).")


(defun org-chronicle-peek ()
  "Show the feasible window for the scene at point in the echo area.
Computes the window by solving over all scenes so propagation is honoured,
then reads the window for the heading at point from the solution."
  (interactive)
  (require 'org-chronicle-solve)
  (let* ((scene (org-chronicle--scene-at-point))
         (m (plist-get scene :marker))
         (scenes (org-chronicle--all-scenes))
         (ctx (org-chronicle--cached-context))
         (sol (org-chronicle--solution scenes ctx))
         (consistent (plist-get sol :consistent))
         (windows (plist-get sol :windows))
         (win (and windows m (gethash m windows)))
         (own-date (plist-get scene :own-date))
         (verdict (cond
                   ((not consistent) 'inconsistent)
                   ((null own-date) 'floating)
                   ((and win
                         (not (org-chronicle--date-in-span-p
                               own-date (car win) (cdr win))))
                    'out-of-window)
                   (t 'consistent))))
    (message "%s" (org-chronicle--peek-string verdict (or win (cons nil nil))))))

(defun org-chronicle--annotate-windows-refresh ()
  "Remove all window overlays in the current buffer and redraw them.
Called by `org-chronicle-annotate-windows' on `after-save-hook' while
the annotation is active."
  (require 'org-chronicle-solve)
  (let* ((scenes (org-chronicle--all-scenes))
         (ctx (org-chronicle--cached-context))
         (sol (org-chronicle--solution scenes ctx))
         (windows (plist-get sol :windows)))
    (mapc #'delete-overlay org-chronicle--window-overlays)
    (setq org-chronicle--window-overlays nil)
    (when windows
      (dolist (scene scenes)
        (let* ((m (plist-get scene :marker))
               (own-date (plist-get scene :own-date))
               (win (gethash m windows)))
          (when (and (null own-date) win (marker-buffer m)
                     (eq (marker-buffer m) (current-buffer)))
            (let* ((ov (make-overlay (marker-position m)
                                     (marker-position m)
                                     (current-buffer) nil t))
                   (label (format "  ⟦%s⟧" (org-chronicle--window-string win))))
              (overlay-put ov 'after-string label)
              (overlay-put ov 'org-chronicle-window-overlay t)
              (push ov org-chronicle--window-overlays))))))))

(defun org-chronicle-annotate-windows ()
  "Toggle ephemeral window overlays on floating scene headings in this buffer.
When enabling, draws an overlay after each floating scene heading whose
after-string shows the feasible window as ⟦LO .. HI⟧.  Overlays are stored
in `org-chronicle--window-overlays' and refreshed on `after-save-hook'.
When disabling, removes all overlays and the save-hook refresh.  Overlays
are never written to disk and do not set the buffer-modified flag."
  (interactive)
  (if org-chronicle--window-overlays-active
      (progn
        (mapc #'delete-overlay org-chronicle--window-overlays)
        (setq org-chronicle--window-overlays nil)
        (remove-hook 'after-save-hook
                     #'org-chronicle--annotate-windows-refresh t)
        (setq org-chronicle--window-overlays-active nil)
        (message "Window overlays removed."))
    (org-chronicle--annotate-windows-refresh)
    (add-hook 'after-save-hook
              #'org-chronicle--annotate-windows-refresh nil t)
    (setq org-chronicle--window-overlays-active t)
    (message "Window overlays drawn (%d floating scene(s))."
             (length org-chronicle--window-overlays))))

;;;; (sections added by later tasks)

(provide 'org-chronicle)

;;; org-chronicle.el ends here
