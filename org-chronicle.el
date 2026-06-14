;;; org-chronicle.el --- Event timeline for historical fiction -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rob Duncan

;; Author: Rob Duncan
;; URL: https://github.com/YOUR-USERNAME/org-chronicle
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

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)

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
  "Non-nil if date plist A sorts strictly before date plist B."
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

(defcustom org-chronicle-timeline-file "~/org/timeline.org"
  "Org file holding one heading per timeline event."
  :type 'file
  :group 'org-chronicle)

(defcustom org-chronicle-entities-files '("~/org/people.org" "~/org/places.org")
  "Org files holding promoted person, place, and group entities."
  :type '(repeat file)
  :group 'org-chronicle)

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
        :date (org-chronicle--date-parse (org-entry-get nil "DATE"))
        :date-end (org-chronicle--date-parse (org-entry-get nil "DATE-END"))
        :people (org-chronicle--split (org-entry-get nil "PEOPLE"))
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
  "Return event plists from FILE."
  (with-current-buffer (find-file-noselect file)
    (org-chronicle--buffer-events)))

(defun org-chronicle--all-events ()
  "Return event plists from `org-chronicle-timeline-file'."
  (org-chronicle--file-events org-chronicle-timeline-file))

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
  "Return entity plists from every file in `org-chronicle-entities-files'."
  (cl-loop for file in org-chronicle-entities-files
           when (file-exists-p (expand-file-name file))
           append (with-current-buffer (find-file-noselect file)
                    (org-chronicle--buffer-entities))))

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
  "Return names of the place PLACE-ID and all places PART-OF it, transitively."
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
  "Filter EVENTS (resolving names via alias index IDX).
TRUTH is a list of allowed truth strings (nil = all).  FROM and UNTIL are
date plists bounding `:date' inclusively (nil = open).  Returns events
sorted ascending by date."
  (let ((out (cl-remove-if-not
              (lambda (e)
                (and (plist-get e :date)
                     (or (null truth) (member (plist-get e :truth) truth))
                     (org-chronicle--date-in-span-p (plist-get e :date) from until)))
              events)))
    (sort out (lambda (a b)
                (org-chronicle--date-lessp (plist-get a :date) (plist-get b :date))))))

(defun org-chronicle--lane-names-for (name domain entities)
  "Return the set of canonical names an entity NAME contributes to a lane.
For a group, that is its members; for a parent place, its descendants;
otherwise the entity's own canonical name.  DOMAIN selects which kinds of
expansion apply (`people' expands groups, `location' expands places)."
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
  "Build a list of lane plists for NAME.
With MODE `:expand', a group/parent-place yields one lane per resolved
member/descendant; with `:collapse' it yields a single lane (see
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
                    names))))))

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

(defun org-chronicle--cell-text (event)
  "Return the propertized cell text (title + truth marker) for EVENT."
  (let ((s (format "%s %s" (plist-get event :title)
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
         (rule (make-string (length header) ?-))
         (lines (list rule header)))
    (let ((by-date '()))
      (dolist (e events)
        (let ((key (org-chronicle--date-format (plist-get e :date))))
          (push e (alist-get key by-date nil nil #'equal))))
      (setq by-date (nreverse by-date))
      (dolist (cell by-date)
        (let* ((date (car cell))
               (day-events (nreverse (cdr cell)))
               (row (org-chronicle--pad date dcw)))
          (dolist (lane lanes)
            (let* ((hits (cl-remove-if-not
                          (lambda (e) (org-chronicle--event-in-lane-p e lane idx))
                          day-events))
                   (txt (mapconcat #'org-chronicle--cell-text hits " / ")))
              (setq row (concat row (org-chronicle--pad txt col-width)))))
          (push row lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

;;;; View

(defcustom org-chronicle-lane-column-width 22
  "Width in columns of each lane in the timeline view."
  :type 'integer
  :group 'org-chronicle)

(defun org-chronicle--lanes-from-params (people locations entities mode)
  "Build the list of lane plists from PEOPLE and LOCATIONS name lists.
MODE is `:collapse' or `:expand'; ENTITIES is the entity list."
  (append
   (cl-loop for n in people
            append (org-chronicle--build-lanes-for n 'people entities mode))
   (cl-loop for n in locations
            append (org-chronicle--build-lanes-for n 'location entities mode))))

(cl-defun org-chronicle--compose (&key people locations truth from until (mode :collapse))
  "Return the rendered timeline string for the given filters.
PEOPLE/LOCATIONS are name lists naming lanes; TRUTH a list of allowed
truth strings; FROM/UNTIL date strings; MODE `:collapse' or `:expand'."
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (lanes (org-chronicle--lanes-from-params people locations entities mode))
         (events (org-chronicle--filter-events
                  (org-chronicle--all-events) idx
                  :truth truth
                  :from (and from (org-chronicle--date-parse from))
                  :until (and until (org-chronicle--date-parse until)))))
    (org-chronicle--render events lanes idx org-chronicle-lane-column-width)))

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
(cl-defun org-chronicle-timeline (&key people locations truth from until (mode :collapse))
  "Display a swimlane timeline filtered by PEOPLE, LOCATIONS, TRUTH, FROM, UNTIL.
PEOPLE and LOCATIONS are lists of names that become lanes.  MODE is
`:collapse' (default) or `:expand' for groups/parent places.  Interactively,
prompts for people, locations, and a truth subset."
  (interactive
   (let* ((entities (org-chronicle--all-entities))
          (names (mapcar (lambda (e) (plist-get e :name)) entities)))
     (list :people (completing-read-multiple "People/groups (lanes): " names)
           :locations (completing-read-multiple "Places (lanes): " names)
           :truth (let ((v (completing-read-multiple
                            "Truth (blank=all): "
                            '("historical" "fictionalized" "fictional"))))
                    (and v (delete "" v)))
           :mode (if (y-or-n-p "Expand groups into member lanes? ") :expand :collapse))))
  (let ((args (list :people people :locations locations :truth truth
                    :from from :until until :mode mode))
        (text (org-chronicle--compose :people people :locations locations
                                      :truth truth :from from :until until :mode mode)))
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
PARAMS keys mirror `org-chronicle-timeline' (:people :locations :truth
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
                                            people location sources)
  "Return the Org heading text for a new event with the given fields.
PEOPLE is a list; the rest are strings (DATE-END/LOCATION/SOURCES optional)."
  (concat
   (format "* %s\n" title)
   ":PROPERTIES:\n"
   (format ":TRUTH:    %s\n" (or truth "historical"))
   (format ":DATE:     %s\n" (org-chronicle--ts date))
   (when (and date-end (not (string-blank-p date-end)))
     (format ":DATE-END: %s\n" (org-chronicle--ts date-end)))
   (when people (format ":PEOPLE:   %s\n" (org-chronicle--join people)))
   (when (and location (not (string-blank-p location)))
     (format ":LOCATION: %s\n" location))
   (when (and sources (not (string-blank-p sources)))
     (format ":SOURCES:  %s\n" sources))
   ":END:\n"))

(defun org-chronicle--known-people ()
  "Return a sorted, de-duplicated list of names seen in events and entities."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (e (ignore-errors (org-chronicle--all-events)))
      (dolist (p (plist-get e :people)) (puthash p t names)))
    (dolist (e (ignore-errors (org-chronicle--all-entities)))
      (puthash (plist-get e :name) t names)
      (dolist (a (plist-get e :aliases)) (puthash a t names)))
    (sort (hash-table-keys names) #'string<)))

(defun org-chronicle--read-people ()
  "Prompt for people with completion against known names; return a list."
  (completing-read-multiple "People (TAB to complete, blank to skip): "
                            (org-chronicle--known-people)))

;;;###autoload
(defun org-chronicle-add-event ()
  "Interactively capture a new event into `org-chronicle-timeline-file'."
  (interactive)
  (let* ((title (read-string "Event title: "))
         (date (read-string "Date (YYYY-MM-DD): "))
         (date-end (read-string "End date (blank for none): "))
         (truth (completing-read "Truth: " org-chronicle--truth-values nil t
                                 nil nil "historical"))
         (people (org-chronicle--read-people))
         (location (read-string "Location (blank to skip): "))
         (text (org-chronicle--event-string
                :title title :truth truth :date date :date-end date-end
                :people people :location location)))
    (with-current-buffer (find-file-noselect org-chronicle-timeline-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert text)
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-chronicle-normalize)
      (save-buffer))
    (message "Captured: %s" title)))

;;;###autoload
(defun org-chronicle-capture ()
  "Return a new event heading string for use in `org-capture' templates.
Prompts for the same fields as `org-chronicle-add-event'."
  (let ((title (read-string "Event title: "))
        (date (read-string "Date (YYYY-MM-DD): "))
        (truth (completing-read "Truth: " org-chronicle--truth-values nil t
                                nil nil "historical"))
        (people (org-chronicle--read-people))
        (location (read-string "Location (blank to skip): ")))
    (org-chronicle--event-string :title title :truth truth :date date
                                 :people people :location location)))

;;;###autoload
(defun org-chronicle-normalize ()
  "Validate and tidy the event heading at point.
Mirrors TRUTH to a tag, canonicalizes people/location names via aliases,
and warns if DATE does not parse."
  (interactive)
  (org-back-to-heading t)
  (let* ((entities (org-chronicle--all-entities))
         (idx (org-chronicle--alias-index entities))
         (truth (org-entry-get nil "TRUTH"))
         (date (org-entry-get nil "DATE")))
    (when (and date (null (org-chronicle--date-parse date)))
      (message "org-chronicle: DATE %S does not parse" date))
    (let ((people (org-chronicle--split (org-entry-get nil "PEOPLE"))))
      (when people
        (org-set-property
         "PEOPLE"
         (org-chronicle--join
          (mapcar (lambda (p) (org-chronicle--canonical p idx)) people)))))
    (let ((loc (org-entry-get nil "LOCATION")))
      (when loc
        (org-set-property "LOCATION" (org-chronicle--canonical loc idx))))
    (when (member truth org-chronicle--truth-values)
      (let ((tags (cl-remove-if (lambda (tg) (member tg org-chronicle--truth-values))
                                (org-get-tags nil t))))
        (org-set-tags (cons truth tags))))))

;;;; Entity creation

(defcustom org-chronicle-people-file "~/org/people.org"
  "File where new person and group entities are filed."
  :type 'file
  :group 'org-chronicle)

(defcustom org-chronicle-places-file "~/org/places.org"
  "File where new place entities are filed."
  :type 'file
  :group 'org-chronicle)

(cl-defun org-chronicle--entity-string (&key name kind aliases props)
  "Return the Org heading text for a new entity.
KIND is a symbol; ALIASES a list; PROPS an alist of (PROP . VALUE) extra
properties (only non-blank values are written)."
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

;;;###autoload
(defun org-chronicle-add-person (name)
  "Create a person entity NAME in `org-chronicle-people-file'.
Prompts for aliases, birth, and death."
  (interactive "sPerson name: ")
  (let ((aliases (completing-read-multiple "Aliases (blank to skip): " nil))
        (born (read-string "Born (YYYY-MM-DD, blank to skip): "))
        (died (read-string "Died (YYYY-MM-DD, blank to skip): ")))
    (org-chronicle--file-entity
     org-chronicle-people-file
     (org-chronicle--entity-string
      :name name :kind 'person :aliases aliases
      :props `(("BORN" . ,(and (not (string-blank-p born)) (org-chronicle--ts born)))
               ("DIED" . ,(and (not (string-blank-p died)) (org-chronicle--ts died))))))
    (message "Added person: %s" name)))

;;;###autoload
(defun org-chronicle-add-place (name)
  "Create a place entity NAME in `org-chronicle-places-file'.
Prompts for an optional build/raze span."
  (interactive "sPlace name: ")
  (let ((built (read-string "Built (blank to skip): "))
        (razed (read-string "Razed (blank to skip): ")))
    (org-chronicle--file-entity
     org-chronicle-places-file
     (org-chronicle--entity-string
      :name name :kind 'place
      :props `(("BUILT" . ,(and (not (string-blank-p built)) (org-chronicle--ts built)))
               ("RAZED" . ,(and (not (string-blank-p razed)) (org-chronicle--ts razed))))))
    (message "Added place: %s" name)))

;;;###autoload
(defun org-chronicle-add-group (name)
  "Create a group entity NAME in `org-chronicle-people-file'."
  (interactive "sGroup name: ")
  (org-chronicle--file-entity
   org-chronicle-people-file
   (org-chronicle--entity-string :name name :kind 'group))
  (message "Added group: %s" name))

;;;###autoload
(defun org-chronicle-promote ()
  "Promote the symbol/name at point (or a prompted name) into a person entity.
Seeds ALIASES with the literal text promoted."
  (interactive)
  (let* ((variant (or (thing-at-point 'symbol t)
                      (read-string "Promote name: ")))
         (canonical (read-string "Canonical name: " variant)))
    (org-chronicle--file-entity
     org-chronicle-people-file
     (org-chronicle--entity-string
      :name canonical :kind 'person
      :aliases (unless (equal canonical variant) (list variant))))
    (message "Promoted %S as entity %S" variant canonical)))























;;;; (sections added by later tasks)

(provide 'org-chronicle)


;;; org-chronicle.el ends here
