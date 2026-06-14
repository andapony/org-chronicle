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










;;;; (sections added by later tasks)

(provide 'org-chronicle)
;;; org-chronicle.el ends here
