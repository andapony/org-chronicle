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

;;;; (sections added by later tasks)

(provide 'org-chronicle)
;;; org-chronicle.el ends here
