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

;;;; (sections added by later tasks)

(provide 'org-chronicle)
;;; org-chronicle.el ends here
