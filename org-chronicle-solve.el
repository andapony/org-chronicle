;;; org-chronicle-solve.el --- Continuity constraint solver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A simple temporal network (STN) over scenes.  Scenes become [start,end]
;; time points; AFTER/BEFORE/EARLIEST/LATEST/entity-span/name-onset become
;; difference constraints, solved by all-pairs shortest path.  The engine is
;; pure (plists in, plists out) so it is tested without Org or buffers.

;;; Code:

(require 'cl-lib)
(require 'org-chronicle)

(provide 'org-chronicle-solve)

;;; org-chronicle-solve.el ends here
