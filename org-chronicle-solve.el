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

(defconst org-chronicle--stn-inf most-positive-fixnum
  "Sentinel distance standing for no path (an open bound).")

(defun org-chronicle--stn-distances (nodes edges)
  "Return an all-pairs shortest-path distance hash for NODES and EDGES.
Keys are (FROM . TO) conses; values are integer distances or
`org-chronicle--stn-inf' when unreachable.  Each edge (FROM TO WEIGHT _)
contributes TO - FROM <= WEIGHT."
  (let ((dist (make-hash-table :test #'equal)))
    (dolist (a nodes)
      (puthash (cons a a) 0 dist)
      (dolist (b nodes)
        (unless (equal a b)
          (puthash (cons a b) org-chronicle--stn-inf dist))))
    (dolist (e edges)
      (let* ((from (nth 0 e)) (to (nth 1 e)) (w (nth 2 e))
             (cur (gethash (cons from to) dist)))
        ;; edge means to - from <= w, i.e. a from->to arc of weight w.
        (when (< w cur) (puthash (cons from to) w dist))))
    (dolist (k nodes dist)
      (dolist (i nodes)
        (let ((dik (gethash (cons i k) dist)))
          (unless (= dik org-chronicle--stn-inf)
            (dolist (j nodes)
              (let ((dkj (gethash (cons k j) dist)))
                (unless (= dkj org-chronicle--stn-inf)
                  (let ((through (+ dik dkj)))
                    (when (< through (gethash (cons i j) dist))
                      (puthash (cons i j) through dist))))))))))))

(defun org-chronicle--stn-solve (network)
  "Solve NETWORK, returning (:consistent BOOL :lo HASH :hi HASH).
NETWORK is (:nodes NODES :edges EDGES).  :lo/:hi map each non-`:zero' node
to its tightest lower/upper day-ordinal bound, or nil when open.  When the
network is inconsistent (any negative self-distance) :lo and :hi are nil."
  (let* ((nodes (plist-get network :nodes))
         (edges (plist-get network :edges))
         (dist (org-chronicle--stn-distances nodes edges))
         (consistent (cl-every (lambda (n) (>= (gethash (cons n n) dist) 0))
                               nodes)))
    (if (not consistent)
        (list :consistent nil :lo nil :hi nil)
      (let ((lo (make-hash-table :test #'equal))
            (hi (make-hash-table :test #'equal)))
        (dolist (n nodes)
          (unless (eq n :zero)
            ;; upper bound = dist(zero -> n); lower bound = -dist(n -> zero).
            (let ((up (gethash (cons :zero n) dist))
                  (down (gethash (cons n :zero) dist)))
              (puthash n (if (= up org-chronicle--stn-inf) nil up) hi)
              (puthash n (if (= down org-chronicle--stn-inf) nil (- down)) lo))))
        (list :consistent t :lo lo :hi hi)))))




(provide 'org-chronicle-solve)

;;; org-chronicle-solve.el ends here
