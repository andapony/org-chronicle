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
contributes TO - FROM <= WEIGHT.  Every node referenced by an edge must
appear in NODES; violating this contract causes a nil distance lookup and
a runtime error."
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

(defun org-chronicle--stn-conflict (network)
  "Return the edge LABELs of one negative cycle in NETWORK, or nil if none.
NETWORK is (:nodes NODES :edges EDGES) with edges (FROM TO WEIGHT LABEL).
Uses Bellman-Ford from `:zero', relaxing one extra round to find an edge on
a negative cycle, then walks predecessors to collect the cycle's labels."
  (let* ((nodes (plist-get network :nodes))
         (edges (plist-get network :edges))
         (d (make-hash-table :test #'equal))
         (pred (make-hash-table :test #'equal)))
    (dolist (n nodes) (puthash n 0 d))   ; 0 init finds any negative cycle
    (let ((culprit nil) (rounds (length nodes)))
      (dotimes (_ rounds)
        (dolist (e edges)
          (let* ((from (nth 0 e)) (to (nth 1 e)) (w (nth 2 e)))
            (when (< (+ (gethash from d) w) (gethash to d))
              (puthash to (+ (gethash from d) w) d)
              (puthash to e pred)))))
      ;; One more round: any edge that still relaxes lies on a negative cycle.
      (dolist (e edges)
        (let* ((from (nth 0 e)) (to (nth 1 e)) (w (nth 2 e)))
          (when (< (+ (gethash from d) w) (gethash to d))
            (setq culprit to))))
      (when culprit
        ;; Walk predecessors far enough to be inside the cycle, then collect.
        (dotimes (_ rounds)
          (setq culprit (nth 0 (gethash culprit pred))))
        (let ((labels '()) (node culprit) (start culprit) (first t))
          (while (or first (not (equal node start)))
            (setq first nil)
            (let ((e (gethash node pred)))
              (unless e (error "Broken predecessor chain at %s" node))
              (when e
                (push (nth 3 e) labels)
                (setq node (nth 0 e)))))
          (delete-dups labels))))))

(defun org-chronicle--scene-start-node (scene)
  "Return the STN start-node id for SCENE."
  (cons :start (plist-get scene :marker)))

(defun org-chronicle--scene-end-node (scene)
  "Return the STN end-node id for SCENE."
  (cons :end (plist-get scene :marker)))

(defun org-chronicle--stn-upper (node date label)
  "Return an edge bounding NODE on/before DATE (a date plist), tagged LABEL."
  (list :zero node (org-chronicle--date-ordinal
                    (org-chronicle--date-upper-bound date))
        label))

(defun org-chronicle--stn-lower (node date label)
  "Return an edge bounding NODE on/after DATE (a date plist), tagged LABEL."
  (list node :zero (- (org-chronicle--date-ordinal
                       (org-chronicle--date-lower-bound date)))
        label))

(defun org-chronicle--build-network (scenes ctx)
  "Return the constraint network for SCENES under context CTX.
Result is (:nodes NODES :edges EDGES :starts HASH :ends HASH); HASHes map a
scene's :marker to its start/end node ids.  Events and other scenes are
resolved as constants/variables; AFTER/BEFORE accept either."
  (let* ((by-event (plist-get ctx :events-by-id))
         (by-scene (make-hash-table :test #'equal))
         (starts (make-hash-table :test #'equal))
         (ends (make-hash-table :test #'equal))
         (nodes (list :zero))
         (edges '()))
    (dolist (s scenes)
      (when (plist-get s :id) (puthash (plist-get s :id) s by-scene)))
    (cl-labels
        ((lbl (desc marker) (list :desc desc :marker marker))
         (referent-end-date (id)
           ;; Date plist for "after this referent's end", or nil if undated.
           (let ((ev (gethash id by-event)))
             (if ev (or (plist-get ev :date-end) (plist-get ev :date))
               (let ((sc (gethash id by-scene)))
                 (and sc (or (plist-get sc :own-date-end)
                             (plist-get sc :own-date)))))))
         (referent-start-date (id)
           (let ((ev (gethash id by-event)))
             (if ev (plist-get ev :date)
               (let ((sc (gethash id by-scene)))
                 (and sc (plist-get sc :own-date))))))
         (referent-start-node (id)
           (let ((sc (gethash id by-scene)))
             (and sc (not (plist-get sc :own-date))
                  (org-chronicle--scene-start-node sc))))
         (referent-end-node (id)
           (let ((sc (gethash id by-scene)))
             (and sc (not (plist-get sc :own-date))
                  (org-chronicle--scene-end-node sc)))))
      (dolist (s scenes)
        (let ((sn (org-chronicle--scene-start-node s))
              (en (org-chronicle--scene-end-node s))
              (m (plist-get s :marker)))
          (puthash m sn starts)
          (puthash m en ends)
          (push sn nodes)
          (push en nodes)
          ;; internal start <= end: sn - en <= 0 => edge (en sn 0)
          (push (list en sn 0 (lbl "start before end" m)) edges)
          ;; own DATE fixes the scene (constant) as a closed [start,end].
          (when (plist-get s :own-date)
            (push (org-chronicle--stn-lower sn (plist-get s :own-date)
                                            (lbl "own date" m)) edges)
            (push (org-chronicle--stn-upper
                   en (or (plist-get s :own-date-end) (plist-get s :own-date))
                   (lbl "own date" m)) edges))
          ;; EARLIEST / LATEST authored bounds.
          (when (plist-get s :earliest)
            (push (org-chronicle--stn-lower sn (plist-get s :earliest)
                                            (lbl "EARLIEST" m)) edges))
          (when (plist-get s :latest)
            (push (org-chronicle--stn-upper en (plist-get s :latest)
                                            (lbl "LATEST" m)) edges))
          ;; AFTER: start >= referent end.
          ;; sn >= rn => rn - sn <= 0 => edge (sn rn 0).
          (dolist (id (plist-get s :after-ids))
            (let ((d (referent-end-date id)) (rn (referent-end-node id)))
              (cond (d (push (org-chronicle--stn-lower sn d (lbl "AFTER" m)) edges))
                    (rn (push (list sn rn 0 (lbl "AFTER scene" m)) edges)))))
          ;; BEFORE: end <= referent start.
          ;; en <= rn => en - rn <= 0 => edge (rn en 0).
          (dolist (id (plist-get s :before-ids))
            (let ((d (referent-start-date id)) (rn (referent-start-node id)))
              (cond (d (push (org-chronicle--stn-upper en d (lbl "BEFORE" m)) edges))
                    (rn (push (list rn en 0 (lbl "BEFORE scene" m)) edges)))))
          ;; Entity mentions: existence span as a box on [start,end].
          (dolist (ref (plist-get s :refs))
            (let ((ent (org-chronicle--entity-by-id
                        (plist-get ref :id) (plist-get ctx :entities))))
              (when ent
                (let ((span (org-chronicle--span-for-name
                             (plist-get ent :name) (plist-get ctx :entities)
                             (plist-get ctx :idx) (plist-get ctx :index))))
                  (when span
                    (when (car span)
                      (push (org-chronicle--stn-lower
                             sn (car span)
                             (lbl (format "%s extant" (plist-get ent :name))
                                  (plist-get ref :marker))) edges))
                    (when (cdr span)
                      (push (org-chronicle--stn-upper
                             en (cdr span)
                             (lbl (format "%s extant" (plist-get ent :name))
                                  (plist-get ref :marker))) edges))))))))))
    (list :nodes (delete-dups nodes) :edges edges :starts starts :ends ends)))






(provide 'org-chronicle-solve)

;;; org-chronicle-solve.el ends here
