;;;; stroke.lisp — turn a polyline into a fillable stroke outline.
;;;;
;;;; Each segment becomes a rectangle offset by half the line width along the
;;;; segment normal; a small square stamped at every interior vertex fills the
;;;; join gaps.  All rectangles are wound the same way, so the non-zero fill
;;;; unions them (overlaps clamp to full coverage rather than cancelling).  This
;;;; is a butt-cap / approximate-round-join stroker — good for lines, borders and
;;;; SVG paths; precise miter/round/bevel joins and caps are a later refinement.
(in-package #:gesso)

(defun %quad-subpath (ax ay bx by cx cy dx dy)
  (let ((sp (make-subpath)) (v (make-array 4 :adjustable t :fill-pointer 0)))
    (vector-push-extend (cons (df ax) (df ay)) v)
    (vector-push-extend (cons (df bx) (df by)) v)
    (vector-push-extend (cons (df cx) (df cy)) v)
    (vector-push-extend (cons (df dx) (df dy)) v)
    (setf (subpath-points sp) v (subpath-closed sp) t)
    sp))

(defun %square-subpath (cx cy hw)
  ;; wound the same way as a segment rectangle so the non-zero fill unions
  ;; (rather than cancels) where a join square overlaps its segments
  (%quad-subpath (- cx hw) (- cy hw) (- cx hw) (+ cy hw)
                 (+ cx hw) (+ cy hw) (+ cx hw) (- cy hw)))

(defun %normalize-dashes (dashes)
  "A vector of dash lengths with a positive sum, doubling an odd-length pattern
   (SVG stroke-dasharray semantics), or NIL when there is no visible dashing."
  (when dashes
    (let ((d (remove-if (lambda (x) (< x 0d0)) (mapcar #'df (coerce dashes 'list)))))
      (when (and d (some #'plusp d))
        (coerce (if (oddp (length d)) (append d d) d) 'vector)))))

(defun %dash-subpath (sp dashes offset)
  "Split subpath SP into the 'on' dash runs (fresh open subpaths) per DASHES (a
   normalized device-space pattern) and OFFSET (distance into the pattern at the
   path start)."
  (let* ((raw (subpath-points sp)) (n (length raw)))
    (when (< n 2) (return-from %dash-subpath nil))
    (let* ((pts (if (subpath-closed sp)
                    (let ((v (make-array (1+ n))))
                      (dotimes (i n) (setf (aref v i) (aref raw i)))
                      (setf (aref v n) (aref raw 0)) v)
                    raw))
           (np (length dashes))
           (total (loop for x across dashes sum x)))
      (when (<= total 0d0) (return-from %dash-subpath (list sp)))
      (let ((di 0) (on t) (rem 0d0) (result '()) (cur '()))
        ;; consume the dash offset to find the starting phase
        (let ((off (mod (df offset) total)))
          (loop while (> off 0d0) do
            (let ((seg (aref dashes di)))
              (if (< off seg) (setf rem (- seg off) off 0d0)
                  (progn (decf off seg) (setf di (mod (1+ di) np) on (not on)))))))
        (when (<= rem 0d0) (setf rem (aref dashes di)))
        (flet ((start (p) (setf cur (list p)))
               (add (p) (push p cur))
               (finish () (when (cdr cur)
                            (let ((s (make-subpath)))
                              (dolist (p (nreverse cur))
                                (vector-push-extend p (subpath-points s)))
                              (push s result)))
                        (setf cur '())))
          (when on (start (aref pts 0)))
          (loop for i from 1 below (length pts) do
            (let* ((p0 (aref pts (1- i))) (p1 (aref pts i))
                   (dx (- (car p1) (car p0))) (dy (- (cdr p1) (cdr p0)))
                   (seglen (sqrt (+ (* dx dx) (* dy dy)))))
              (when (> seglen 1d-9)
                (let ((pos 0d0) (ux (/ dx seglen)) (uy (/ dy seglen)))
                  (loop
                    (if (<= (- seglen pos) rem)
                        (progn (decf rem (- seglen pos))
                               (when on (add p1))
                               (return))
                        (progn (incf pos rem)
                               (let ((pt (cons (+ (car p0) (* ux pos)) (+ (cdr p0) (* uy pos)))))
                                 (if on (progn (add pt) (finish)) (start pt)))
                               (setf di (mod (1+ di) np) on (not on) rem (aref dashes di)))))))))
          (finish)
          (nreverse result))))))

(defun %dash-all (subpaths dashes offset)
  (let ((out '()))
    (dolist (sp subpaths) (setf out (nconc out (%dash-subpath sp dashes offset))))
    out))

(defun stroke-subpaths (cv subpaths paint line-width alpha &optional inv dashes dash-offset)
  "Stroke SUBPATHS (device-space polylines) onto scribe canvas CV with PAINT (a
   solid colour or a GRADIENT evaluated through the inverse CTM INV).  DASHES, when
   a normalizable pattern, dashes the polylines first (stroke-dasharray)."
  (let* ((norm (%normalize-dashes dashes))
         (subpaths (if norm (%dash-all subpaths norm (or dash-offset 0d0)) subpaths)))
  (let ((hw (max 0.35d0 (* 0.5d0 (df line-width)))) (rects '()))
    (dolist (sp subpaths)
      (let* ((pts (subpath-points sp)) (n (length pts)))
        (when (>= n 2)
          (let ((limit (if (subpath-closed sp) n (1- n))))
            (dotimes (i limit)
              (let* ((p0 (aref pts i)) (p1 (aref pts (mod (1+ i) n)))
                     (dx (- (car p1) (car p0))) (dy (- (cdr p1) (cdr p0)))
                     (len (sqrt (+ (* dx dx) (* dy dy)))))
                (when (> len 1d-9)
                  (let ((nx (* (/ (- dy) len) hw)) (ny (* (/ dx len) hw)))
                    (push (%quad-subpath (+ (car p0) nx) (+ (cdr p0) ny)
                                         (+ (car p1) nx) (+ (cdr p1) ny)
                                         (- (car p1) nx) (- (cdr p1) ny)
                                         (- (car p0) nx) (- (cdr p0) ny))
                          rects))))))
          ;; join/cap fill: a square at each shared vertex (skip line endpoints)
          (when (> hw 0.9d0)
            (loop for i from (if (subpath-closed sp) 0 1) below (if (subpath-closed sp) n (1- n))
                  for p = (aref pts i)
                  do (push (%square-subpath (car p) (cdr p) hw) rects))))))
    (when rects (fill-subpaths cv rects paint alpha inv)))))
