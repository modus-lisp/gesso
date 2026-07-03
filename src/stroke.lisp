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

(defun stroke-subpaths (cv subpaths color line-width alpha)
  "Stroke SUBPATHS (device-space polylines) onto scribe canvas CV."
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
    (when rects (fill-subpaths cv rects color alpha))))
