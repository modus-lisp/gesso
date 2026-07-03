;;;; path.lisp — path storage and curve/arc flattening.
;;;;
;;;; A path is a list of SUBPATHs.  Curves and arcs are flattened to line
;;;; segments as they are recorded, and every point is mapped through the
;;;; current transform at record time (the canvas rule: geometry is captured in
;;;; device space when it is added, not when it is painted).  A subpath is thus a
;;;; polyline of device-space points that the rasterizer can consume directly.
(in-package #:gesso)

(defstruct (subpath (:constructor %make-subpath))
  (points (make-array 0 :adjustable t :fill-pointer 0))   ; vector of (x . y) double conses
  (closed nil))

(defun make-subpath () (%make-subpath))

(defun subpath-empty-p (sp) (zerop (length (subpath-points sp))))

(defun subpath-last (sp)
  (let ((p (subpath-points sp)))
    (when (plusp (length p)) (aref p (1- (length p))))))

(defun %emit (sp m x y)
  "Map (X,Y) through M and append the device point to subpath SP."
  (multiple-value-bind (dx dy) (mat-apply m x y)
    (vector-push-extend (cons dx dy) (subpath-points sp))))

(defun flatten-quadratic (sp m x0 y0 cx cy x1 y1 &optional (n 16))
  "Flatten a quadratic Bézier (P0=control-start, control CX/CY, end X1/Y1) through
   M into SP.  X0/Y0 are user-space (the current point)."
  (let ((x0 (df x0)) (y0 (df y0)) (cx (df cx)) (cy (df cy)) (x1 (df x1)) (y1 (df y1)))
    (loop for i from 1 to n
          for tt = (/ (df i) n)
          for u = (- 1d0 tt) do
            (%emit sp m
                   (+ (* u u x0) (* 2d0 u tt cx) (* tt tt x1))
                   (+ (* u u y0) (* 2d0 u tt cy) (* tt tt y1))))))

(defun flatten-cubic (sp m x0 y0 x1 y1 x2 y2 x3 y3 &optional (n 24))
  "Flatten a cubic Bézier through M into SP.  X0/Y0 is the current point."
  (let ((x0 (df x0)) (y0 (df y0)) (x1 (df x1)) (y1 (df y1))
        (x2 (df x2)) (y2 (df y2)) (x3 (df x3)) (y3 (df y3)))
    (loop for i from 1 to n
          for tt = (/ (df i) n)
          for u = (- 1d0 tt)
          for uu = (* u u) for tt2 = (* tt tt) do
            (%emit sp m
                   (+ (* uu u x0) (* 3d0 uu tt x1) (* 3d0 u tt2 x2) (* tt tt2 x3))
                   (+ (* uu u y0) (* 3d0 uu tt y1) (* 3d0 u tt2 y2) (* tt tt2 y3))))))

(defun flatten-arc (sp m cx cy r a0 a1 ccw)
  "Flatten a circular arc (centre CX/CY, radius R, from angle A0 to A1) through M
   into SP.  CCW selects direction, matching canvas arc()."
  (let* ((cx (df cx)) (cy (df cy)) (r (df r)) (a0 (df a0)) (a1 (df a1)))
    ;; normalize the sweep into a single directed range
    (cond
      ((and (not ccw) (< a1 a0)) (setf a1 (+ a1 (* 2d0 pi (1+ (floor (/ (- a0 a1) (* 2d0 pi))))))))
      ((and ccw (> a1 a0))       (setf a1 (- a1 (* 2d0 pi (1+ (floor (/ (- a1 a0) (* 2d0 pi)))))))))
    (let* ((sweep (- a1 a0))
           (steps (max 2 (ceiling (/ (abs sweep) (/ pi 16d0))))))
      (loop for i from 0 to steps
            for a = (+ a0 (* sweep (/ (df i) steps))) do
              (%emit sp m (+ cx (* r (cos a))) (+ cy (* r (sin a))))))))
