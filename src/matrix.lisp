;;;; matrix.lisp — 2D affine transforms.
;;;;
;;;; A transform is the 6-tuple (a b c d e f), the canvas convention:
;;;;   x' = a*x + c*y + e
;;;;   y' = b*x + d*y + f
;;;; i.e. the 3x3 matrix [[a c e] [b d f] [0 0 1]].  All values double-float.
(in-package #:gesso)

(declaim (inline df))
(defun df (x) (float x 1d0))

(defun mat-identity () (list 1d0 0d0 0d0 1d0 0d0 0d0))

(defun mat-mul (outer inner)
  "Compose OUTER and INNER so the result maps a point through INNER, then OUTER
   (result(p) = outer(inner(p))).  This is how a canvas concatenates a local
   transform onto the current one."
  (destructuring-bind (a1 b1 c1 d1 e1 f1) outer
    (destructuring-bind (a2 b2 c2 d2 e2 f2) inner
      (list (+ (* a1 a2) (* c1 b2))
            (+ (* b1 a2) (* d1 b2))
            (+ (* a1 c2) (* c1 d2))
            (+ (* b1 c2) (* d1 d2))
            (+ (* a1 e2) (* c1 f2) e1)
            (+ (* b1 e2) (* d1 f2) f1)))))

(defun mat-apply (m x y)
  "Map (X,Y) through M.  Returns (values x' y') as double-floats."
  (destructuring-bind (a b c d e f) m
    (let ((x (df x)) (y (df y)))
      (values (+ (* a x) (* c y) e) (+ (* b x) (* d y) f)))))

(defun mat-translate (tx ty) (list 1d0 0d0 0d0 1d0 (df tx) (df ty)))
(defun mat-scale (sx sy)     (list (df sx) 0d0 0d0 (df sy) 0d0 0d0))
(defun mat-rotate (radians)
  (let ((c (cos (df radians))) (s (sin (df radians))))
    (list c s (- s) c 0d0 0d0)))

(defun mat-invert (m)
  "The inverse of affine M, or NIL if singular.  Maps device points back to the
   user space M was built in (used to evaluate a paint at a device pixel)."
  (destructuring-bind (a b c d e f) m
    (let ((det (- (* a d) (* b c))))
      (when (> (abs det) 1d-12)
        (let ((ia (/ d det)) (ib (/ (- b) det)) (ic (/ (- c) det)) (id (/ a det)))
          (list ia ib ic id
                (- (+ (* ia e) (* ic f)))
                (- (+ (* ib e) (* id f)))))))))

(defun mat-mean-scale (m)
  "A scalar approximating M's linear scale — the geometric mean of the two axis
   lengths.  Used to pick a rasterization ppem for text under a scaled transform."
  (destructuring-bind (a b c d e f) m
    (declare (ignore e f))
    (let ((sx (sqrt (+ (* a a) (* b b)))) (sy (sqrt (+ (* c c) (* d d)))))
      (sqrt (max 1d-6 (* sx sy))))))
