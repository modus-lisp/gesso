;;;; paint.lisp — gradient paint sources for fill and stroke.
;;;;
;;;; A paint is either a solid colour (an (r g b) list, the default) or a
;;;; GRADIENT.  A gradient is evaluated per device pixel: the pixel is mapped back
;;;; through the inverse of the transform in force when the gradient was painted,
;;;; then the linear projection / radial two-circle parameter picks a colour from
;;;; the sorted colour stops.  Stops carry straight alpha, folded into coverage.
(in-package #:gesso)

(defstruct gradient
  (kind :linear)                                   ; :linear or :radial
  (x0 0d0) (y0 0d0) (r0 0d0) (x1 0d0) (y1 0d0) (r1 0d0)
  (stops nil))                                     ; sorted list of (offset r g b a), a in [0,1]

(defun make-linear-gradient (x0 y0 x1 y1)
  (make-gradient :kind :linear :x0 (df x0) :y0 (df y0) :x1 (df x1) :y1 (df y1)))

(defun make-radial-gradient (x0 y0 r0 x1 y1 r1)
  (make-gradient :kind :radial :x0 (df x0) :y0 (df y0) :r0 (df r0)
                 :x1 (df x1) :y1 (df y1) :r1 (df r1)))

(defun add-color-stop (grad offset color &optional (alpha 1d0))
  "Add a colour stop at OFFSET (0..1) with COLOR (r g b) and ALPHA, keeping the
   stop list sorted by offset."
  (let ((stop (list (df offset) (first color) (second color) (third color) (df alpha))))
    ;; Append (not prepend) before the stable sort so that stops sharing an offset —
    ;; a hard colour stop, e.g. `red 50%, blue 50%` — keep their insertion order.
    ;; Prepending would let a later equal-offset stop sort ahead of an earlier one,
    ;; inverting the hard transition.
    (setf (gradient-stops grad)
          (stable-sort (append (gradient-stops grad) (list stop)) #'< :key #'first)))
  grad)

(defun paint->solid (paint)
  "A solid (r g b) for PAINT: the paint itself when solid, else its first stop's
   colour (a flat approximation, used for gradient-filled text)."
  (if (gradient-p paint)
      (let ((s (gradient-stops paint)))
        (if s (list (second (car s)) (third (car s)) (fourth (car s))) '(0 0 0)))
      paint))

(defun %lerp (a b tt) (+ a (* (- b a) tt)))

(defun gradient-color-at (grad ux uy)
  "The (values r g b a) of GRAD at user point (UX,UY); a in [0,1].  Colours are
   clamped (pad) outside [0,1]."
  (let ((stops (gradient-stops grad)))
    (when (null stops) (return-from gradient-color-at (values 0 0 0 0d0)))
    (let ((tt (ecase (gradient-kind grad)
                (:linear (%linear-t grad ux uy))
                (:radial (%radial-t grad ux uy)))))
      (if (null tt)
          (values 0 0 0 0d0)                       ; radial: point outside the cone
          (%sample-stops stops (max 0d0 (min 1d0 tt)))))))

(defun %linear-t (grad ux uy)
  (let* ((dx (- (gradient-x1 grad) (gradient-x0 grad)))
         (dy (- (gradient-y1 grad) (gradient-y0 grad)))
         (len2 (+ (* dx dx) (* dy dy))))
    (if (< len2 1d-12) 0d0
        (/ (+ (* (- ux (gradient-x0 grad)) dx) (* (- uy (gradient-y0 grad)) dy)) len2))))

(defun %radial-t (grad ux uy)
  "The gradient parameter for a two-circle radial gradient, or NIL when the point
   is not reachable.  Solves |P - C(t)| = R(t) for the largest t with R(t) >= 0."
  (let* ((cdx (- (gradient-x1 grad) (gradient-x0 grad)))
         (cdy (- (gradient-y1 grad) (gradient-y0 grad)))
         (dr  (- (gradient-r1 grad) (gradient-r0 grad)))
         (pdx (- ux (gradient-x0 grad))) (pdy (- uy (gradient-y0 grad)))
         (a (- (+ (* cdx cdx) (* cdy cdy)) (* dr dr)))
         (b (+ (* pdx cdx) (* pdy cdy) (* (gradient-r0 grad) dr)))
         (c (- (+ (* pdx pdx) (* pdy pdy)) (* (gradient-r0 grad) (gradient-r0 grad)))))
    (flet ((ok (tt) (and tt (>= (+ (gradient-r0 grad) (* tt dr)) 0d0) tt)))
      (if (< (abs a) 1d-9)
          (and (> (abs b) 1d-12) (ok (/ c (* 2d0 b))))
          (let ((disc (- (* b b) (* a c))))
            (when (>= disc 0d0)
              (let ((sq (sqrt disc)))
                (or (ok (/ (+ b sq) a)) (ok (/ (- b sq) a))))))))))

(defun %sample-stops (stops tt)
  "Interpolate the sorted STOPS at parameter TT in [0,1].  Returns (values r g b a)."
  (let ((first (first stops)))
    (when (<= tt (first first))
      (return-from %sample-stops (values (second first) (third first) (fourth first) (fifth first))))
    (loop for (lo hi) on stops while hi do
      (when (<= (first lo) tt (first hi))
        (let* ((span (- (first hi) (first lo)))
               (f (if (< span 1d-9) 0d0 (/ (- tt (first lo)) span))))
          (return-from %sample-stops
            (values (round (%lerp (second lo) (second hi) f))
                    (round (%lerp (third lo) (third hi) f))
                    (round (%lerp (fourth lo) (fourth hi) f))
                    (%lerp (fifth lo) (fifth hi) f))))))
    (let ((last (car (last stops))))
      (values (second last) (third last) (fourth last) (fifth last)))))
