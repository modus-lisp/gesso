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

;;; ---- patterns -------------------------------------------------------------
;;;
;;; A PATTERN is a tile of pixels repeated over user space: SVG's <pattern>, and the
;;; paint source every tiling feature ends up wanting (feTile, feImage, CSS
;;; background-repeat).  Like a gradient it is evaluated per device pixel through the
;;; inverse CTM, so it needs no cooperation from the rasterizer beyond being asked.
;;;
;;; DEFSTRUCT here and not DEFCLASS, against the house default, for the same reason
;;; GRADIENT above is one: this is read once per covered pixel — millions of times in a
;;; full-page fill — and it is a value type with no identity to preserve across a
;;; redefinition.  That is the measured-hot exception, not a lapse.

(defstruct pattern
  ;; A scribe RGBA canvas: PREMULTIPLIED rgb + a separate alpha plane.
  (tile nil)
  ;; The tile's rectangle in USER space.  Sampling is (u - x) mod w, so x/y matter:
  ;; a pattern is anchored, not merely periodic.
  (x 0d0) (y 0d0) (w 1d0) (h 1d0)
  ;; Inverse of patternTransform, applied to the user point before tiling.  NIL for
  ;; the common case, so the transform costs nothing when it is not used.
  (inv nil))

(defun pattern-color-at (pat ux uy)
  "The (values r g b a) of PAT at user point (UX,UY), a in [0,1] and rgb STRAIGHT —
   the tile stores premultiplied colour, and the rasterizer wants it unmultiplied
   because it folds alpha into coverage itself."
  (let ((tile (pattern-tile pat)))
    (when (null tile) (return-from pattern-color-at (values 0 0 0 0d0)))
    (when (pattern-inv pat)
      (multiple-value-setq (ux uy) (mat-apply (pattern-inv pat) ux uy)))
    (let* ((tw (pattern-w pat)) (th (pattern-h pat)))
      (when (or (<= tw 0d0) (<= th 0d0))
        (return-from pattern-color-at (values 0 0 0 0d0)))
      (let* ((iw (scribe:canvas-width tile)) (ih (scribe:canvas-height tile))
             ;; MOD, not REM: a user point left of or above the tile origin has a
             ;; negative offset, and REM would mirror the tile there instead of
             ;; repeating it — visible as a seam through the origin and nowhere else.
             (fx (/ (mod (- ux (pattern-x pat)) tw) tw))
             (fy (/ (mod (- uy (pattern-y pat)) th) th))
             ;; BILINEAR, and wrapping at the tile edge.  Nearest-neighbour left every
             ;; internal edge of the tile aliased against Chromium's filtered sampling —
             ;; a band of tests all landing 4-8% wrong, which is the shape of "right
             ;; geometry, wrong sampling".  Wrapping (not clamping) the far tap is what
             ;; makes the seam between two tiles look like the inside of one.
             (sx (- (* fx iw) 0.5d0)) (sy (- (* fy ih) 0.5d0))
             (x0 (floor sx)) (y0 (floor sy))
             (tx (- sx x0)) (ty (- sy y0))
             (pix (scribe:canvas-pixels tile))
             (ap (scribe:canvas-alpha tile)))
        (macrolet ((tap (xi yi)
                     `(let ((j (+ (mod ,xi iw) (* (mod ,yi ih) iw))))
                        (values (aref pix (* 3 j)) (aref pix (+ (* 3 j) 1))
                                (aref pix (+ (* 3 j) 2))
                                (if ap (aref ap j) 255)))))
          (flet ((mix4 (i)
                   ;; Interpolate in PREMULTIPLIED space, which is the only place it is
                   ;; correct: blending straight colour across a transparent neighbour
                   ;; drags that neighbour's meaningless RGB into the result.
                   (multiple-value-bind (r0 g0 b0 a0) (tap x0 y0)
                     (multiple-value-bind (r1 g1 b1 a1) (tap (1+ x0) y0)
                       (multiple-value-bind (r2 g2 b2 a2) (tap x0 (1+ y0))
                         (multiple-value-bind (r3 g3 b3 a3) (tap (1+ x0) (1+ y0))
                           (let ((v (list (list r0 g0 b0 a0) (list r1 g1 b1 a1)
                                          (list r2 g2 b2 a2) (list r3 g3 b3 a3))))
                             (+ (* (nth i (first v))  (- 1d0 tx) (- 1d0 ty))
                                (* (nth i (second v)) tx        (- 1d0 ty))
                                (* (nth i (third v))  (- 1d0 tx) ty)
                                (* (nth i (fourth v)) tx        ty)))))))))
            (let ((a (mix4 3)))
              (if (< a 0.5d0)
                  (values 0 0 0 0d0)
                  (values (min 255 (round (* (mix4 0) 255) a))
                          (min 255 (round (* (mix4 1) 255) a))
                          (min 255 (round (* (mix4 2) 255) a))
                          (min 1d0 (/ a 255d0)))))))))))

;;; ---- the paint protocol ---------------------------------------------------

(defun paint-source-p (paint)
  "True for a paint evaluated PER PIXEL (gradient or pattern) rather than a flat
   colour.  The rasterizer and the inverse-CTM capture both branch on this, and
   adding a fourth source should mean adding it here and in PAINT-COLOR-AT only."
  (or (gradient-p paint) (pattern-p paint)))

(defun paint-color-at (paint ux uy)
  "The (values r g b a) of a per-pixel PAINT at user point (UX,UY)."
  (if (pattern-p paint)
      (pattern-color-at paint ux uy)
      (gradient-color-at paint ux uy)))

(defun paint->solid (paint)
  "A solid (r g b) for PAINT: the paint itself when solid, else a flat approximation
   (used for gradient- and pattern-filled text) — a gradient's first stop, a
   pattern's middle pixel."
  (cond ((gradient-p paint)
         (let ((s (gradient-stops paint)))
           (if s (list (second (car s)) (third (car s)) (fourth (car s))) '(0 0 0))))
        ((pattern-p paint)
         (let ((tile (pattern-tile paint)))
           (if (null tile) '(0 0 0)
               (multiple-value-bind (r g b a)
                   (pattern-color-at paint
                                     (+ (pattern-x paint) (/ (pattern-w paint) 2))
                                     (+ (pattern-y paint) (/ (pattern-h paint) 2)))
                 (declare (ignore a))
                 (list r g b)))))
        (t paint)))

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
