;;;; context.lisp — the stateful 2D drawing context.
;;;;
;;;; Holds a scribe canvas, a stack of drawing state (transform, colours, line
;;;; width, alpha, font) and the current path.  The API mirrors the HTML canvas
;;;; 2D context so a host (weft) can bind CanvasRenderingContext2D onto it
;;;; op-for-op; colours are (r g b) 8-bit lists.
(in-package #:gesso)

(defstruct gstate
  (transform (mat-identity))
  (fill-color '(0 0 0))
  (stroke-color '(0 0 0))
  (line-width 1d0)
  (global-alpha 1d0)
  (font-size 10d0)
  (font-weight 400)
  (font-style :normal)
  (font-family nil)
  (fill-rule :nonzero)
  (line-dash nil)        ; stroke dash pattern (list of lengths) or NIL
  (line-dash-offset 0d0)
  (clip nil)             ; device-space coverage mask (intersection of clips), or NIL
  (blend-mode :normal)   ; separable blend mode (§11.3.5); :normal = source-over
  (soft-mask nil))       ; device-space alpha mask (§11.6.5.2), or NIL

(defun copy-state (s)
  (make-gstate :transform (copy-list (gstate-transform s))
               :fill-color (gstate-fill-color s) :stroke-color (gstate-stroke-color s)
               :line-width (gstate-line-width s) :global-alpha (gstate-global-alpha s)
               :font-size (gstate-font-size s) :font-weight (gstate-font-weight s)
               :font-style (gstate-font-style s) :font-family (gstate-font-family s)
               :fill-rule (gstate-fill-rule s) :clip (gstate-clip s)
               :line-dash (gstate-line-dash s) :line-dash-offset (gstate-line-dash-offset s)
               :blend-mode (gstate-blend-mode s) :soft-mask (gstate-soft-mask s)))

(defstruct (context (:constructor %make-context))
  canvas
  (state (make-gstate))
  (stack nil)
  (subpaths nil)          ; list of subpaths (fill/stroke targets)
  (open nil)              ; the subpath currently being extended
  (cur-user nil)          ; current point in USER space (for curve starts)
  (clear-color '(255 255 255)))

(defun make-context (width height &key (background '(255 255 255)) canvas alpha)
  "A 2D context over a new scribe canvas WIDTH x HEIGHT (filled BACKGROUND), or
   over an existing scribe CANVAS when supplied (the weft integration path).  When
   ALPHA is true (and no CANVAS is given) the surface is a transparent RGBA canvas,
   so clearRect and semi-transparent fills composite correctly onto the page."
  (%make-context :canvas (or canvas (if alpha (scribe:make-rgba-canvas width height)
                                        (scribe:make-canvas width height background)))
                 :clear-color background))

(defun %paint-inv (ctx paint)
  "The inverse CTM for evaluating a gradient PAINT, or NIL for a solid colour."
  (when (gradient-p paint) (mat-invert (ctm ctx))))

(defun context-width (ctx) (scribe:canvas-width (context-canvas ctx)))
(defun context-height (ctx) (scribe:canvas-height (context-canvas ctx)))
(defun ctm (ctx) (gstate-transform (context-state ctx)))
(defun write-png (ctx path) (scribe:write-png (context-canvas ctx) path))

;;; ---- state stack ----------------------------------------------------------
(defun save (ctx)
  (push (copy-state (context-state ctx)) (context-stack ctx)))
(defun restore (ctx)
  (when (context-stack ctx) (setf (context-state ctx) (pop (context-stack ctx)))))

(defun set-fill (ctx color) (setf (gstate-fill-color (context-state ctx)) color))
(defun current-fill (ctx)
  "The current fill colour as a solid (r g b) list (a stencil mask paints in it)."
  (paint->solid (gstate-fill-color (context-state ctx))))
(defun set-stroke (ctx color) (setf (gstate-stroke-color (context-state ctx)) color))
(defun set-line-width (ctx w) (setf (gstate-line-width (context-state ctx)) (df w)))
(defun set-global-alpha (ctx a) (setf (gstate-global-alpha (context-state ctx)) (max 0d0 (min 1d0 (df a)))))
(defun set-blend-mode (ctx mode)
  "Set the current separable blend mode (a keyword; :NORMAL = source-over)."
  (setf (gstate-blend-mode (context-state ctx)) (or mode :normal)))
(defun set-soft-mask (ctx mask)
  "Set (or clear, with NIL) the active soft mask: a device-space alpha array
   (canvas-width*canvas-height doubles in 0..1) multiplied into every paint."
  (setf (gstate-soft-mask (context-state ctx)) mask))
(defun set-font (ctx size &key family (weight 400) (style :normal))
  (let ((s (context-state ctx)))
    (setf (gstate-font-size s) (df size) (gstate-font-family s) family
          (gstate-font-weight s) weight (gstate-font-style s) style)))
(defun set-fill-rule (ctx rule)
  "RULE is :nonzero or :evenodd (accepts the strings \"nonzero\"/\"evenodd\")."
  (setf (gstate-fill-rule (context-state ctx))
        (if (or (eq rule :evenodd) (and (stringp rule) (string-equal rule "evenodd")))
            :evenodd :nonzero)))
(defun set-line-dash (ctx segments &optional (offset 0d0))
  "SEGMENTS is a list of dash lengths (empty/NIL = solid); OFFSET is the phase."
  (let ((s (context-state ctx)))
    (setf (gstate-line-dash s) (and segments (mapcar #'df segments))
          (gstate-line-dash-offset s) (df offset))))

;;; ---- transforms -----------------------------------------------------------
(defun %concat (ctx local)
  (setf (gstate-transform (context-state ctx)) (mat-mul (ctm ctx) local)))
(defun translate (ctx tx ty) (%concat ctx (mat-translate tx ty)))
(defun scale (ctx sx sy) (%concat ctx (mat-scale sx sy)))
(defun rotate (ctx radians) (%concat ctx (mat-rotate radians)))
(defun transform (ctx a b c d e f) (%concat ctx (list (df a) (df b) (df c) (df d) (df e) (df f))))
(defun set-transform (ctx a b c d e f)
  (setf (gstate-transform (context-state ctx)) (list (df a) (df b) (df c) (df d) (df e) (df f))))
(defun reset-transform (ctx) (setf (gstate-transform (context-state ctx)) (mat-identity)))

;;; ---- path building --------------------------------------------------------
(defun begin-path (ctx)
  (setf (context-subpaths ctx) nil (context-open ctx) nil (context-cur-user ctx) nil))

(defun %new-subpath (ctx)
  (let ((sp (make-subpath)))
    (push sp (context-subpaths ctx))
    (setf (context-open ctx) sp)
    sp))

(defun move-to (ctx x y)
  (let ((sp (%new-subpath ctx)))
    (%emit sp (ctm ctx) x y)
    (setf (context-cur-user ctx) (cons (df x) (df y)))))

(defun line-to (ctx x y)
  ;; with no open subpath a lone line-to starts one (its first point is the move)
  (let ((sp (or (context-open ctx) (%new-subpath ctx))))
    (%emit sp (ctm ctx) x y)
    (setf (context-cur-user ctx) (cons (df x) (df y)))))

(defun quadratic-curve-to (ctx cx cy x y)
  (let ((sp (or (context-open ctx) (%new-subpath ctx)))
        (p (or (context-cur-user ctx) (cons (df cx) (df cy)))))
    (flatten-quadratic sp (ctm ctx) (car p) (cdr p) cx cy x y)
    (setf (context-cur-user ctx) (cons (df x) (df y)))))

(defun bezier-curve-to (ctx c1x c1y c2x c2y x y)
  (let ((sp (or (context-open ctx) (%new-subpath ctx)))
        (p (or (context-cur-user ctx) (cons (df c1x) (df c1y)))))
    (flatten-cubic sp (ctm ctx) (car p) (cdr p) c1x c1y c2x c2y x y)
    (setf (context-cur-user ctx) (cons (df x) (df y)))))

(defun arc (ctx cx cy r a0 a1 &optional ccw)
  (let ((sp (or (context-open ctx) (%new-subpath ctx))))
    (flatten-arc sp (ctm ctx) cx cy r a0 a1 ccw)
    (setf (context-cur-user ctx)
          (cons (+ (df cx) (* (df r) (cos (df a1)))) (+ (df cy) (* (df r) (sin (df a1))))))))

(defun ellipse (ctx cx cy rx ry rotation a0 a1 &optional ccw)
  "Approximate an ellipse by flattening it in a rotated/scaled local frame."
  (let* ((sp (or (context-open ctx) (%new-subpath ctx)))
         (m (mat-mul (mat-mul (ctm ctx) (mat-translate cx cy)) (mat-rotate rotation))))
    (flatten-arc sp (mat-mul m (mat-scale rx ry)) 0 0 1d0 a0 a1 ccw)
    (setf (context-open ctx) sp)))

(defun rect (ctx x y w h)
  (let ((sp (%new-subpath ctx)) (m (ctm ctx)))
    (%emit sp m x y) (%emit sp m (+ x w) y) (%emit sp m (+ x w) (+ y h)) (%emit sp m x (+ y h))
    (setf (subpath-closed sp) t (context-open ctx) nil (context-cur-user ctx) (cons (df x) (df y)))))

(defun close-path (ctx)
  (when (context-open ctx) (setf (subpath-closed (context-open ctx)) t)))

;;; ---- clipping -------------------------------------------------------------
(defun clip (ctx &optional fill-rule)
  "Intersect the current clip region with the fill of the current path (FILL-RULE
   :nonzero (default) or :evenodd), so subsequent paint is confined to it."
  (let* ((s (context-state ctx)) (cv (context-canvas ctx))
         (cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv))
         (m (subpaths-coverage-mask cw ch (context-subpaths ctx)
                                    (or fill-rule (gstate-fill-rule s))))
         (old (gstate-clip s)))
    (setf (gstate-clip s)
          (if old
              (let ((r (make-array (* cw ch) :element-type 'double-float)))
                (dotimes (i (* cw ch) r) (setf (aref r i) (* (aref old i) (aref m i)))))
              m))))

;;; ---- painting -------------------------------------------------------------
(defun fill-path (ctx)
  (let* ((s (context-state ctx))
         (*clip-mask* (gstate-clip s))
         (*blend-mode* (gstate-blend-mode s))
         (*soft-mask* (gstate-soft-mask s)))
    (fill-subpaths (context-canvas ctx) (context-subpaths ctx)
                   (gstate-fill-color s) (gstate-global-alpha s)
                   (%paint-inv ctx (gstate-fill-color s)) (gstate-fill-rule s))))

(defun stroke-path (ctx)
  (let* ((s (context-state ctx))
         (*clip-mask* (gstate-clip s))
         (*blend-mode* (gstate-blend-mode s))
         (*soft-mask* (gstate-soft-mask s)))
    (stroke-subpaths (context-canvas ctx) (context-subpaths ctx)
                     (gstate-stroke-color s) (gstate-line-width s) (gstate-global-alpha s)
                     (%paint-inv ctx (gstate-stroke-color s))
                     (gstate-line-dash s) (gstate-line-dash-offset s))))

(defun %rect-subpath (ctx x y w h)
  (let ((sp (make-subpath)) (m (ctm ctx)))
    (%emit sp m x y) (%emit sp m (+ x w) y) (%emit sp m (+ x w) (+ y h)) (%emit sp m x (+ y h))
    (setf (subpath-closed sp) t)
    sp))

(defun fill-rect (ctx x y w h)
  (let ((s (context-state ctx))
        (*clip-mask* (gstate-clip (context-state ctx))))
    (fill-subpaths (context-canvas ctx) (list (%rect-subpath ctx x y w h))
                   (gstate-fill-color s) (gstate-global-alpha s)
                   (%paint-inv ctx (gstate-fill-color s)))))

(defun stroke-rect (ctx x y w h)
  (let ((s (context-state ctx)))
    (stroke-subpaths (context-canvas ctx) (list (%rect-subpath ctx x y w h))
                     (gstate-stroke-color s) (gstate-line-width s) (gstate-global-alpha s)
                     (%paint-inv ctx (gstate-stroke-color s)))))

(defun clear-rect (ctx x y w h)
  "Erase the rectangle.  On an RGBA canvas the pixels become transparent black; on
   an opaque canvas they reset to the clear colour."
  (let ((cv (context-canvas ctx)))
    (if (scribe:canvas-alpha cv)
        (%clear-rgba-rect ctx x y w h)
        (fill-subpaths cv (list (%rect-subpath ctx x y w h))
                       (context-clear-color ctx) 1d0))))

(defun %clear-rgba-rect (ctx x y w h)
  "Zero the RGBA canvas over the device bounding box of user rect (X,Y,W,H)."
  (let* ((cv (context-canvas ctx)) (m (ctm ctx))
         (cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv))
         (px (scribe:canvas-pixels cv)) (ap (scribe:canvas-alpha cv))
         (xs '()) (ys '()))
    (dolist (corner (list (cons x y) (cons (+ x w) y) (cons (+ x w) (+ y h)) (cons x (+ y h))))
      (multiple-value-bind (dx dy) (mat-apply m (car corner) (cdr corner))
        (push dx xs) (push dy ys)))
    (let ((x0 (max 0 (floor (reduce #'min xs)))) (x1 (min cw (ceiling (reduce #'max xs))))
          (y0 (max 0 (floor (reduce #'min ys)))) (y1 (min ch (ceiling (reduce #'max ys)))))
      (loop for py from y0 below y1 do
        (loop for pxi from x0 below x1 do
          (let ((j (+ (* py cw) pxi)))
            (setf (aref ap j) 0
                  (aref px (* 3 j)) 0 (aref px (+ (* 3 j) 1)) 0 (aref px (+ (* 3 j) 2)) 0)))))))

(defun fill-text (ctx text x y)
  (let ((s (context-state ctx)))
    (draw-text* ctx text x y (paint->solid (gstate-fill-color s)) (gstate-global-alpha s))))

(defun stroke-text (ctx text x y)
  ;; a filled outline in the stroke colour approximates stroked text for now
  (let ((s (context-state ctx)))
    (draw-text* ctx text x y (paint->solid (gstate-stroke-color s)) (gstate-global-alpha s))))

(defun measure-text (ctx text) (text-width ctx text))

;;; ---- images ---------------------------------------------------------------
(defun draw-image (ctx src dx dy &optional dw dh)
  "Blit scribe canvas SRC at device (DX,DY), nearest-neighbour, honouring the
   current transform's translate + scale (rotation/skew is a later refinement)."
  (let* ((m (ctm ctx)) (cv (context-canvas ctx))
         (sw (scribe:canvas-width src)) (sh (scribe:canvas-height src))
         (dw (df (or dw sw))) (dh (df (or dh sh)))
         (a (gstate-global-alpha (context-state ctx)))
         (sp (scribe:canvas-pixels src)))
    (multiple-value-bind (x0 y0) (mat-apply m dx dy)
      (multiple-value-bind (x1 y1) (mat-apply m (+ (df dx) dw) (+ (df dy) dh))
        (let ((ix0 (floor (min x0 x1))) (iy0 (floor (min y0 y1)))
              (ix1 (ceiling (max x0 x1))) (iy1 (ceiling (max y0 y1))))
          (loop for py from iy0 below iy1 do
            (loop for px from ix0 below ix1 do
              (let* ((u (/ (- (+ px 0.5d0) (min x0 x1)) (max 1d-6 (abs (- x1 x0)))))
                     (v (/ (- (+ py 0.5d0) (min y0 y1)) (max 1d-6 (abs (- y1 y0)))))
                     (sx (min (1- sw) (max 0 (floor (* u sw)))))
                     (sy (min (1- sh) (max 0 (floor (* v sh))))))
                (when (and (>= u 0d0) (< u 1d0) (>= v 0d0) (< v 1d0))
                  (let ((i (* 3 (+ (* sy sw) sx))))
                    (scribe:blend-coverage cv px py a
                                           (list (aref sp i) (aref sp (+ i 1)) (aref sp (+ i 2)))))))))))))
  (values))

(defun draw-image-rgba (ctx rgba iw ih)
  "Composite a straight-alpha RGBA image (RGBA an (IW*IH*4) octet vector, row 0 at
   the TOP) onto the canvas.  The image occupies the unit square [0,1]x[0,1] in the
   CURRENT user space; that square is mapped through the CTM to a device
   parallelogram (so rotation and skew are handled, not just translate+scale).  For
   each covered device pixel we inverse-map to (u,v) in the unit square, sample the
   nearest source texel, and blend it honouring the per-pixel alpha, the global
   alpha, and the active clip mask."
  (let* ((m (ctm ctx)) (inv (mat-invert m)))
    (when (null inv) (return-from draw-image-rgba (values)))
    (let* ((cv (context-canvas ctx))
           (cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv))
           (ga (gstate-global-alpha (context-state ctx)))
           (*clip-mask* (gstate-clip (context-state ctx)))
           (*blend-mode* (gstate-blend-mode (context-state ctx)))
           (*soft-mask* (gstate-soft-mask (context-state ctx)))
           ;; images composite in device (gamma) space — partial-alpha (SMask /
           ;; constant-alpha) pixels then match pdfium; opaque pixels are unchanged.
           (*straight-composite* t)
           (xs '()) (ys '()))
      ;; device bounding box of the mapped unit square
      (dolist (corner '((0d0 . 0d0) (1d0 . 0d0) (1d0 . 1d0) (0d0 . 1d0)))
        (multiple-value-bind (dx dy) (mat-apply m (car corner) (cdr corner))
          (push dx xs) (push dy ys)))
      (let ((x0 (max 0 (floor (reduce #'min xs)))) (x1 (min cw (ceiling (reduce #'max xs))))
            (y0 (max 0 (floor (reduce #'min ys)))) (y1 (min ch (ceiling (reduce #'max ys)))))
        (loop for py from y0 below y1 do
          (loop for pxi from x0 below x1 do
            (multiple-value-bind (u v) (mat-apply inv (+ pxi 0.5d0) (+ py 0.5d0))
              (when (and (>= u 0d0) (< u 1d0) (>= v 0d0) (< v 1d0))
                (let* ((sx (min (1- iw) (max 0 (floor (* u iw)))))
                       ;; v=1 is the TOP of the unit square = image row 0
                       (sy (min (1- ih) (max 0 (floor (* (- 1d0 v) ih)))))
                       (si (* 4 (+ (* sy iw) sx)))
                       (sa (/ (aref rgba (+ si 3)) 255d0))
                       (cov (%clip-at pxi py cv))
                       (a (* sa ga cov)))
                  (when (> a 0d0)
                    (%composite cv pxi py a
                                (list (aref rgba si) (aref rgba (+ si 1))
                                      (aref rgba (+ si 2))))))))))))
    (values)))
