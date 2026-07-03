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
  (font-family nil))

(defun copy-state (s)
  (make-gstate :transform (copy-list (gstate-transform s))
               :fill-color (gstate-fill-color s) :stroke-color (gstate-stroke-color s)
               :line-width (gstate-line-width s) :global-alpha (gstate-global-alpha s)
               :font-size (gstate-font-size s) :font-weight (gstate-font-weight s)
               :font-style (gstate-font-style s) :font-family (gstate-font-family s)))

(defstruct (context (:constructor %make-context))
  canvas
  (state (make-gstate))
  (stack nil)
  (subpaths nil)          ; list of subpaths (fill/stroke targets)
  (open nil)              ; the subpath currently being extended
  (cur-user nil)          ; current point in USER space (for curve starts)
  (clear-color '(255 255 255)))

(defun make-context (width height &key (background '(255 255 255)) canvas)
  "A 2D context over a new scribe canvas WIDTH x HEIGHT (filled BACKGROUND), or
   over an existing scribe CANVAS when supplied (the weft integration path)."
  (%make-context :canvas (or canvas (scribe:make-canvas width height background))
                 :clear-color background))

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
(defun set-stroke (ctx color) (setf (gstate-stroke-color (context-state ctx)) color))
(defun set-line-width (ctx w) (setf (gstate-line-width (context-state ctx)) (df w)))
(defun set-global-alpha (ctx a) (setf (gstate-global-alpha (context-state ctx)) (max 0d0 (min 1d0 (df a)))))
(defun set-font (ctx size &key family (weight 400) (style :normal))
  (let ((s (context-state ctx)))
    (setf (gstate-font-size s) (df size) (gstate-font-family s) family
          (gstate-font-weight s) weight (gstate-font-style s) style)))

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

;;; ---- painting -------------------------------------------------------------
(defun fill-path (ctx)
  (let ((s (context-state ctx)))
    (fill-subpaths (context-canvas ctx) (context-subpaths ctx)
                   (gstate-fill-color s) (gstate-global-alpha s))))

(defun stroke-path (ctx)
  (let ((s (context-state ctx)))
    (stroke-subpaths (context-canvas ctx) (context-subpaths ctx)
                     (gstate-stroke-color s) (gstate-line-width s) (gstate-global-alpha s))))

(defun %rect-subpath (ctx x y w h)
  (let ((sp (make-subpath)) (m (ctm ctx)))
    (%emit sp m x y) (%emit sp m (+ x w) y) (%emit sp m (+ x w) (+ y h)) (%emit sp m x (+ y h))
    (setf (subpath-closed sp) t)
    sp))

(defun fill-rect (ctx x y w h)
  (let ((s (context-state ctx)))
    (fill-subpaths (context-canvas ctx) (list (%rect-subpath ctx x y w h))
                   (gstate-fill-color s) (gstate-global-alpha s))))

(defun stroke-rect (ctx x y w h)
  (let ((s (context-state ctx)))
    (stroke-subpaths (context-canvas ctx) (list (%rect-subpath ctx x y w h))
                     (gstate-stroke-color s) (gstate-line-width s) (gstate-global-alpha s))))

(defun clear-rect (ctx x y w h)
  "Reset the rectangle to the canvas's clear colour (opaque model: no alpha)."
  (fill-subpaths (context-canvas ctx) (list (%rect-subpath ctx x y w h))
                 (context-clear-color ctx) 1d0))

(defun fill-text (ctx text x y)
  (let ((s (context-state ctx)))
    (draw-text* ctx text x y (gstate-fill-color s) (gstate-global-alpha s))))

(defun stroke-text (ctx text x y)
  ;; a filled outline in the stroke colour approximates stroked text for now
  (let ((s (context-state ctx)))
    (draw-text* ctx text x y (gstate-stroke-color s) (gstate-global-alpha s))))

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
