;;;; raster.lisp — fill a set of subpaths by scan-converting through scribe.
;;;;
;;;; Device-space polylines are handed to scribe:rasterize-outline (the analytic
;;;; signed-area rasterizer used for glyph outlines) as move/line contours, then
;;;; the returned coverage bitmap is composited onto the canvas with the fill
;;;; colour.  scribe's rasterizer is authored for font outlines (y-up, y flipped
;;;; on output); a canvas is y-down, so we feed negated y and an origin that
;;;; un-flips it, landing coverage row 0 at the top of the shape's bounding box.
(in-package #:gesso)

(defun %paths-bbox (subpaths)
  "Device-space (values minx miny maxx maxy) over all points, or NIL if empty."
  (let ((minx 1d30) (miny 1d30) (maxx -1d30) (maxy -1d30) (any nil))
    (dolist (sp subpaths)
      (loop for p across (subpath-points sp) do
        (setf any t
              minx (min minx (car p)) maxx (max maxx (car p))
              miny (min miny (cdr p)) maxy (max maxy (cdr p)))))
    (when any (values minx miny maxx maxy))))

(defun %subpath-contour (sp)
  "One scribe contour (a move + lines, y negated) for subpath SP, or NIL if SP
   has fewer than two points."
  (let ((pts (subpath-points sp)))
    (when (>= (length pts) 2)
      (let ((segs (list (list :move (car (aref pts 0)) (- (cdr (aref pts 0)))))))
        (loop for i from 1 below (length pts)
              do (push (list :line (car (aref pts i)) (- (cdr (aref pts i)))) segs))
        (nreverse segs)))))

(defun path-user-bounds (ctx)
  "(values minx miny maxx maxy) of the current path in USER space (the current
   path's device bounds mapped back through the inverse CTM), or NIL if empty.
   Used to place an objectBoundingBox gradient over a shape."
  (multiple-value-bind (dnx dny dxx dxy) (%paths-bbox (context-subpaths ctx))
    (when dnx
      (let ((inv (mat-invert (ctm ctx))))
        (if (null inv)
            (values dnx dny dxx dxy)
            (let ((xs '()) (ys '()))
              (dolist (c (list (cons dnx dny) (cons dxx dny) (cons dxx dxy) (cons dnx dxy)))
                (multiple-value-bind (ux uy) (mat-apply inv (car c) (cdr c))
                  (push ux xs) (push uy ys)))
              (values (reduce #'min xs) (reduce #'min ys)
                      (reduce #'max xs) (reduce #'max ys))))))))

(defvar *clip-mask* nil
  "When bound to a device-space coverage array (canvas-width*canvas-height doubles,
   0..1), every fill/stroke coverage is multiplied by it — the intersection of the
   active clip regions (SVG clip-path / canvas clip()).  NIL = no clip.")

(defun %clip-at (px py cv)
  "The active clip coverage at device pixel (PX,PY), or 1d0 when unclipped."
  (if *clip-mask*
      (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
        (if (and (>= px 0) (< px cw) (>= py 0) (< py ch))
            (aref *clip-mask* (+ (* py cw) px))
            0d0))
      1d0))

(defun %blit-coverage (cv cov w h ix0 iy0 paint alpha inv)
  "Composite coverage bitmap COV (w*h) at device origin (IX0,IY0) with PAINT scaled
   by ALPHA in [0,1].  PAINT is a solid (r g b) list or a GRADIENT; a gradient is
   evaluated per pixel by mapping the device point back through INV (the inverse of
   the transform in force at paint time), folding the stop alpha into coverage.
   Coverage is intersected with the active *CLIP-MASK* (if any)."
  (let ((a (df alpha)))
    (if (gradient-p paint)
        (dotimes (yy h)
          (let ((py (+ iy0 yy)) (row (* yy w)))
            (dotimes (xx w)
              (let* ((px (+ ix0 xx))
                     (c (* (aref cov (+ row xx)) (%clip-at px py cv))))
                (when (> c 0d0)
                  (multiple-value-bind (ux uy) (mat-apply inv (+ px 0.5d0) (+ py 0.5d0))
                    (multiple-value-bind (r g b sa) (gradient-color-at paint ux uy)
                      (when (> sa 0d0)
                        (scribe:blend-coverage cv px py (* c a sa) (list r g b))))))))))
        (dotimes (yy h)
          (let ((py (+ iy0 yy)) (row (* yy w)))
            (dotimes (xx w)
              (let* ((px (+ ix0 xx))
                     (c (* (aref cov (+ row xx)) (%clip-at px py cv))))
                (when (> c 0d0)
                  (scribe:blend-coverage cv px py (* c a) paint)))))))))

(defun %evenodd-coverage (subpaths)
  "(values COV W H IX0 IY0): even-odd combined coverage over the union bbox of
   SUBPATHS.  Each subpath is rasterized independently (its non-zero interior) in a
   frame anchored at the union top-left, then XOR-combined (a+b-2ab) so overlapping
   interiors cancel — the even-odd rule for non-self-intersecting subpaths."
  (multiple-value-bind (minx miny maxx maxy) (%paths-bbox subpaths)
    (when minx
      (let* ((ix0 (floor minx)) (iy0 (floor miny))
             (w (max 1 (+ 2 (ceiling (- maxx ix0)))))
             (h (max 1 (+ 2 (ceiling (- maxy iy0)))))
             (acc (make-array (* w h) :element-type 'double-float :initial-element 0d0)))
        (dolist (sp subpaths)
          (let ((c (%subpath-contour sp)))
            (when c
              (multiple-value-bind (cov cw ch)
                  (scribe:rasterize-outline (list c) 1d0
                                            :origin-x (df ix0) :origin-y (df (- iy0)))
                (when cov
                  (dotimes (yy (min ch h))
                    (dotimes (xx (min cw w))
                      (let* ((idx (+ (* yy w) xx))
                             (a (aref acc idx)) (v (aref cov (+ (* yy cw) xx))))
                        (setf (aref acc idx) (+ a v (* -2d0 a v)))))))))))
        (values acc w h ix0 iy0)))))

(defun fill-subpaths (cv subpaths paint alpha &optional inv fill-rule)
  "Fill SUBPATHS onto scribe canvas CV with PAINT and ALPHA.  PAINT is a solid
   colour or a GRADIENT (evaluated through INV, the inverse CTM).  FILL-RULE is
   :nonzero (default) or :evenodd."
  (if (eq fill-rule :evenodd)
      (multiple-value-bind (cov w h ix0 iy0) (%evenodd-coverage subpaths)
        (when cov (%blit-coverage cv cov w h ix0 iy0 paint alpha inv)))
      (multiple-value-bind (minx miny) (%paths-bbox subpaths)
        (when minx
          (let* ((ix0 (floor minx)) (iy0 (floor miny))
                 (contours (loop for sp in subpaths
                                 for c = (%subpath-contour sp)
                                 when c collect c)))
            (when contours
              (multiple-value-bind (cov w h)
                  (scribe:rasterize-outline contours 1d0
                                            :origin-x (df ix0) :origin-y (df (- iy0)))
                (when cov (%blit-coverage cv cov w h ix0 iy0 paint alpha inv)))))))))

(defun subpaths-coverage-mask (cw ch subpaths &optional (fill-rule :nonzero))
  "A CW*CH device-space coverage mask (double 0..1) for the FILL of SUBPATHS — the
   region a clip() would keep.  FILL-RULE is :nonzero or :evenodd."
  (let ((mask (make-array (* cw ch) :element-type 'double-float :initial-element 0d0)))
    (flet ((stamp (cov w h ix0 iy0)
             (when cov
               (dotimes (yy h)
                 (let ((py (+ iy0 yy)))
                   (when (and (>= py 0) (< py ch))
                     (dotimes (xx w)
                       (let ((px (+ ix0 xx)))
                         (when (and (>= px 0) (< px cw))
                           (setf (aref mask (+ (* py cw) px))
                                 (aref cov (+ (* yy w) xx))))))))))))
      (if (eq fill-rule :evenodd)
          (multiple-value-bind (cov w h ix0 iy0) (%evenodd-coverage subpaths)
            (stamp cov w h ix0 iy0))
          (multiple-value-bind (minx miny) (%paths-bbox subpaths)
            (when minx
              (let* ((ix0 (floor minx)) (iy0 (floor miny))
                     (contours (loop for sp in subpaths for c = (%subpath-contour sp)
                                     when c collect c)))
                (when contours
                  (multiple-value-bind (cov w h)
                      (scribe:rasterize-outline contours 1d0
                                                :origin-x (df ix0) :origin-y (df (- iy0)))
                    (stamp cov w h ix0 iy0))))))))
    mask))
