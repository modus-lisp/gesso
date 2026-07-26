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

(defvar *blend-mode* :normal
  "Current separable blend mode (ISO 32000-1 §11.3.5 / CSS Compositing 1) applied
   when compositing a source colour onto the backdrop.  :NORMAL = source-over,
   byte-identical to the pre-transparency path.  Other values (:multiply :screen
   :overlay :darken :lighten :color-dodge :color-burn :hard-light :soft-light
   :difference :exclusion) blend each source channel Cs against the backdrop Cb
   as B(Cb,Cs) before the over-composite.")

(defvar *soft-mask* nil
  "When bound to a device-space alpha array (canvas-width*canvas-height doubles,
   0..1), every composited coverage is multiplied by it — an ExtGState soft mask
   (ISO 32000-1 §11.6.5.2).  NIL = no soft mask.")

(defvar *straight-composite* nil
  "When true, the :NORMAL / no-soft-mask path composites in DEVICE (gamma) space via
   %STRAIGHT-OVER instead of scribe's linear-light coverage blend.  Used for image
   blits, whose partial-alpha (SMask / constant-alpha) pixels must match PDF/pdfium's
   device-space source-over (§11.3.6).  At full coverage this is byte-identical to the
   linear path (both yield the source), so opaque images are unchanged.")

(defun %clip-at (px py cv)
  "The active clip coverage at device pixel (PX,PY), or 1d0 when unclipped."
  (if *clip-mask*
      (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
        (if (and (>= px 0) (< px cw) (>= py 0) (< py ch))
            (aref *clip-mask* (+ (* py cw) px))
            0d0))
      1d0))

(defun %mask-at (px py cv)
  "The active soft-mask alpha at device pixel (PX,PY), or 1d0 when unmasked."
  (if *soft-mask*
      (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
        (if (and (>= px 0) (< px cw) (>= py 0) (< py ch))
            (aref *soft-mask* (+ (* py cw) px))
            0d0))
      1d0))

(defun %blend-sep (mode cb cs)
  "Separable blend of one backdrop channel CB with source channel CS (8-bit 0..255),
   per ISO 32000-1 §11.3.5 (the same formulas as CSS Compositing 1)."
  (declare (type (unsigned-byte 8) cb cs))
  (ecase mode
    (:multiply    (truncate (* cb cs) 255))
    (:screen      (- 255 (truncate (* (- 255 cb) (- 255 cs)) 255)))
    (:overlay     (if (< cb 128) (truncate (* 2 cb cs) 255)
                      (- 255 (truncate (* 2 (- 255 cb) (- 255 cs)) 255))))
    (:darken      (min cb cs))
    (:lighten     (max cb cs))
    (:color-dodge (cond ((= cb 0) 0) ((= cs 255) 255)
                        (t (min 255 (truncate (* 255 cb) (- 255 cs))))))
    (:color-burn  (cond ((= cb 255) 255) ((= cs 0) 0)
                        (t (- 255 (min 255 (truncate (* 255 (- 255 cb)) cs))))))
    (:hard-light  (if (< cs 128) (truncate (* 2 cs cb) 255)
                      (- 255 (truncate (* 2 (- 255 cs) (- 255 cb)) 255))))
    (:soft-light  (let ((b (/ cb 255d0)) (s (/ cs 255d0)))
                    (let ((res (if (<= s 0.5d0)
                                   (- b (* (- 1d0 (* 2d0 s)) b (- 1d0 b)))
                                   (let ((d (if (<= b 0.25d0)
                                                (* (- (* 16d0 b) 12d0) (+ (* b b) b))
                                                (sqrt b))))
                                     (+ b (* (- (* 2d0 s) 1d0) (- d b)))))))
                      (min 255 (max 0 (round (* 255d0 res)))))))
    (:difference  (abs (- cb cs)))
    (:exclusion   (- (+ cb cs) (truncate (* 2 cb cs) 255)))))

;;; ---- non-separable blend modes (ISO 32000-1 §11.3.5.3) --------------------
;;; Hue / Saturation / Color / Luminosity act on the whole (r g b) triple, not
;;; channel-by-channel, via the standard Lum / SetLum / Sat / SetSat helpers.
;;; Triples here are floats in 0..1.

(declaim (inline %nonsep-mode-p))
(defun %nonsep-mode-p (mode)
  (case mode ((:hue :saturation :color :luminosity) t) (t nil)))

(defun %lum (c) (+ (* 0.3d0 (first c)) (* 0.59d0 (second c)) (* 0.11d0 (third c))))

(defun %clip-color (c)
  "Clip an (r g b) triple back into the unit gamut about its luminance (§11.3.5.3)."
  (let* ((l (%lum c))
         (r (first c)) (g (second c)) (b (third c))
         (n (min r g b)) (x (max r g b)))
    (when (< n 0d0)
      (flet ((f (v) (if (= l n) l (+ l (/ (* (- v l) l) (- l n))))))
        (setf r (f r) g (f g) b (f b))))
    (when (> x 1d0)
      (flet ((f (v) (if (= x l) l (+ l (/ (* (- v l) (- 1d0 l)) (- x l))))))
        (setf r (f r) g (f g) b (f b))))
    (list r g b)))

(defun %set-lum (c l)
  (let ((d (- l (%lum c))))
    (%clip-color (list (+ (first c) d) (+ (second c) d) (+ (third c) d)))))

(defun %sat (c) (- (max (first c) (second c) (third c))
                   (min (first c) (second c) (third c))))

(defun %set-sat (c s)
  "Set the saturation of triple C to S (§11.3.5.3): min->0, max->s, mid scaled."
  (let* ((v (make-array 3 :element-type 'double-float
                          :initial-contents (list (float (first c) 1d0)
                                                   (float (second c) 1d0)
                                                   (float (third c) 1d0))))
         ;; index of min, mid, max (stable for ties)
         (mn 0) (mx 0))
    (dotimes (i 3)
      (when (< (aref v i) (aref v mn)) (setf mn i))
      (when (> (aref v i) (aref v mx)) (setf mx i)))
    (when (= mn mx) (setf mx (mod (1+ mn) 3)))     ; all equal: pick any distinct
    (let ((md (- 3 mn mx)))
      (when (= md mn) (setf md mx))                ; degenerate guard
      (if (> (aref v mx) (aref v mn))
          (progn
            (setf (aref v md) (/ (* (- (aref v md) (aref v mn)) (float s 1d0))
                                 (- (aref v mx) (aref v mn))))
            (setf (aref v mx) (float s 1d0)))
          (setf (aref v md) 0d0 (aref v mx) 0d0))
      (setf (aref v mn) 0d0))
    (list (aref v 0) (aref v 1) (aref v 2))))

(defun %blend-nonsep (mode cb cs)
  "Non-separable blend of 8-bit backdrop CB with 8-bit source CS -> 8-bit (r g b)."
  (let ((b (mapcar (lambda (x) (/ x 255d0)) cb))
        (s (mapcar (lambda (x) (/ x 255d0)) cs)))
    (let ((res (ecase mode
                 (:hue        (%set-lum (%set-sat s (%sat b)) (%lum b)))
                 (:saturation (%set-lum (%set-sat b (%sat s)) (%lum b)))
                 (:color      (%set-lum s (%lum b)))
                 (:luminosity (%set-lum b (%lum s))))))
      (mapcar (lambda (x) (max 0 (min 255 (round (* 255d0 x))))) res))))

(defun %blend-backdrop (cv px py color)
  "COLOR (source r g b) blended against the canvas backdrop at (PX,PY) under
   *BLEND-MODE*, returning the blended (r g b) to be over-composited.  Reads the
   opaque RGB backdrop directly (folio's page canvas).  Separable modes blend each
   channel independently; the four non-separable modes blend the whole triple."
  (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
    (if (and (>= px 0) (< px cw) (>= py 0) (< py ch))
        (let* ((p (scribe:canvas-pixels cv))
               (i (* 3 (+ (* py cw) px))))
          (if (%nonsep-mode-p *blend-mode*)
              (%blend-nonsep *blend-mode*
                             (list (aref p i) (aref p (+ i 1)) (aref p (+ i 2)))
                             color)
              (list (%blend-sep *blend-mode* (aref p i)       (first color))
                    (%blend-sep *blend-mode* (aref p (+ i 1)) (second color))
                    (%blend-sep *blend-mode* (aref p (+ i 2)) (third color)))))
        color)))

(defun %straight-over (cv px py cov src)
  "Straight (device-space, non-linear) source-over of SRC (r g b) at coverage COV
   onto the opaque RGB canvas: C = (1-COV)*backdrop + COV*SRC per 8-bit channel.
   This matches how PDF/pdfium composite a soft-masked or blended mark (ISO 32000-1
   §11.3.6), rather than scribe's linear-light coverage blend."
  (declare (type double-float cov))
  (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
    (when (and (>= px 0) (< px cw) (>= py 0) (< py ch))
      (let* ((p (scribe:canvas-pixels cv))
             (i (* 3 (+ (* py cw) px)))
             (k (max 0d0 (min 1d0 cov))) (ik (- 1d0 k)))
        (flet ((mix (bg fg) (max 0 (min 255 (round (+ (* ik bg) (* k fg)))))))
          (setf (aref p i)       (mix (aref p i)       (first src))
                (aref p (+ i 1)) (mix (aref p (+ i 1)) (second src))
                (aref p (+ i 2)) (mix (aref p (+ i 2)) (third src))))))))

(declaim (inline %composite))
(defun %composite (cv px py cov color)
  "Composite COLOR at device pixel (PX,PY) with coverage COV, honouring *SOFT-MASK*
   (multiplies coverage) and *BLEND-MODE* (blends the source against the backdrop
   before the over-composite).  With NO mask and :NORMAL mode this is exactly
   SCRIBE:BLEND-COVERAGE — byte-identical to the pre-transparency path.  When a soft
   mask or a non-normal blend mode is active, the mark is composited in device space
   (straight source-over) to match PDF/pdfium transparency."
  (declare (type double-float cov))
  (let ((cov (if *soft-mask* (* cov (%mask-at px py cv)) cov)))
    (cond
      ((and (eq *blend-mode* :normal) (null *soft-mask*) (not *straight-composite*))
       (scribe:blend-coverage cv px py cov color))     ; fast path (byte-identical)
      ((<= cov 0d0) nil)
      (t
       (let ((src (if (eq *blend-mode* :normal) color (%blend-backdrop cv px py color))))
         (if (scribe:canvas-alpha cv)
             (scribe:blend-coverage cv px py cov src)   ; RGBA surface (weft): keep as-is
             (%straight-over cv px py cov src)))))))

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
                        (%composite cv px py (* c a sa) (list r g b))))))))))
        (dotimes (yy h)
          (let ((py (+ iy0 yy)) (row (* yy w)))
            (dotimes (xx w)
              (let* ((px (+ ix0 xx))
                     (c (* (aref cov (+ row xx)) (%clip-at px py cv))))
                (when (> c 0d0)
                  (%composite cv px py (* c a) paint)))))))))

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

(defun %clip-bounds (cv clip)
  "Device bounding box (values X0 Y0 X1 Y1, half-open) of the nonzero region of the
   coverage array CLIP, or the whole canvas when CLIP is NIL.  Limits a per-pixel
   paint (a shading) to the clipped area."
  (let ((cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv)))
    (if (null clip)
        (values 0 0 cw ch)
        (let ((minx cw) (miny ch) (maxx -1) (maxy -1))
          (dotimes (y ch)
            (let ((row (* y cw)))
              (dotimes (x cw)
                (when (> (aref clip (+ row x)) 0d0)
                  (when (< x minx) (setf minx x))
                  (when (> x maxx) (setf maxx x))
                  (when (< y miny) (setf miny y))
                  (when (> y maxy) (setf maxy y))))))
          (if (>= maxx 0)
              (values minx miny (1+ maxx) (1+ maxy))
              (values 0 0 0 0))))))

(defun clip-bounds (ctx)
  "Device-space bounding box (values X0 Y0 X1 Y1, half-open) of the current clip
   region, or the whole canvas when unclipped.  Lets a caller (a PDF tiling pattern)
   know which area it must cover."
  (%clip-bounds (context-canvas ctx) (gstate-clip (context-state ctx))))

(defun fill-callback (ctx fn)
  "Paint a per-pixel colour source across the current clip region: for each device
   pixel (PX,PY) inside the active clip, call (FN PX PY), which returns either NIL
   (leave the pixel untouched) or (values R G B ALPHA) with R,G,B 8-bit and ALPHA in
   0..1.  The result is composited through the SAME chokepoint as fill-path — the
   clip coverage, the global alpha, the blend mode and the soft mask all apply — so a
   PDF shading (ISO 32000-1 §8.7.4.5) under a soft mask or blend mode composites
   correctly.  Iterates only the clip's bounding box."
  (let* ((s (context-state ctx))
         (cv (context-canvas ctx))
         (ga (gstate-global-alpha s))
         (*clip-mask* (gstate-clip s))
         (*blend-mode* (gstate-blend-mode s))
         (*soft-mask* (gstate-soft-mask s)))
    (multiple-value-bind (x0 y0 x1 y1) (%clip-bounds cv (gstate-clip s))
      (loop for py from y0 below y1 do
        (loop for px from x0 below x1 do
          (let ((cc (%clip-at px py cv)))
            (when (> cc 0d0)
              (multiple-value-bind (r g b a) (funcall fn px py)
                (when (and r a (> (the double-float (df a)) 0d0))
                  (%composite cv px py (* cc ga (df a)) (list r g b)))))))))
    (values)))

(defun composite-group (ctx src &optional bx0 by0 bx1 by1)
  "Composite an offscreen transparency-group context SRC (an RGBA context the same
   device size as CTX's canvas) onto CTX's canvas as ONE group (ISO 32000-1 §11.4.6):
   the group's straight colour and alpha are recovered per pixel (SRC holds
   premultiplied RGB + an alpha plane) and composited through the SAME chokepoint as
   fill-path — so CTX's current global alpha (the group constant alpha), blend mode,
   soft mask and clip all apply to the group result exactly once, instead of to each
   inner mark.  Optional device bounds BX0..BY1 (the group's BBox) restrict the loop."
  (let* ((s (context-state ctx))
         (cv (context-canvas ctx))
         (scv (context-canvas src))
         (cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv))
         (sp (scribe:canvas-pixels scv)) (sa (scribe:canvas-alpha scv))
         (ga (gstate-global-alpha s))
         (*clip-mask* (gstate-clip s))
         (*blend-mode* (gstate-blend-mode s))
         (*soft-mask* (gstate-soft-mask s))
         ;; the group is composited in device (gamma) space, like an image blit:
         ;; partial-alpha edges then match pdfium's source-over (§11.3.6).
         (*straight-composite* t))
    (unless (and sa (= cw (scribe:canvas-width scv)) (= ch (scribe:canvas-height scv)))
      (return-from composite-group (values)))
    (let ((x0 (max 0 (floor (or bx0 0)))) (y0 (max 0 (floor (or by0 0))))
          (x1 (min cw (ceiling (or bx1 cw)))) (y1 (min ch (ceiling (or by1 ch)))))
      (loop for py from y0 below y1 do
        (loop for px from x0 below x1 do
          (let* ((j (+ (* py cw) px))
                 (av (aref sa j)))
            (when (> av 0)
              (let* ((af (/ av 255d0))
                     (i (* 3 j))
                     ;; un-premultiply premultiplied RGB back to straight colour
                     (r (min 255 (max 0 (round (/ (aref sp i)       af)))))
                     (g (min 255 (max 0 (round (/ (aref sp (+ i 1)) af)))))
                     (b (min 255 (max 0 (round (/ (aref sp (+ i 2)) af)))))
                     (cov (* af ga (%clip-at px py cv))))
                (when (> cov 0d0)
                  (%composite cv px py cov (list r g b)))))))))
    (values)))

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
