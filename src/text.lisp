;;;; text.lisp — fillText / measureText through scribe shaping.
;;;;
;;;; Text is shaped by scribe (GSUB/GPOS) and each glyph rasterized to coverage,
;;;; then composited with the fill colour.  The current transform contributes its
;;;; translation (the text origin) and its mean scale (the rasterization ppem);
;;;; rotation/skew of glyph outlines is a later refinement.
(in-package #:gesso)

(defvar *font-cache* (make-hash-table :test 'equal))

(defun resolve-font (family weight style)
  "A scribe font for FAMILY (a list of family strings or NIL), WEIGHT (100-900)
   and STYLE (:normal/:italic), cached.  NIL if no face is available."
  (let ((key (list family weight style)))
    (multiple-value-bind (hit present) (gethash key *font-cache*)
      (if present hit
          (setf (gethash key *font-cache*)
                (ignore-errors (scribe:match-font family :weight weight :style style)))))))

(defun %shape-px (font text ppem)
  "Shape TEXT with FONT at PPEM.  Returns a list of (gid xadv xoff yoff) in px."
  (let* ((upem (float (scribe:font-units-per-em font) 1d0))
         (s (/ ppem upem)))
    (loop for g across (scribe:shape-run font text)
          for gid = (scribe::glyph-pos-gid g)
          collect (list gid
                        (if (zerop gid) (* 0.5d0 ppem) (* (scribe::glyph-pos-x-advance g) s))
                        (* (scribe::glyph-pos-x-offset g) s)
                        (* (scribe::glyph-pos-y-offset g) s)))))

(defun text-ppem (ctx)
  (let ((st (context-state ctx)))
    (max 1d0 (* (gstate-font-size st) (mat-mean-scale (gstate-transform st))))))

(defun draw-text* (ctx text x y color alpha)
  "Shape and blit TEXT with its baseline origin at user point (X,Y)."
  (let* ((st (context-state ctx))
         (font (resolve-font (gstate-font-family st) (gstate-font-weight st)
                             (gstate-font-style st))))
    (when (and font (plusp (length text)))
      (multiple-value-bind (ox oy) (mat-apply (gstate-transform st) x y)
        (let ((ppem (text-ppem ctx)) (penx (df ox)) (cv (context-canvas ctx)) (a (df alpha)))
          (dolist (g (%shape-px font text ppem))
            (destructuring-bind (gid xadv xoff yoff) g
              (if (zerop gid)
                  (incf penx xadv)
                  (let ((sub (- (+ penx xoff) (ffloor (+ penx xoff)))))
                    (multiple-value-bind (cov w h left top adv)
                        (scribe:rasterize-glyph font gid ppem :subpixel sub)
                      (declare (ignore adv))
                      (when cov
                        (let ((gx (+ (floor (+ penx xoff)) left))
                              (gy (+ (round oy) top (- (round yoff)))))
                          (dotimes (yy h)
                            (dotimes (xx w)
                              (let ((c (aref cov (+ (* yy w) xx))))
                                (when (> c 0d0)
                                  (scribe:blend-coverage cv (+ gx xx) (+ gy yy) (* c a) color)))))))
                      (incf penx xadv)))))))))))

(defun text-width (ctx text)
  "Advance width of TEXT in device px at the current font/scale, or 0."
  (let* ((st (context-state ctx))
         (font (resolve-font (gstate-font-family st) (gstate-font-weight st)
                             (gstate-font-style st))))
    (if (and font (plusp (length text)))
        (reduce #'+ (%shape-px font text (text-ppem ctx)) :key #'second :initial-value 0d0)
        0d0)))
