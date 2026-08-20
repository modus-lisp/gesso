;;;; self-test.lisp — exercises the gesso 2D pipeline end to end.
;;;;   sbcl --script inspect/self-test.lisp
(require :asdf)
;; EVERY sibling, not scribe by name.  scribe later grew a brotli-pure dependency, and
;; from that day the command on line 2 died inside ASDF before the first check ran —
;; a suite that will not start looks exactly like a suite with nothing to say.
(let ((here (directory-namestring *load-truename*)))
  (push (truename (merge-pathnames "../" here)) asdf:*central-registry*)
  (dolist (d (directory (merge-pathnames "../../*/" here)))
    (push d asdf:*central-registry*)))
(asdf:load-system "gesso")
(defpackage #:gesso.self-test (:use #:cl) (:local-nicknames (#:g #:gesso) (#:s #:scribe)))
(in-package #:gesso.self-test)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (label test) (if test (incf *pass*) (progn (incf *fail*) (format t "~&FAIL ~a~%" label))))

(defun px (ctx x y)
  (let* ((cv (g:context-canvas ctx)) (w (s:canvas-width cv)) (p (s:canvas-pixels cv))
         (i (* 3 (+ (* y w) x))))
    (list (aref p i) (aref p (+ i 1)) (aref p (+ i 2)))))

(defun ink (ctx)
  (let* ((cv (g:context-canvas ctx)) (p (s:canvas-pixels cv)) (n 0))
    (loop for i from 0 below (length p) by 3
          unless (and (= (aref p i) 255) (= (aref p (+ i 1)) 255) (= (aref p (+ i 2)) 255))
            do (incf n))
    n))

(defun near (a b &optional (tol 12)) (<= (abs (- a b)) tol))
(defun color-near (c want) (every #'near c want))

;;; fill-rect lands the fill colour on an interior pixel
(let ((ctx (g:make-context 64 64)))
  (g:set-fill ctx '(220 30 30))
  (g:fill-rect ctx 10 10 20 20)
  (ok "fill-rect interior colour" (color-near (px ctx 20 20) '(220 30 30)))
  (ok "fill-rect leaves outside untouched" (color-near (px ctx 2 2) '(255 255 255))))

;;; a filled triangle path covers its centroid and not a far corner
(let ((ctx (g:make-context 64 64)))
  (g:set-fill ctx '(20 120 220))
  (g:begin-path ctx)
  (g:move-to ctx 32 8) (g:line-to ctx 56 56) (g:line-to ctx 8 56) (g:close-path ctx)
  (g:fill-path ctx)
  (ok "triangle fills centroid" (color-near (px ctx 32 44) '(20 120 220)))
  (ok "triangle misses top corner" (color-near (px ctx 4 4) '(255 255 255))))

;;; a stroked horizontal line lays ink along its length at the right thickness
(let ((ctx (g:make-context 64 64)))
  (g:set-stroke ctx '(0 0 0)) (g:set-line-width ctx 4)
  (g:begin-path ctx) (g:move-to ctx 8 32) (g:line-to ctx 56 32) (g:stroke-path ctx)
  (ok "stroke covers the line" (not (color-near (px ctx 32 32) '(255 255 255))))
  (ok "stroke has width (>=1px above centre)" (not (color-near (px ctx 32 31) '(255 255 255))))
  (ok "stroke does not bleed far off-line" (color-near (px ctx 32 40) '(255 255 255))))

;;; transforms: a translated + scaled rect lands where the CTM puts it
(let ((ctx (g:make-context 64 64)))
  (g:set-fill ctx '(0 0 0))
  (g:save ctx) (g:translate ctx 20 20) (g:scale ctx 2 2)
  (g:fill-rect ctx 0 0 5 5)            ; device rect 20,20 .. 30,30
  (g:restore ctx)
  (ok "transformed rect at mapped centre" (color-near (px ctx 25 25) '(0 0 0)))
  (ok "transformed rect clear before origin" (color-near (px ctx 15 15) '(255 255 255))))

;;; fillText through scribe puts ink down and advances measureText > 0
(let ((ctx (g:make-context 200 48)))
  (g:set-fill ctx '(0 0 0)) (g:set-font ctx 28)
  (let ((before (ink ctx)))
    (g:fill-text ctx "Ag" 8 34)
    (ok "fillText draws glyph ink" (> (ink ctx) (+ before 30)))
    (ok "measureText advance positive" (> (g:measure-text ctx "Ag") 0d0))))

;;; drawImage blits a source canvas
(let ((ctx (g:make-context 40 40)) (src (s:make-canvas 10 10 '(10 200 10))))
  (g:draw-image ctx src 5 5 20 20)
  (ok "drawImage blits source colour" (color-near (px ctx 15 15) '(10 200 10))))

;;; a horizontal linear gradient interpolates left->right across the fill
(let ((ctx (g:make-context 100 20)))
  (let ((grad (g:make-linear-gradient 0 0 100 0)))
    (g:add-color-stop grad 0d0 '(255 0 0))
    (g:add-color-stop grad 1d0 '(0 0 255))
    (g:set-fill ctx grad)
    (g:fill-rect ctx 0 0 100 20))
  (ok "gradient left stop is red"   (color-near (px ctx 2 10) '(255 0 0)))
  (ok "gradient midpoint is purple" (color-near (px ctx 50 10) '(128 0 128)))
  (ok "gradient right stop is blue" (color-near (px ctx 97 10) '(0 0 255))))

;;; a radial gradient runs centre -> edge
(let ((ctx (g:make-context 60 60)))
  (let ((grad (g:make-radial-gradient 30 30 0 30 30 30)))
    (g:add-color-stop grad 0d0 '(255 255 0))
    (g:add-color-stop grad 1d0 '(0 128 0))
    (g:set-fill ctx grad)
    (g:fill-rect ctx 0 0 60 60))
  (ok "radial centre is inner colour" (color-near (px ctx 30 30) '(255 255 0)))
  (ok "radial edge is outer colour"   (let ((c (px ctx 58 30)))   ; greenish, not the yellow centre
                                        (and (< (first c) 60) (> (second c) 90) (< (third c) 40)))))

;;; an RGBA canvas tracks alpha: a half-alpha fill, then clearRect punches a hole
(let ((ctx (g:make-context 20 20 :alpha t)))
  (g:set-fill ctx '(255 0 0)) (g:set-global-alpha ctx 0.5d0)
  (g:fill-rect ctx 0 0 20 20)
  (g:set-global-alpha ctx 1d0)
  (g:clear-rect ctx 5 5 10 10)
  (let* ((cv (g:context-canvas ctx)) (ap (s:canvas-alpha cv)))
    (ok "rgba half-alpha fill records ~50% alpha" (near (aref ap 0) 128 8))
    (ok "clearRect zeroes alpha" (= (aref ap (+ (* 10 20) 10)) 0))))

(format t "~&gesso self-test: ~a passed, ~a failed~%" *pass* *fail*)
(when (plusp *fail*) (uiop:quit 1))
