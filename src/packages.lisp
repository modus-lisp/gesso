;;;; packages.lisp — gesso package definition.
(defpackage #:gesso
  (:use #:cl)
  (:export
   ;; context lifecycle
   #:context #:make-context #:context-canvas #:context-width #:context-height
   #:write-png
   ;; state stack + drawing state
   #:save #:restore
   #:set-fill #:set-stroke #:set-line-width #:set-global-alpha #:set-font #:current-fill
   #:set-fill-rule #:clip #:set-line-dash #:set-blend-mode #:set-soft-mask
   ;; transforms
   #:translate #:scale #:rotate #:transform #:set-transform #:reset-transform
   ;; path building
   #:begin-path #:move-to #:line-to #:quadratic-curve-to #:bezier-curve-to
   #:arc #:arc-to #:ellipse #:rect #:close-path
   ;; painting
   #:fill-path #:stroke-path #:fill-rect #:stroke-rect #:clear-rect #:fill-callback
   #:fill-text #:stroke-text #:measure-text #:draw-image #:draw-image-rgba
   ;; paint sources (paint.lisp)
   #:gradient #:gradient-p #:make-linear-gradient #:make-radial-gradient
   #:add-color-stop #:gradient-color-at #:paint->solid #:path-user-bounds
   ;; affine helpers (matrix.lisp)
   #:mat-identity #:mat-mul #:mat-apply #:mat-translate #:mat-scale #:mat-rotate
   #:mat-invert))
