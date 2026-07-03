;;;; gesso.asd — 2D vector graphics in pure Common Lisp.
(asdf:defsystem :gesso
  :description "2D vector graphics in pure Common Lisp: affine transforms, path
building (lines/beziers/arcs), non-zero fill, stroking (offset-outline), text via
scribe, and image blitting — composited on scribe's analytic-coverage rasterizer.
The drawing engine behind an HTML canvas 2D context.  No FFI, no Cairo, no Skia."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("scribe")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "matrix")     ; 2D affine transforms (a b c d e f)
     (:file "paint")      ; gradient paint sources (linear/radial)
     (:file "path")       ; path building + curve/arc flattening
     (:file "raster")     ; contour -> coverage -> canvas (fill blitter)
     (:file "stroke")     ; stroke a polyline into a fill outline
     (:file "text")       ; fillText through scribe shaping + glyph raster
     (:file "context"))))) ; the stateful 2D context (the canvas API surface)
