# Canvas 2D on gesso — design

How an HTML `<canvas>` 2D context is served by gesso, and the road from the
current first slice to full coverage.

## Binding surface (host = weft)

`HTMLCanvasElement.getContext('2d')` returns a `CanvasRenderingContext2D` whose
methods and properties are thin shims over a `gesso:context`:

1. The element's `width`/`height` (or the 300×150 default) create a
   `gesso:make-context`. Its scribe canvas may be the page canvas (to draw the
   `<canvas>` in place) or a private buffer (for off-screen use / `toDataURL`).
2. Property setters translate CSS syntax to gesso's native forms:
   - `fillStyle`/`strokeStyle` colour strings → `(r g b)` via weft's CSS colour
     parser; gradient/pattern objects → gesso paint sources (deferred).
   - `font` shorthand → size px + family list + weight + style via weft's font
     resolver → `gesso:set-font`.
   - `lineWidth`, `globalAlpha`, transform ops → the matching gesso setter.
3. Method calls forward op-for-op (see the table in the README).
4. `toDataURL()` = `gesso:write-png` to an octet stream → base64 `data:` URL.

Because a context can wrap an existing scribe canvas, an inline `<canvas>` paints
directly onto the page buffer at its layout box; nothing special is needed to
composite it with the rest of the page.

## Op semantics

- **State**: `save`/`restore` copy/pop the whole drawing state (transform,
  colours, line width, alpha, font). Path state is *not* part of the stack, per
  spec.
- **Paths**: coordinates are baked to device space through the current transform
  when added. Curves/arcs flatten to line segments at record time; the rasterizer
  sees only polylines. `fill` unions all subpaths under non-zero winding.
- **Fill/stroke** run through `scribe:rasterize-outline` → coverage →
  `scribe:blend-coverage`, so antialiasing is analytic and compositing is
  gamma-correct, matching text.
- **Text** shapes with scribe (kerning + ligatures) and rasterizes each glyph;
  the transform contributes the origin (translation) and ppem (mean scale).
- **Images** blit a source scribe canvas; the first slice is nearest-neighbour
  under translate + scale.

## Roadmap

| Area | First slice | Next |
|---|---|---|
| Paths | line/quad/cubic/arc/ellipse/rect, non-zero fill | even-odd rule, `arcTo`, `roundRect` |
| Stroke | offset rectangles + vertex squares | miter/round/bevel joins, caps, dash |
| Paint | solid `(r g b)` | linear/radial/conic gradients, patterns |
| Text | `fillText`, `measureText` | `textAlign`/`textBaseline`, stroked glyph outlines |
| Images | nearest, translate+scale | bilinear, full matrix, `getImageData`/`putImageData` |
| Compositing | source-over, `globalAlpha` | `globalCompositeOperation`, shadows, `clip()` |

Each step stays additive on the scribe rasterizer + compositor; none needs FFI.
