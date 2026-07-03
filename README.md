# gesso

**2D vector graphics in pure Common Lisp.** Clean-room — no FFI, no Cairo, no
Skia. Gesso is the primer coat a painter lays down before the image; this gesso
is the drawing surface beneath a rendered graphic.

gesso turns path and text drawing commands into antialiased, gamma-correct
pixels. It builds paths (lines, quadratic/cubic Béziers, arcs, ellipses), fills
them with non-zero winding, strokes them, draws text, and blits images — all
composited on [`scribe`](../scribe)'s analytic-coverage rasterizer and
linear-light compositor. It is the drawing engine behind an HTML `<canvas>` 2D
context, and the rendering backend the [`stencil`](../stencil) SVG library paints
through.

```
scribe  (font + raster + compositing)
   |
 gesso  (2D vector graphics: paths, fill, stroke, text, images)
   |
stencil (SVG parse + DOM + render)
```

## Layering

gesso depends only on scribe. It reuses:

- **`scribe:rasterize-outline`** — scan-convert arbitrary `move`/`line`/`quad`/
  `cubic` contours to a coverage bitmap (signed-area accumulation, the same
  kernel that rasterizes glyph outlines).
- **`scribe:blend-coverage`** — composite a coverage value onto a pixel in
  linear light.
- **`scribe:match-font` / `shape-run` / `rasterize-glyph`** — shaped, kerned text
  for `fillText`.
- **`scribe:canvas` / `make-canvas` / `write-png`** — the RGB8 surface.

A gesso context can wrap an existing scribe canvas (`make-context … :canvas cv`),
so a host renders a page and its canvases onto one shared buffer.

## Model

Coordinates are recorded in **device space at the moment they are added**: every
`move-to`/`line-to`/curve maps through the current transform immediately and
stores a device point, exactly as the canvas spec requires. Curves and arcs are
flattened to line segments on the way in, so the rasterizer only ever sees
polylines. scribe's rasterizer is authored for y-up glyph outlines and flips y on
output; a canvas is y-down, so gesso feeds negated y with a matching origin,
un-flipping it so coverage row 0 lands at the top of a shape's bounding box.

## The HTML canvas 2D mapping

Each `CanvasRenderingContext2D` member maps to one gesso primitive. A host
(weft) binds the JS surface onto these:

| Canvas 2D | gesso |
|---|---|
| `save()` / `restore()` | `save` / `restore` (state stack) |
| `translate/scale/rotate/transform/setTransform` | `translate` `scale` `rotate` `transform` `set-transform` |
| `fillStyle` / `strokeStyle` | `set-fill` / `set-stroke` (r g b) |
| `lineWidth`, `globalAlpha`, `font` | `set-line-width`, `set-global-alpha`, `set-font` |
| `beginPath()` | `begin-path` |
| `moveTo`/`lineTo` | `move-to` / `line-to` |
| `quadraticCurveTo`/`bezierCurveTo` | `quadratic-curve-to` / `bezier-curve-to` |
| `arc`/`ellipse`/`rect`/`closePath` | `arc` / `ellipse` / `rect` / `close-path` |
| `fill()` / `stroke()` | `fill-path` / `stroke-path` |
| `fillRect`/`strokeRect`/`clearRect` | `fill-rect` / `stroke-rect` / `clear-rect` |
| `fillText`/`strokeText`/`measureText` | `fill-text` / `stroke-text` / `measure-text` |
| `drawImage` | `draw-image` |
| `toDataURL()` | `write-png` → base64 (host) |

Colours here are `(r g b)` 8-bit lists; a host resolves CSS colour syntax and
CSS `font` shorthand before calling in.

## What works (first slice)

- Affine transforms with a save/restore stack.
- Path building: lines, quadratic + cubic Béziers, arcs, ellipses, rects.
- Non-zero-winding **fill** of arbitrary paths (analytic AA via scribe).
- **Stroke**: per-segment offset rectangles + vertex squares unioned under
  non-zero fill (butt caps, approximate joins).
- `fill-rect` / `stroke-rect` / `clear-rect`.
- **`fill-text`** via scribe shaping + glyph coverage; `measure-text`.
- Nearest-neighbour `draw-image` (translate + scale).
- Self-test (`inspect/self-test.lisp`): 12 checks, all green.

## Deferred

- Gradients (linear/radial/conic) and patterns as paint sources.
- Even-odd fill rule; precise miter/round/bevel joins and line caps; dashing.
- Clipping paths (`clip()`); global composite operations; shadows.
- Full-matrix (rotated/skewed) text and image sampling (bilinear); an alpha
  channel for true `clearRect` transparency.
- `getImageData`/`putImageData`.

## Run the tests

```
sbcl --script inspect/self-test.lisp
```

Pure Common Lisp. No FFI.
