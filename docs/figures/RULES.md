# Figure rules (v2): performance-worklog Excalidraw style

These are the house rules for the diagrams in
[`docs/reflections.md`](../reflections.md). They extend the original
README-figure rules in [`gen_figures.py`](gen_figures.py): the README figures
stay minimal and explanatory, while reflection figures are allowed to be
data-rich, comparative, and annotated like a performance worklog (the style of
Hamza Elshafie's GEMM worklog,
<https://hamzaelshafie.bearblog.dev/worklog-optimising-gemm-on-nvidia-h100-for-cublas-like-performance-wip/>,
is the reference: every diagram makes one argument and carries its own
evidence).

The generator is the source of truth. Never hand-edit a `.excalidraw` scene:
edit `gen_figures.py`, run it, and render every scene with the Excalidraw CLI.

```console
$ python3 docs/figures/gen_figures.py
$ EXCALIDRAW_BROWSER_PATH=/nix/store/...-chromium/bin/chromium \
    excalidraw render docs/figures/optimization-cost.excalidraw \
    --out docs/figures/optimization-cost.png --scale 2 --max-width 3200
```

## Canvas and typography

1. Default scene is 1520 x 940 with a 40-unit margin. Use 1680 x 1060 only for
   a "hero" figure that needs two stacked arguments.
2. Type scale: title 32, section 24, body 22, math 22, note 18. Never go below
   18 in a rendered scene.
3. Title states the claim of the figure, not the topic. "Direct pack retention
   removes the loose-object round trip" beats "Clone pipeline".
4. Hand-drawn face only (font family 1), roughness 2, hachure fill, stroke
   width 2. Code snippets are drawn as short notes in a `code` box; no
   monospace font is used because the render probe does not guarantee it.

## Colour semantics

Fill is semantic, never decorative. The palette is the README palette plus
performance roles:

| role | meaning in reflection figures |
| --- | --- |
| `risk` (red) | old/removed/slow path |
| `hip` (green) | new/fast path |
| `compiler` (blue) | network / transport work |
| `runtime` (violet) | decoding / parsing / hashing |
| `analysis` (yellow) | measured numbers, math, evidence |
| `model` (teal) | store / cache / data structure |
| `neutral` (grey) | repository on disk, annotations |
| `code` (dark) | code paths and module tags |

Colour is never the only signal: every colour difference is also labelled
("before", "after", "42.4 MiB", "24.3 s"). The palette is colour-blind safe as
a set because the labels carry the distinction.

## Composition rules

1. One figure, one claim. If you need a second claim, draw a second figure.
2. Reading order is left-to-right, top-to-bottom. Arrows never cross.
3. Every arrow is labelled with the quantity or verb it carries (`175 MiB`,
   `N loose writes`, `applyDelta`, `one pass`).
4. Comparative figures use paired lanes (before/after) or stacked bars on a
   shared scale; never put two time axes on one bar without labelling the
   zoom factor.
5. Measured values are written as `number unit` and are never rounded away
   from the source table in reflections.md. Cite the measurement in the
   caption.
6. Every figure carries at least one equation or measured number, placed next
   to the element it explains with a short annotation arrow.
7. Module tags (`Retain.hs`, `Clone.hs`, `Transport.hs`) sit as small notes
   above the boxes they describe, so the diagram can be read as an annotated
   call graph.
8. "Churn" (file writes, seeks, unlinks, allocations) is drawn as a row of
   small repeated cells with an ellipsis, not as a single box. This makes the
   `O(N)` work visible.
9. Intuition panels are allowed once per figure (e.g. a queue of loose files
   being shuttled to disk and back). The panel is never the evidence: the
   measured bar or equation is.
10. Curves are drawn as connected straight segments from a small data table in
    the generator; axis labels carry the units and the exponent (`O(N^2)`,
    `O(N)`).

## Text and glyph whitelist

The render probe verifies a glyph set in the hand-drawn face. Reflection
figures may use the README glyph set

`‖ ⊕ ∈ ⌊ ⌋ ≤ ≠ ∅ ≈ → ↦ Δ Σ ≡ −`

plus the extended set added for the worklog figures:

`↑ ↓ × · ² ³ ⁻ √ ∞ ¬ ∑ ∏`

The generator asserts the whitelist before saving; if the render probe rejects
a glyph, remove it from `EXTENDED_GLYPHS` in `gen_figures.py` and note the
change here. ASCII stays ASCII: use `^` for powers in equations (`O(N^2)`) and
`mod 2^32` rather than a `%` operator, which the hand face renders poorly.

## Captions

Every figure in `reflections.md` gets a one-sentence informational caption: it
states what the reader should learn from the figure, not what the figure
depicts. Captions may reference the corresponding benchmark row or code module
with a relative link.

## Declarative mirror in the NixOS config

The same rules are exported declaratively from `~/nixos/agents/excalidraw.nix`
(imported by `agent-settings.nix`): the `instructions` string carries the
reflection-figure section and glyph whitelist, `palette` carries the semantic
role aliases (`before`, `after`, `store`, `math`, `measure`, `network`,
`code`), `defaults.reflection` carries the reflection type scale and
`extendedGlyphs`, and `checklist` carries the v2 evidence checks. When these
rules change, update that file in the same change so the agent instructions
and the repository generator stay in sync.

## Reproducibility

1. Scene geometry is generated; the `.excalidraw` JSON and the `.png` are
   committed together.
2. The generator prints every scene path. Rendering is a separate CLI step;
   check that every `.excalidraw` has a matching `.png` before committing.
3. Figures used by the README keep their original four-scene layout so the
   README diff stays small; reflection figures live in their own scene names.
