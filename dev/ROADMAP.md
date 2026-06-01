# dragmapr — next update roadmap

> This file is a development planning note. It is not shipped with the package.
> Updated 2026-05-29 — Priorities 1, 2, and 3 completed; only backlog remains.

---

## Priority 1 — must do before next release (0.1.1)

### Tests for features added in 0.1.0

The test suite was not updated to cover three things changed in 0.1.0:

- **Natural sort** — `natural_sort()` and `natural_sort_key_r()` are internal helpers
  used by `make_region_labels()`, `render_dragged_map()`, and the D3 template.
  Add a unit test in `test-labels.R` that confirms "1", "2", ..., "10" come out
  in numeric order, not lexicographic ("1", "10", "2", ...).

- **`as_drag_annotations()` connector fix** — the bug was that the `connector`
  argument was silently ignored when labels came from `make_region_labels()`.
  Add a test: create labels via `make_region_labels()`, pass them to
  `as_drag_annotations(connector = TRUE)`, and assert that all rows in the result
  have `connector == TRUE`.

- **`dragmapr_iframe_bridge(iframe_selector)`** — the new parameter was added to
  `spatial_io.R` but `test-spatial-io.R` does not check it. Add a test that the
  generated JS string contains the selector literal when a custom selector is
  supplied, and that an empty-string selector throws a clear error.

### Fix duplicate `.gitattributes` entries

The `.gitattributes` file currently has every LFS pattern twice (lines 3–9 and
10–15 are identical). Remove the duplicate block — Git processes the file
line-by-line and the duplicates are harmless but confusing.

### Tidy NEWS.md for the 0.1.0 release

The current `NEWS.md` mixes proper release notes with development-history prose
sections that were written incrementally. Before tagging 0.1.0, consolidate the
entries under a single `# dragmapr 0.1.0` header with the standard CRAN format
(one bullet per user-visible change, no internal headings, no history sections).

---

## Priority 2 — UX improvements (0.1.1 or 0.2.0)

### Legend position in the Drag pane when the helper panel is visible

The legend in the D3 iframe is positioned absolutely (CSS) at the edges of the
`<main>` container. Positions `"right"` and `"top"` use `right: 14px`, which
lands inside the 340px aside panel when `side_panel = TRUE`. Either:
- Adjust the CSS to account for the aside width:
  `.drag-legend.right { right: calc(340px + 14px); }`
  and expose a fallback via a `html.no-side-panel` selector.
- Or document that "left" and "bottom" are the safe positions when the side
  panel is shown (the studio already defaults to "bottom").

### Separate the HuggingFace README from the package README

`README.md` currently carries the HuggingFace YAML frontmatter (required for HF
Spaces) at the very top, which causes it to be lost whenever anyone edits and
re-saves the file without thinking about it. Better options:
- Use a separate `README_HF.md` maintained by a small `deploy.R` script that
  prepends the YAML and pushes to the HF remote.
- Or automate the prepend in a GitHub Action that runs only when pushing to the
  `space` remote.

### Squiggle amplitude is projection-dependent

`connector_squiggle_amplitude` defaults to `12000` (metres) in
`render_dragged_map()`. For data projected in large CRS units (e.g. State Plane
in feet) or very small geometry, this can look wildly wrong. Consider computing
a smart default from the plot extent (similar to how `connector_end_gap` is
auto-computed from the connector span), or at least document the projection
dependency more prominently.

---

## Priority 3 — CRAN preparation (0.2.0)

### Verify all `Suggests` packages are on CRAN

All current `Suggests` packages (`glasstabs`, `knitr`, `pkgdown`, `rmarkdown`,
`shiny`, `shinyWidgets`, `testthat`) are on CRAN. Confirm this holds before
submission with `available.packages()[, "Package"]`.

### Write `cran-comments.md`

Standard file expected by CRAN for first submissions. Should document:
- Local R CMD check results (no errors, no warnings, 0 or explained notes).
- Any `\dontrun{}` examples and why they are skipped.
- Downstream dependencies (none yet).
- Confirmation that the package has been tested on Windows, macOS, and Linux.

### R CMD check clean pass

Run `devtools::check(remote = TRUE, manual = TRUE)` and resolve any remaining
NOTEs:
- Confirm no undeclared global variable warnings from the D3 template file.
- Check that all `\dontrun{}` examples in Rd files are justifiable.
- Verify `inst/prototype/d3.v7.min.js` size does not exceed CRAN limits (5 MB
  single file; confirm size is acceptable).

### Version and DESCRIPTION

- The package is at 0.1.0. Before first CRAN submission bump to a clean `1.0.0`
  or leave at `0.1.0` — either is acceptable; just decide.
- Review `Authors@R` and `Description` field for CRAN policy compliance (no
  leading "A package" in Description, person roles correct).

---

## Backlog / nice to have

- **Mobile layout for the Spatial Studio** — the sidebar is `width = 2` Bootstrap
  columns, which collapses badly on phones. Not critical since the primary users
  are desktop analysts, but worth a note.

- **`render_dragged_map()` circle marker coordinate alignment** — the static
  ggplot circle (via `geom_point`) uses the label's projected centroid, while
  the D3 helper positions circles at the same coordinate. They should match, but
  worth a visual spot-check on a real-world dataset to confirm the coordinates
  line up after region offsets are applied.

- **`CONTRIBUTING.md` and `CODE_OF_CONDUCT.md`** — standard open-source files.
  Low priority until there are external contributors.

- **pkgdown articles page** — consider adding a short "Deploying to HuggingFace"
  article (the deployment guide already exists as a Word document; it could be
  converted to an Rmd vignette in `vignettes/articles/`).

---

## Done in 0.1.0 (for reference)

The changes below are already shipped; this section is here so the roadmap has
context for what was just completed.

- Fixed `as_drag_annotations()` silently dropping the `connector` argument.
- Fixed all iframe postMessage handlers in the studio to target
  `iframe.studio-helper-frame` rather than the first `<iframe>` in the document.
- Loading veil now dismissed by `dragmapr-ready` postMessage after D3 render,
  not by the iframe `load` event (which races with listener setup).
- `dragmapr_iframe_bridge()` gains `iframe_selector` parameter.
- Natural / numeric sort applied consistently in R (`make_region_labels`,
  `render_dragged_map`) and in D3 (`naturalCompare`).
- Label sliders consolidated: only relevant controls shown per label type;
  `label_text_size` added as a universal text-size slider.
- `connectorLayer.raise()` removed from D3 template — it was inverting the
  stacking order so connectors drew over label text.
- All CRS error messages now mention `prepare_dragmapr_sf()` and
  `sf::st_transform()`.
- `@seealso` cross-links added across the five exported function families.
- pkgdown home page: auto-inserted large logo suppressed via `pkgdown/extra.css`.
- Getting-started vignette expanded to cover all 22 parameters of
  `drag_map_prototype()`.
- Vignettes and examples audited and synced with current API.
