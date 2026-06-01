# CRAN submission comments - dragmapr 0.1.0

## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

Local `R CMD check --as-cran --no-manual` reports:

- `checking CRAN incoming feasibility ... NOTE`
- `New submission`

This is expected for a first CRAN submission. GitHub Actions run the same check
matrix on Ubuntu devel/release/oldrel-1, macOS release, and Windows release.
On the local Windows machine, `R CMD check --as-cran --no-manual` can also
report `unable to verify current time`; this is a local check-environment note.

## Notes on bundled files

`inst/prototype/d3.v7.min.js` (274 KB) - the D3 v7 minified library, included
to make the generated drag-map HTML files self-contained and usable offline.
The file is the standard unmodified D3 v7 distribution from <https://d3js.org>.

## Notes on \dontrun{} examples

- `drag_map_prototype(..., open = TRUE)` - opens a file in the default browser;
  skipped in examples to avoid browser interaction during `R CMD check`.
- `read_dragmapr_sf_url()` - requires a network connection; skipped to avoid
  check failures on machines without internet access.
- `read_dragmapr_sf_upload()` - requires a live Shiny session; skipped because
  it depends on the `shiny::fileInput()` reactive environment.
- `dragmapr_addin()` - launches an interactive Shiny gadget; the function body
  is not exercised during `R CMD check`. The addin registration is verified via
  a `testthat` test that reads `inst/rstudio/addins.dcf`.

## Downstream dependencies

None (first submission).
