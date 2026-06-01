# CRAN submission comments - dragmapr 0.1.0

## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: ubuntu-latest (R devel, release, oldrel-1), macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

Local `R CMD check --as-cran --no-manual` reports:

- `checking CRAN incoming feasibility ... NOTE`
- `New submission`

This is expected for the first CRAN submission. GitHub Actions are configured
in `.github/workflows/R-CMD-check.yaml` to run the same package check matrix on
Ubuntu devel/release/oldrel-1, macOS release, and Windows release.

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

## Downstream dependencies

None (first submission).
