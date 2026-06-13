# CRAN submission comments - dragmapr 0.2.0

## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 1 note

Local `R CMD check --as-cran` reports:

- `checking CRAN incoming feasibility ... NOTE`
- `New submission`


## Notes on bundled files

`inst/prototype/d3.v7.min.js` (274 KB) - the D3 v7 minified library, included
to make the generated drag-map HTML files self-contained and usable offline.
The file is the standard unmodified D3 v7 distribution from <https://d3js.org>.

## Notes on interactive examples

- Browser-opening examples use `if(interactive())` so users can see the intended
  workflow without launching a browser during `R CMD check`.
- Shiny examples and the RStudio addin are also guarded with
  `if(interactive())`; non-interactive package checks exercise the reusable
  helper functions and addin registration instead.
- Network examples are guarded with `if(interactive())` to avoid failures on
  machines without internet access.

## Downstream dependencies

None.
