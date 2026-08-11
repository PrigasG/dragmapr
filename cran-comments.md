# CRAN submission comments - dragmapr 0.3.1

## API naming cleanup

- Renamed all exported functions that began with the redundant `dragmapr_`
  prefix to a consistent `d_` prefix. The old names are no longer exported;
  this intentional API cleanup is documented prominently in `NEWS.md`.
- Persisted S3 class names and the state/project schemas were not renamed, so
  JSON state files and project bundles written by earlier versions remain
  readable.
- Updated package code, tests, examples, vignettes, pkgdown configuration, the
  RStudio addin binding, Pipeline Studio, and the installed HTML/PDF cheatsheet
  to use the new names.

## Pipeline Studio compatibility

- Declared the app's `explodemap >= 0.4.0` requirement and added an early,
  actionable version check. The GitHub Actions matrix installs the matching
  `explodemap` development release while these coordinated releases are being
  prepared; `explodemap` 0.4.0 must be available before this dragmapr release.
- Reviewed the bundled Pipeline Studio after dependency updates exposed a
  deferred Shiny reactive-context error.
- Deferred layout builds, spatial uploads, vertex simplification, and saved
  state restores now execute their point-in-time reactive reads in an isolated
  context. Processing-overlay cleanup also isolates its internal reactive
  counter read.
- This prevents `Operation not allowed without an active reactive context`
  while preserving the app's asynchronous processing UI.
- Added a server-level regression test for deferred reactive reads and overlay
  cleanup. The complete local `testthat` suite passes with Shiny 1.12.1,
  `later` 1.4.5, and bslib 0.10.0.
- Follow-up testing found that repeated build events could queue synchronous
  layout jobs and keep the processing overlay visible for the entire queue.
  Pipeline Studio now permits one active job, disables the build action while
  busy, and declines duplicate work.
- Label-aware parameter search now falls back to the standard layout above
  1,000 features or 50 parent groups. This prevents very large municipal
  layers from spending an unbounded-feeling amount of time scoring candidate
  layouts, and the app tells the user when the safeguard is applied.

## Spatial composition and hierarchy contracts

- Added explicit composition of algorithmic base, inherited ancestor, and
  manual editorial offsets, with projected `sf` materialization through the
  existing state application API.
- Formalized generic hierarchy relationships and pure hierarchy operations;
  no Census, state-specific, or Shiny UI policy was moved into the package.
- Added optional keyed feature styles and migrated saved state to schema 1.2.0.
  Older state JSON remains readable through the existing migration path.
- Reduced Shiny traffic during dragging by making full-state emission default
  to drag completion, with opt-in compact throttled/continuous live events.
- Extended project bundles with optional hierarchy and style artifacts plus
  package/schema/CRS/geometry provenance.

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
