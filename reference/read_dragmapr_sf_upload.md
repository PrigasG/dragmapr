# Read an sf object from a Shiny file upload

Handles the data frame returned by
[`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html),
including multi-file uploads of shapefile sidecars (`.shp`, `.dbf`,
`.shx`, `.prj`) and zipped archives containing any supported format.
When `upload` is `NULL` or empty the function returns `NULL` so callers
can fall back to demo data.

## Usage

``` r
read_dragmapr_sf_upload(upload)
```

## Arguments

- upload:

  The value of `input$<id>` from a
  [`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html)
  widget. Each row is one uploaded file with `name` and `datapath`
  columns. When `NULL` or a zero-row data frame, returns `NULL`.

## Value

An `sf` object, or `NULL` if `upload` is `NULL` or empty.

## Examples

``` r
# Typical Shiny server usage:
if(interactive()){
observeEvent(input$spatial_upload, {
  x <- read_dragmapr_sf_upload(input$spatial_upload)
  if (!is.null(x)) {
    state$source <- x
  }
})
}
```
