#' Write a draggable map prototype HTML file
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for label text. Defaults to `region_col`.
#' @param file Output HTML path.
#'
#' @return Invisibly returns `file`.
#' @export
drag_map_prototype <- function(x, region_col, label_col = region_col, file = "drag-map.html") {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  if (!label_col %in% names(x)) {
    stop("label_col '", label_col, "' not found.", call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop("Project `x` before using the prototype.", call. = FALSE)
  }

  tmp <- tempfile(fileext = ".geojson")
  export <- x
  export$drag_region <- as.character(export[[region_col]])
  sf::st_write(export, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  labels <- make_labels(x, region_col = region_col, label_col = label_col)

  template <- system.file("prototype", "index.html", package = "dragmapr", mustWork = TRUE)
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  geojson <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  html <- sub("__GEOJSON__", jsonlite::toJSON(geojson, auto_unbox = TRUE), html, fixed = TRUE)
  html <- sub("__LABELS__", jsonlite::toJSON(labels, dataframe = "rows", auto_unbox = TRUE), html, fixed = TRUE)

  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}
