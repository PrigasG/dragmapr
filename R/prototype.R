#' Write a draggable plot prototype HTML file
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for default region-label text. Defaults to
#'   `region_col`.
#' @param labels Show draggable labels. Use `TRUE` to derive one label per
#'   region, `FALSE` to omit labels, or a data frame accepted by
#'   [as_drag_labels()] for user-supplied labels.
#' @param draggable_labels Allow labels to be dragged independently of regions.
#' @param label_marker Draw circular markers behind labels.
#' @param label_radius Label marker radius in screen pixels.
#' @param label_text_size Label text size in screen pixels.
#' @param label_box_width,label_box_height Default browser dimensions for
#'   draggable annotation boxes.
#' @param connector_linewidth Browser connector line width in pixels.
#' @param region_offsets Optional data frame with `region`, `dx_m`, and `dy_m`
#'   columns used to initialize region positions in the browser helper.
#' @param label_offsets Optional data frame with `label_id`, `region`, `dx_m`,
#'   and `dy_m` columns used to initialize label positions in the browser
#'   helper.
#' @param region_palette Optional named character vector mapping region values
#'   to hex color strings (e.g. `c(North = "#2166ac", South = "#d73027")`).
#'   When supplied the D3 helper uses these colors instead of its built-in
#'   palette, so the interactive colors match a [render_dragged_map()] call
#'   that uses the same palette.
#' @param file Output HTML path.
#' @param open Open the written file in the default browser via
#'   [utils::browseURL()]. Defaults to `FALSE`.
#'
#' @return Invisibly returns `file`.
#' @export
#' @examples
#' poly <- sf::st_sf(
#'   region = c("A", "B"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     crs = 3857
#'   )
#' )
#'
#' # Primary usage: open the interactive draggable plot in the browser.
#' # Drag regions and labels, then copy or download the offset CSVs.
#' \dontrun{
#' drag_map_prototype(poly, region_col = "region", open = TRUE)
#' }
#'
#' # Write to a specific path without opening (e.g. for Shiny or CI).
#' drag_map_prototype(poly, region_col = "region",
#'                   file = tempfile(fileext = ".html"))
drag_map_prototype <- function(x,
                               region_col,
                               label_col = region_col,
                               labels = TRUE,
                               draggable_labels = TRUE,
                               label_marker = TRUE,
                               label_radius = 12,
                               label_text_size = 11,
                               label_box_width = 150,
                               label_box_height = 72,
                               connector_linewidth = 1.3,
                               region_offsets = NULL,
                               label_offsets = NULL,
                               region_palette = NULL,
                               file = "drag-map.html",
                               open = FALSE) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  if (!identical(labels, FALSE) && !is.data.frame(labels) && !label_col %in% names(x)) {
    stop("label_col '", label_col, "' not found.", call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop("Project `x` before using the prototype.", call. = FALSE)
  }
  if (!(is.logical(labels) && length(labels) == 1L && !is.na(labels)) && !is.data.frame(labels)) {
    stop("`labels` must be TRUE, FALSE, or a label data frame.", call. = FALSE)
  }
  if (!is.logical(draggable_labels) || length(draggable_labels) != 1L || is.na(draggable_labels)) {
    stop("`draggable_labels` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(label_marker) || length(label_marker) != 1L || is.na(label_marker)) {
    stop("`label_marker` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(label_radius) || length(label_radius) != 1L || !is.finite(label_radius) || label_radius < 0) {
    stop("`label_radius` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(label_text_size) || length(label_text_size) != 1L || !is.finite(label_text_size) || label_text_size <= 0) {
    stop("`label_text_size` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(label_box_width) || length(label_box_width) != 1L || !is.finite(label_box_width) || label_box_width <= 0) {
    stop("`label_box_width` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(label_box_height) || length(label_box_height) != 1L || !is.finite(label_box_height) || label_box_height <= 0) {
    stop("`label_box_height` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(connector_linewidth) || length(connector_linewidth) != 1L || !is.finite(connector_linewidth) || connector_linewidth <= 0) {
    stop("`connector_linewidth` must be a positive number.", call. = FALSE)
  }
  if (!is.null(region_offsets) && !is.data.frame(region_offsets)) {
    stop("`region_offsets` must be a data frame when supplied.", call. = FALSE)
  }
  if (!is.null(label_offsets) && !is.data.frame(label_offsets)) {
    stop("`label_offsets` must be a data frame when supplied.", call. = FALSE)
  }
  if (!is.null(region_palette) && (is.null(names(region_palette)) || any(names(region_palette) == ""))) {
    stop("`region_palette` must be a named vector when supplied.", call. = FALSE)
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }

  tmp <- tempfile(fileext = ".geojson")
  export <- x
  export$drag_region <- as.character(export[[region_col]])
  sf::st_write(export, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
               layer_options = "RFC7946=NO")
  geojson_text <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  label_data <- if (isTRUE(labels)) {
    make_region_labels(x, region_col = region_col, label_col = label_col)
  } else if (is.data.frame(labels)) {
    as_drag_labels(labels)
  } else {
    data.frame(label_id = character(), region = character(), label = character(), x = numeric(), y = numeric())
  }
  region_offset_data <- if (is.null(region_offsets)) {
    data.frame(region = character(), dx_m = numeric(), dy_m = numeric())
  } else {
    normalize_offsets(region_offsets, source = "`region_offsets`")
  }
  label_offset_data <- if (is.null(label_offsets)) {
    data.frame(label_id = character(), region = character(), dx_m = numeric(), dy_m = numeric())
  } else {
    normalize_label_state(label_offsets, source = "`label_offsets`")
  }
  options <- list(
    labels = !identical(labels, FALSE),
    draggableLabels = isTRUE(draggable_labels),
    labelMarker = isTRUE(label_marker),
    labelRadius = unname(label_radius),
    labelTextSize = unname(label_text_size),
    labelBoxWidth = unname(label_box_width),
    labelBoxHeight = unname(label_box_height),
    connectorLinewidth = unname(connector_linewidth),
    regionPalette = if (is.null(region_palette)) NULL else as.list(region_palette)
  )

  template <- system.file("prototype", "index.html", package = "dragmapr", mustWork = TRUE)
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  html <- sub("__GEOJSON__", geojson_text, html, fixed = TRUE)
  html <- sub("__LABELS__", jsonlite::toJSON(label_data, dataframe = "rows", auto_unbox = TRUE), html, fixed = TRUE)
  html <- sub("__REGION_OFFSETS__", jsonlite::toJSON(region_offset_data, dataframe = "rows", auto_unbox = TRUE), html, fixed = TRUE)
  html <- sub("__LABEL_OFFSETS__", jsonlite::toJSON(label_offset_data, dataframe = "rows", auto_unbox = TRUE), html, fixed = TRUE)
  html <- sub("__OPTIONS__", jsonlite::toJSON(options, auto_unbox = TRUE), html, fixed = TRUE)

  writeLines(html, file, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(file)
  }
  invisible(file)
}
