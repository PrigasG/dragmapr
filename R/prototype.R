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
#' @param label_marker Draw a marker behind ordinary text labels. Set to
#'   `FALSE` for text-only draggable labels.
#' @param label_marker_shape Marker shape for ordinary draggable text labels:
#'   `"rect"` for rounded rectangles, `"circle"` for circles, or `"none"` for a
#'   transparent drag target with text only. If `label_marker = FALSE`,
#'   `label_marker_shape` is treated as `"none"`.
#' @param label_radius Retained for compatibility with earlier circle markers.
#'   Used when `label_marker_shape = "circle"`.
#' @param label_text_size Label text size in screen pixels.
#' @param label_width,label_height Default browser dimensions for ordinary
#'   draggable text-label markers.
#' @param label_box_width,label_box_height Default browser dimensions for
#'   draggable annotation boxes.
#' @param connector_color Browser label-connector color.
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
#' @param show_legend Show a compact region legend inside the browser helper.
#' @param max_legend_keys Maximum number of legend keys to show in the browser
#'   helper before suppressing the legend. Set to `Inf` to always show it.
#' @param legend_position Browser-helper legend position. One of `"bottom"`,
#'   `"top"`, `"left"`, `"right"`, or `"none"`.
#' @param legend_title Title shown above the browser-helper legend.
#' @param legend_values Optional character vector of region values to include
#'   in the browser-helper legend. `NULL` includes all region values.
#' @param label_values Optional character vector of label IDs to display.
#'   `NULL` displays all labels while preserving all label offsets.
#' @param map_background Browser helper background. One of `"white"`,
#'   `"transparent"`, `"light_grid"`, or `"dark"`.
#' @param connector_linetype Browser helper connector line style. One of
#'   `"solid"`, `"dashed"`, or `"dotted"`.
#' @param connector_endpoint Browser helper connector endpoint. One of
#'   `"none"` or `"arrow"`.
#' @param connector_smart Choose connector geometry dynamically in the browser
#'   based on label displacement. When `TRUE`, the helper chooses among the
#'   available connector paths instead of using each row's `connector_type`.
#' @param show_origin_outlines Show the original, unshifted outlines of regions
#'   with non-zero offsets beneath the moved regions.
#' @param show_movement_connectors Draw a connector from each moved region's
#'   original representative point to its current location. Hidden for zero-
#'   offset regions. Defaults to `FALSE`.
#' @param movement_connector_color,movement_connector_opacity,movement_connector_linewidth
#'   Browser styling for movement connectors.
#' @param movement_connector_linetype Movement connector line style. One of
#'   `"solid"`, `"dashed"`, or `"dotted"`.
#' @param movement_connector_endpoint Movement connector endpoint. One of
#'   `"none"`, `"open"`, or `"closed"`.
#' @param show_drag_trail Show a short fading trail of outline snapshots while
#'   a region is actively being dragged. The trail is cleared immediately when
#'   dragging ends and does not appear in static exports or project metadata.
#'   Defaults to `FALSE`.
#' @param side_panel Show the built-in copy/download side panel in the helper
#'   HTML. Defaults to `TRUE`; Shiny apps that provide their own controls can
#'   set this to `FALSE`.
#' @param file Output HTML path. When `NULL`, a temporary `.html` file is
#'   created. Pass an explicit path to save the helper somewhere durable.
#' @param open Open the written file in the default browser via
#'   [utils::browseURL()]. Defaults to `FALSE`.
#'
#' @return Invisibly returns `file`.
#' @seealso [render_dragged_map()] for the optional static ggplot2 render after
#'   dragging; [make_region_labels()] and [as_drag_annotations()] to build
#'   custom label tables; [prepare_dragmapr_sf()] to project uploaded geometry.
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
#' if(interactive()){
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
                               label_marker_shape = c("rect", "circle", "none"),
                               label_radius = 12,
                               label_text_size = 11,
                               label_width = 64,
                               label_height = 30,
                               label_box_width = 150,
                               label_box_height = 72,
                               connector_color = "#334155",
                               connector_linewidth = 1.3,
                               region_offsets = NULL,
                               label_offsets = NULL,
                               region_palette = NULL,
                               show_legend = FALSE,
                               max_legend_keys = 25L,
                               legend_position = c("bottom", "top", "left", "right", "none"),
                               legend_title = "Region",
                               legend_values = NULL,
                               label_values = NULL,
                               map_background = c("white", "transparent", "light_grid", "dark"),
                               connector_linetype = c("solid", "dashed", "dotted"),
                               connector_endpoint = c("none", "arrow"),
                               connector_smart = FALSE,
                               show_origin_outlines = FALSE,
                               show_movement_connectors = FALSE,
                               show_movement_band = FALSE,
                               movement_connector_color = "#64748b",
                               movement_connector_opacity = 0.72,
                               movement_connector_linewidth = 1.4,
                               movement_connector_linetype = c("solid", "dashed", "dotted"),
                               movement_connector_endpoint = c("closed", "open", "none"),
                               show_drag_trail = FALSE,
                               side_panel = TRUE,
                               file = NULL,
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
    stop(
      "Project `x` before using the prototype. ",
      "Use prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857) ",
      "to convert to a projected CRS first.",
      call. = FALSE
    )
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
  label_marker_shape <- match.arg(label_marker_shape)
  if (!isTRUE(label_marker)) {
    label_marker_shape <- "none"
  }
  if (!is.numeric(label_radius) || length(label_radius) != 1L || !is.finite(label_radius) || label_radius < 0) {
    stop("`label_radius` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(label_text_size) || length(label_text_size) != 1L || !is.finite(label_text_size) || label_text_size <= 0) {
    stop("`label_text_size` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(label_width) || length(label_width) != 1L || !is.finite(label_width) || label_width <= 0) {
    stop("`label_width` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(label_height) || length(label_height) != 1L || !is.finite(label_height) || label_height <= 0) {
    stop("`label_height` must be a positive number.", call. = FALSE)
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
  if (!is.character(connector_color) || length(connector_color) != 1L || is.na(connector_color) || !nzchar(connector_color)) {
    stop("`connector_color` must be a single color string.", call. = FALSE)
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
  if (!is.logical(show_legend) || length(show_legend) != 1L || is.na(show_legend)) {
    stop("`show_legend` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_legend_keys) || length(max_legend_keys) != 1L || is.na(max_legend_keys) || max_legend_keys < 0) {
    stop("`max_legend_keys` must be a non-negative number.", call. = FALSE)
  }
  legend_position <- match.arg(legend_position)
  map_background <- match.arg(map_background)
  connector_linetype <- match.arg(connector_linetype)
  connector_endpoint <- match.arg(connector_endpoint)
  movement_connector_linetype <- match.arg(movement_connector_linetype)
  movement_connector_endpoint <- match.arg(movement_connector_endpoint)
  if (identical(legend_position, "none")) {
    show_legend <- FALSE
  }
  if (!is.character(legend_title) || length(legend_title) != 1L || is.na(legend_title)) {
    stop("`legend_title` must be a single string.", call. = FALSE)
  }
  if (!is.null(legend_values) && !is.character(legend_values)) {
    stop("`legend_values` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(label_values) && !is.character(label_values)) {
    stop("`label_values` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.logical(connector_smart) || length(connector_smart) != 1L || is.na(connector_smart)) {
    stop("`connector_smart` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_origin_outlines) || length(show_origin_outlines) != 1L || is.na(show_origin_outlines)) {
    stop("`show_origin_outlines` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_movement_connectors) || length(show_movement_connectors) != 1L || is.na(show_movement_connectors)) {
    stop("`show_movement_connectors` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_movement_band) || length(show_movement_band) != 1L || is.na(show_movement_band)) {
    stop("`show_movement_band` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(movement_connector_color) || length(movement_connector_color) != 1L ||
      is.na(movement_connector_color) || !nzchar(movement_connector_color)) {
    stop("`movement_connector_color` must be a single color string.", call. = FALSE)
  }
  if (!is.numeric(movement_connector_opacity) || length(movement_connector_opacity) != 1L ||
      !is.finite(movement_connector_opacity) || movement_connector_opacity < 0 || movement_connector_opacity > 1) {
    stop("`movement_connector_opacity` must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(movement_connector_linewidth) || length(movement_connector_linewidth) != 1L ||
      !is.finite(movement_connector_linewidth) || movement_connector_linewidth <= 0) {
    stop("`movement_connector_linewidth` must be a positive number.", call. = FALSE)
  }
  if (!is.logical(show_drag_trail) || length(show_drag_trail) != 1L || is.na(show_drag_trail)) {
    stop("`show_drag_trail` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(side_panel) || length(side_panel) != 1L || is.na(side_panel)) {
    stop("`side_panel` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(file)) {
    file <- tempfile("drag-map-", fileext = ".html")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a single output path or NULL.", call. = FALSE)
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
    labelMarkerShape = label_marker_shape,
    labelRadius = unname(label_radius),
    labelTextSize = unname(label_text_size),
    labelWidth = unname(label_width),
    labelHeight = unname(label_height),
    labelBoxWidth = unname(label_box_width),
    labelBoxHeight = unname(label_box_height),
    connectorColor = unname(connector_color),
    connectorLinewidth = unname(connector_linewidth),
    regionPalette = if (is.null(region_palette)) NULL else as.list(region_palette),
    showLegend = isTRUE(show_legend),
    maxLegendKeys = unname(max_legend_keys),
    legendPosition = legend_position,
    legendTitle = unname(legend_title),
    legendValues = if (is.null(legend_values)) NULL else unname(legend_values),
    labelValues = if (is.null(label_values)) NULL else unname(label_values),
    mapBackground = map_background,
    connectorLinetype = connector_linetype,
    connectorEndpoint = connector_endpoint,
    connectorSmart = isTRUE(connector_smart),
    showOriginOutlines = isTRUE(show_origin_outlines),
    showMovementConnectors = isTRUE(show_movement_connectors),
    showMovementBand = isTRUE(show_movement_band),
    movementConnectorColor = unname(movement_connector_color),
    movementConnectorOpacity = unname(movement_connector_opacity),
    movementConnectorLinewidth = unname(movement_connector_linewidth),
    movementConnectorLinetype = movement_connector_linetype,
    movementConnectorEndpoint = movement_connector_endpoint,
    showDragTrail = isTRUE(show_drag_trail),
    sidePanel = isTRUE(side_panel)
  )

  template <- system.file("prototype", "index.html", package = "dragmapr", mustWork = TRUE)
  d3_file <- system.file("prototype", "d3.v7.min.js", package = "dragmapr", mustWork = TRUE)
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  html <- sub("__D3__", paste(readLines(d3_file, warn = FALSE), collapse = "\n"), html, fixed = TRUE)
  html <- sub("__GEOJSON__", json_for_script(geojson_text), html, fixed = TRUE)
  html <- sub("__LABELS__", json_for_script(label_data, dataframe = "rows"), html, fixed = TRUE)
  html <- sub("__REGION_OFFSETS__", json_for_script(region_offset_data, dataframe = "rows"), html, fixed = TRUE)
  html <- sub("__LABEL_OFFSETS__", json_for_script(label_offset_data, dataframe = "rows"), html, fixed = TRUE)
  html <- sub("__OPTIONS__", json_for_script(options), html, fixed = TRUE)
  html <- sub("__HTML_CLASS__", if (isTRUE(side_panel)) "" else "no-side-panel", html, fixed = TRUE)

  writeLines(html, file, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(file)
  }
  invisible(file)
}

json_for_script <- function(x, ...) {
  json <- if (is.character(x) && length(x) == 1L && !length(list(...))) {
    x
  } else {
    jsonlite::toJSON(x, ..., auto_unbox = TRUE)
  }
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  json <- gsub(">", "\\u003e", json, fixed = TRUE)
  json <- gsub("&", "\\u0026", json, fixed = TRUE)
  json
}
