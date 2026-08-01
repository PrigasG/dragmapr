#' Save the dragged layout as a static image
#'
#' Takes the offset CSVs you downloaded from [drag_map_prototype()] and
#' produces a `ggplot2` image with regions and labels in the positions you
#' dragged them to. If you used Spatial Studio, use
#' [render_dragmapr_project()] instead - it reads everything from the project
#' ZIP in one call.
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_offsets Region offsets as a data frame, CSV path, or `NULL`.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for default region-label text. Defaults to
#'   `region_col`.
#' @param label_offsets Label state as a data frame, CSV path, or `NULL`.
#' @param state Optional `dragmapr_state`. When supplied, its region and label
#'   offsets seed the static render. Explicit `region_offsets` or
#'   `label_offsets` arguments override the corresponding state table.
#' @param labels Optional label table from `make_region_labels()` or
#'   `as_drag_labels()`. Use `FALSE` to omit labels from the static render.
#' @param label_values Optional character vector of label IDs to render.
#'   `NULL` renders all labels unless `max_labels` is finite.
#' @param max_labels Maximum number of label IDs to render when `label_values`
#'   is `NULL`. Defaults to `Inf`, preserving the existing behavior of
#'   rendering all labels. Set to a finite value such as `25` to keep static
#'   exports readable.
#' @param label_prefer Optional label IDs to prioritize when `max_labels` is
#'   finite. Useful for moved regions, expanded branches, or labels the user
#'   selected manually.
#' @param region_palette Optional named vector of fill colors keyed by region.
#' @param region_labels Optional named vector of legend labels keyed by region.
#' @param show_legend Show the region fill legend.  When the number of distinct
#'   regions exceeds `max_legend_keys`, the legend is suppressed regardless of
#'   this setting (with an informational message).
#' @param max_legend_keys Maximum number of legend keys before the legend is
#'   automatically hidden.  Defaults to `25`.  Set `Inf` to always show.
#' @param legend_position Position of the fill legend in static exports.
#'   One of `"bottom"`, `"top"`, `"left"`, `"right"`, or `"none"`.
#' @param legend_title Title shown above the fill legend. Defaults to
#'   `"Region"`.
#' @param legend_values Optional character vector of region values to include
#'   in the legend. `NULL` includes all region values.
#' @param title Optional plot title.
#' @param label_size Text size passed to [ggplot2::geom_text()]. Defaults to
#'   `3.4`.
#' @param show_label_marker Draw markers behind ordinary text labels. Use
#'   `FALSE` for text-only labels.
#' @param label_marker_shape Marker shape for ordinary text labels in static
#'   exports. One of `"circle"`, `"rect"`, or `"none"`. If
#'   `show_label_marker = FALSE`, this is treated as `"none"`.
#' @param marker_size Point size passed to [ggplot2::geom_point()]. Defaults to
#'   `3.2`.
#' @param marker_fill Fill colour of the label marker circle. Defaults to
#'   `"white"`.
#' @param marker_color Stroke colour of the label marker circle. Defaults to
#'   `"black"`.
#' @param marker_stroke Stroke width of the label marker circle. Defaults to
#'   `0.8`.
#' @param box_label_size Text size for annotation boxes. Defaults to `3`.
#' @param box_fill,box_color Fill and border colours for annotation boxes.
#' @param box_wrap Approximate character width used to wrap annotation-box text.
#' @param connector_color,connector_linewidth,connector_linetype Static export
#'   styling for label connector lines.
#' @param connector_endpoint Connector endpoint style. One of `"none"` or
#'   `"arrow"`.
#' @param connector_curvature Curvature used by `"curve"` connectors.
#' @param connector_squiggle_amplitude,connector_squiggle_waves Appearance of
#'   `"squiggle"` connectors in static exports.
#'   `connector_squiggle_amplitude` is in the same coordinate units as the
#'   projected CRS. For common metre-based CRSs such as EPSG:3857, the value is
#'   metres; for degree-based or custom projected CRSs, use that CRS's units.
#'   The default of `12000` is tuned for country-scale metre-based datasets;
#'   scale it up or down to match your data extent.
#' @param connector_end_gap Distance, in plot units, to trim connector endpoints
#'   away from label centers. Defaults to an automatic value based on plot size.
#' @param show_origin_outlines Show the original, unshifted outlines of regions
#'   with non-zero offsets beneath the moved regions.
#' @param show_movement_connectors Draw a connector from each moved region's
#'   original representative point to its translated location.
#' @param show_movement_band Draw a swept shadow between each region's original
#'   footprint and its translated position. The shadow is built from the actual
#'   polygon boundary, so it follows the shape of the region rather than a flat
#'   bounding-box band.
#' @param movement_connector_color,movement_connector_opacity,movement_connector_linewidth
#'   Static export styling for movement connectors and the swept movement shadow.
#' @param movement_connector_linetype Movement connector line style. One of
#'   `"solid"`, `"dashed"`, or `"dotted"`.
#' @param movement_connector_endpoint Movement connector endpoint. One of
#'   `"none"`, `"open"`, or `"closed"`.
#' @param label_padding Proportional padding added around displaced labels and
#'   connectors to avoid clipping static exports.
#' @param map_background Static map background. One of `"white"`,
#'   `"transparent"`, `"light_grid"`, or `"dark"`.
#' @param file Optional output image path. When supplied, `ggplot2::ggsave()`
#'   saves the plot.
#' @param width,height,dpi Output settings used when `file` is supplied.
#'
#' @return A `ggplot` object.
#' @seealso [drag_map_prototype()] for the interactive draggable browser helper
#'   that produces the offset CSVs; [read_offsets()] and [read_label_state()]
#'   to load saved offsets from disk; [as_drag_annotations()] for info-box labels.
#' @importFrom rlang .data
#' @export
#' @examples
#' # render_dragged_map() is an optional static-export step.
#' # The primary workflow is drag_map_prototype(open = interactive()): open the
#' # interactive HTML, drag regions and labels to your liking, then
#' # copy or download the offset CSVs.  Only call render_dragged_map()
#' # if you also need a reproducible ggplot2 image from those offsets.
#' if(interactive()){
#' # Step 1 (always): open the interactive draggable plot.
#' drag_map_prototype(poly, region_col = "region", open = interactive())
#'
#' # Step 2 (optional): after dragging, reconstruct as a static image.
#' region_offsets <- read_offsets("my_region_offsets.csv")
#' render_dragged_map(poly, region_col = "region",
#'                   region_offsets = region_offsets)
#' }
#'
#' # Minimal runnable example (no prior dragging session needed).
#' poly <- sf::st_sf(
#'   region = c("A", "B"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     crs = 3857
#'   )
#' )
#' render_dragged_map(poly, region_col = "region")
render_dragged_map <- function(x,
                               region_offsets = NULL,
                               region_col,
                               label_col = region_col,
                               label_offsets = NULL,
                               state = NULL,
                               labels = NULL,
                               label_values = NULL,
                               max_labels = Inf,
                               label_prefer = NULL,
                               region_palette = NULL,
                               region_labels = NULL,
                               show_legend = TRUE,
                               max_legend_keys = 25L,
                               legend_position = c("bottom", "top", "left", "right", "none"),
                               legend_title = "Region",
                               legend_values = NULL,
                               title = NULL,
                               label_size = 3.4,
                               show_label_marker = TRUE,
                               label_marker_shape = c("circle", "rect", "none"),
                               marker_size = 3.2,
                               marker_fill = "white",
                               marker_color = "black",
                               marker_stroke = 0.8,
                               box_label_size = 3,
                               box_fill = "white",
                               box_color = "black",
                               box_wrap = 24,
                               connector_color = "#334155",
                               connector_linewidth = 0.35,
                               connector_linetype = "solid",
                               connector_endpoint = c("none", "arrow"),
                               connector_curvature = 0.18,
                               connector_squiggle_amplitude = 12000,
                               connector_squiggle_waves = 4,
                               connector_end_gap = NULL,
                               show_origin_outlines = FALSE,
                               show_movement_connectors = FALSE,
                               show_movement_band = FALSE,
                               movement_connector_color = "#64748b",
                               movement_connector_opacity = 0.72,
                               movement_connector_linewidth = 0.45,
                               movement_connector_linetype = c("solid", "dashed", "dotted"),
                               movement_connector_endpoint = c("closed", "open", "none"),
                               label_padding = 0.08,
                               map_background = c("white", "transparent", "light_grid", "dark"),
                               file = NULL,
                               width = 8,
                               height = 6,
                               dpi = 300) {
  validate_dragmapr_sf(x)
  legend_position <- match.arg(legend_position)
  connector_linetype <- match.arg(connector_linetype, c("solid", "dashed", "dotted"))
  connector_endpoint <- match.arg(connector_endpoint)
  movement_connector_linetype <- match.arg(movement_connector_linetype)
  movement_connector_endpoint <- match.arg(movement_connector_endpoint)
  map_background <- match.arg(map_background)
  label_marker_shape <- match.arg(label_marker_shape)
  if (!is.null(legend_values) && !is.character(legend_values)) {
    stop("`legend_values` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(label_values) && !is.character(label_values)) {
    stop("`label_values` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.numeric(max_labels) || length(max_labels) != 1L ||
      is.na(max_labels) || max_labels < 0) {
    stop("`max_labels` must be a non-negative number or Inf.", call. = FALSE)
  }
  if (!is.null(label_prefer) && !is.character(label_prefer)) {
    stop("`label_prefer` must be a character vector or NULL.", call. = FALSE)
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
  if (identical(legend_position, "none")) {
    show_legend <- FALSE
  }
  if (!isTRUE(show_label_marker)) {
    label_marker_shape <- "none"
  }
  if (!is.null(state)) {
    state <- validate_dragmapr_state(state)
    if (!is.null(region_offsets) || !is.null(label_offsets)) {
      message(
        "dragmapr: explicit offset arguments override matching tables in `state`."
      )
    }
    if (is.null(region_offsets)) {
      region_offsets <- state$region_offsets
    }
    if (is.null(label_offsets)) {
      label_offsets <- state$label_offsets
    }
    if (!is.null(state$crs) && inherits(x, "sf")) {
      target <- sf::st_crs(x)
      authored <- tryCatch(sf::st_crs(state$crs), error = function(e) NA)
      if (!is.na(authored) && !is.na(target) && authored != target) {
        warning(
          "State was composed in a different CRS than `x`. ",
          "Metre offsets may be misplaced; reproject `x` to the state CRS first.",
          call. = FALSE
        )
      }
    }
  }
  if (is.null(region_offsets)) {
    region_offsets <- data.frame(
      region = natural_sort(unique(as.character(x[[region_col]]))),
      dx_m = 0,
      dy_m = 0,
      stringsAsFactors = FALSE
    )
  }
  if (is.character(region_offsets) && length(region_offsets) == 1L) {
    region_offsets <- read_offsets(region_offsets)
  }
  if (!is.data.frame(region_offsets)) {
    stop("`region_offsets` must be a data frame, CSV path, or NULL.", call. = FALSE)
  }
  region_offsets <- normalize_offsets(region_offsets, source = "`region_offsets`")
  adjusted <- apply_offsets(x, region_offsets, region_col = region_col)
  moved_regions <- region_offsets$region[
    is.finite(region_offsets$dx_m) & is.finite(region_offsets$dy_m) &
      (region_offsets$dx_m != 0 | region_offsets$dy_m != 0)
  ]
  origin_outlines <- if (isTRUE(show_origin_outlines) && length(moved_regions) > 0L) {
    x[as.character(x[[region_col]]) %in% moved_regions, , drop = FALSE]
  } else {
    x[FALSE, , drop = FALSE]
  }
  movement_connectors <- if (isTRUE(show_movement_connectors)) {
    make_movement_connector_data(x, region_offsets, region_col)
  } else {
    data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  }
  movement_band <- if (isTRUE(show_movement_band)) {
    make_boundary_swept_band(x, region_offsets, region_col)
  } else {
    sf::st_sf(
      data.frame(region = character(), dx_m = numeric(), dy_m = numeric(),
                 distance = numeric(), stringsAsFactors = FALSE),
      geometry = sf::st_sfc(crs = sf::st_crs(x))
    )
  }
  show_labels <- !identical(labels, FALSE)
  base_labels <- if (is.null(labels)) {
    make_region_labels(x, region_col = region_col, label_col = label_col)
  } else if (identical(labels, FALSE)) {
    data.frame(label_id = character(), region = character(), label = character(), x = numeric(), y = numeric())
  } else {
    as_drag_labels(labels)
  }
  if (is.null(label_values) && is.finite(max_labels) && nrow(base_labels) > max_labels) {
    label_values <- select_label_ids(
      base_labels,
      max_labels = as.integer(max_labels),
      prefer = label_prefer
    )
  }
  if (!is.null(label_values)) {
    base_labels <- base_labels[as.character(base_labels$label_id) %in% label_values, , drop = FALSE]
  }
  anchor_labels <- apply_region_offsets_to_labels(base_labels, region_offsets)
  labels <- apply_label_state(anchor_labels, label_offsets)
  connectors <- if (show_labels) make_connector_data(
    anchor_labels,
    labels,
    end_gap = connector_end_gap
  ) else empty_connector_data()
  limit_geometry <- if (nrow(origin_outlines) > 0L) rbind(adjusted, origin_outlines) else adjusted
  limit_connectors <- c(connectors, list(movement = movement_connectors, band_sf = movement_band))
  limits <- plot_limits(limit_geometry, if (show_labels) labels else NULL, limit_connectors, padding = label_padding)

  regions <- natural_sort(unique(as.character(adjusted[[region_col]])))
  legend_breaks <- if (is.null(legend_values)) regions else intersect(legend_values, regions)
  if (length(legend_breaks) == 0L) {
    show_legend <- FALSE
  }
  if (show_legend && length(regions) > max_legend_keys) {
    message(
      "dragmapr: legend suppressed - ", length(regions), " groups exceeds ",
      "max_legend_keys = ", max_legend_keys, ". ",
      "Set show_legend = FALSE explicitly to silence this, or raise max_legend_keys."
    )
    show_legend <- FALSE
  }
  fill_scale <- if (is.null(region_palette)) {
    ggplot2::scale_fill_discrete(
      name = legend_title,
      breaks = legend_breaks,
      labels = if (is.null(region_labels)) ggplot2::waiver() else region_labels[legend_breaks],
      guide = if (show_legend) "legend" else "none"
    )
  } else {
    ggplot2::scale_fill_manual(
      values = region_palette,
      breaks = legend_breaks,
      labels = if (is.null(region_labels)) ggplot2::waiver() else region_labels[legend_breaks],
      name = legend_title,
      guide = if (show_legend) "legend" else "none"
    )
  }

  background <- map_background_style(map_background)
  connector_arrow <- if (identical(connector_endpoint, "arrow")) {
    grid::arrow(type = "closed", length = grid::unit(0.08, "inches"))
  } else {
    NULL
  }
  movement_connector_arrow <- switch(
    movement_connector_endpoint,
    open = grid::arrow(type = "open", length = grid::unit(0.06, "inches")),
    closed = grid::arrow(type = "closed", length = grid::unit(0.06, "inches")),
    NULL
  )

  plot <- ggplot2::ggplot()
  if (nrow(origin_outlines) > 0L) {
    plot <- plot +
      ggplot2::geom_sf(
        data = origin_outlines,
        fill = NA,
        color = "#64748b",
        linewidth = 0.45,
        linetype = "dashed",
        alpha = 0.58
      )
  }
  if (nrow(movement_connectors) > 0L) {
    plot <- plot +
      ggplot2::geom_segment(
        data = movement_connectors,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        inherit.aes = FALSE,
        color = movement_connector_color,
        linewidth = movement_connector_linewidth,
        linetype = movement_connector_linetype,
        alpha = movement_connector_opacity,
        arrow = movement_connector_arrow
      )
  }
  if (nrow(movement_band) > 0L) {
    plot <- plot +
      ggplot2::geom_sf(
        data = movement_band,
        inherit.aes = FALSE,
        fill = movement_connector_color,
        color = movement_connector_color,
        alpha = movement_connector_opacity * 0.18,
        linewidth = movement_connector_linewidth * 0.25
      )
  }
  plot <- plot +
    ggplot2::geom_sf(
      data = adjusted,
      ggplot2::aes(fill = factor(.data[[region_col]], levels = regions)),
      color = "white",
      linewidth = 0.3
    ) +
    fill_scale +
    ggplot2::coord_sf(xlim = limits$xlim, ylim = limits$ylim, expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background$plot, color = NA),
      panel.background = ggplot2::element_rect(fill = background$panel, color = NA),
      panel.grid.major = background$grid,
      legend.position = legend_position,
      legend.background = ggplot2::element_rect(fill = background$legend, color = NA),
      legend.title = ggplot2::element_text(color = background$text),
      legend.text = ggplot2::element_text(color = background$text),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        color = background$text
      )
    ) +
    ggplot2::labs(title = title)

  if (show_labels) {
    uses_label_color <- FALSE
    if (nrow(connectors$straight) > 0L) {
      plot <- plot +
        ggplot2::geom_segment(
          data = connectors$straight,
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype,
          arrow = connector_arrow
        )
    }
    if (nrow(connectors$elbow) > 0L) {
      plot <- plot +
        ggplot2::geom_segment(
          data = connectors$elbow,
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xmid, yend = .data$ymid),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype,
          arrow = NULL
        ) +
        ggplot2::geom_segment(
          data = connectors$elbow,
          ggplot2::aes(x = .data$xmid, y = .data$ymid, xend = .data$xend, yend = .data$yend),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype,
          arrow = connector_arrow
        )
    }
    if (nrow(connectors$curve) > 0L) {
      plot <- plot +
        ggplot2::geom_curve(
          data = connectors$curve,
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype,
          curvature = connector_curvature,
          arrow = connector_arrow
        )
    }
    if (nrow(connectors$squiggle) > 0L) {
      plot <- plot +
        ggplot2::geom_path(
          data = make_squiggle_data(
            connectors$squiggle,
            amplitude = connector_squiggle_amplitude,
            waves = connector_squiggle_waves
          ),
          ggplot2::aes(x = .data$x, y = .data$y, group = .data$connector_id),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype,
          arrow = connector_arrow
        )
    }
    point_labels <- labels[labels$label_type == "label", , drop = FALSE]
    box_labels <- labels[labels$label_type == "box", , drop = FALSE]
    if (nrow(point_labels) > 0L) {
      if (!"label_color" %in% names(point_labels)) {
        point_labels$label_color <- marker_color
      }
      if (identical(label_marker_shape, "circle")) {
        plot <- plot +
          ggplot2::geom_point(
            data = point_labels,
            ggplot2::aes(x = .data$x, y = .data$y),
            size = marker_size,
            shape = 21,
            fill = marker_fill,
            color = marker_color,
            stroke = marker_stroke
          )
      } else if (identical(label_marker_shape, "rect")) {
        plot <- plot +
          ggplot2::geom_label(
            data = point_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, color = .data$label_color),
            fontface = "bold",
            size = label_size,
            fill = marker_fill,
            linewidth = marker_stroke * 0.25,
            label.padding = grid::unit(0.16, "lines")
          )
      }
      if (!identical(label_marker_shape, "rect")) {
        # Use geom_label with no visible border so the fill masks the connector
        # line behind text characters that overflow the marker boundary.
        plot <- plot +
          ggplot2::geom_label(
            data = point_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, color = .data$label_color),
            fontface = "bold",
            size = label_size,
            fill = marker_fill,
            linewidth = 0,
            label.padding = grid::unit(0.08, "lines")
          )
      }
      uses_label_color <- TRUE
    }
    if (nrow(box_labels) > 0L) {
      box_labels$label <- wrap_label_text(box_labels$label, box_wrap)
      if (!"label_color" %in% names(box_labels)) {
        box_labels$label_color <- box_color
      }
      plot <- plot +
        ggplot2::geom_label(
          data = box_labels,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, color = .data$label_color),
          size = box_label_size,
          fill = box_fill,
          linewidth = 0.35,
          label.padding = grid::unit(0.22, "lines"),
          lineheight = 0.95
        )
      uses_label_color <- TRUE
    }
    if (isTRUE(uses_label_color)) {
      plot <- plot + ggplot2::scale_color_identity()
    }
  }

  if (!is.null(file)) {
    ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi)
  }
  plot
}

map_background_style <- function(map_background) {
  switch(
    map_background,
    white = list(
      plot = "white",
      panel = "white",
      legend = "white",
      text = "#172033",
      grid = ggplot2::element_blank()
    ),
    transparent = list(
      plot = NA,
      panel = NA,
      legend = NA,
      text = "#172033",
      grid = ggplot2::element_blank()
    ),
    light_grid = list(
      plot = "white",
      panel = "#f8fafc",
      legend = "white",
      text = "#172033",
      grid = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.2)
    ),
    dark = list(
      plot = "#111827",
      panel = "#111827",
      legend = "#111827",
      text = "#f8fafc",
      grid = ggplot2::element_blank()
    )
  )
}

make_movement_connector_data <- function(x, region_offsets, region_col) {
  moved <- region_offsets[
    is.finite(region_offsets$dx_m) & is.finite(region_offsets$dy_m) &
      (region_offsets$dx_m != 0 | region_offsets$dy_m != 0),
    ,
    drop = FALSE
  ]
  if (nrow(moved) == 0L) {
    return(data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()))
  }
  grouped <- split(seq_len(nrow(x)), as.character(x[[region_col]]))
  rows <- lapply(moved$region, function(region) {
    idx <- grouped[[as.character(region)]]
    if (is.null(idx) || length(idx) == 0L) return(NULL)
    point <- suppressWarnings(sf::st_point_on_surface(sf::st_union(sf::st_geometry(x[idx, , drop = FALSE]))))
    coords <- sf::st_coordinates(point)[1L, c("X", "Y")]
    offset <- moved[moved$region == region, , drop = FALSE][1L, ]
    data.frame(
      region = as.character(region),
      x = unname(coords[["X"]]),
      y = unname(coords[["Y"]]),
      xend = unname(coords[["X"]]) + offset$dx_m,
      yend = unname(coords[["Y"]]) + offset$dy_m,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  } else {
    rownames(out) <- NULL
    out
  }
}

make_boundary_swept_band <- function(x, offsets, region_col, min_distance = 1e-6) {
  if (!inherits(x, "sf")) stop("`x` must be an sf object.", call. = FALSE)
  offsets <- normalize_offsets(offsets, source = "`offsets`")
  groups  <- unique(as.character(x[[region_col]]))
  crs     <- sf::st_crs(x)

  make_ring_faces <- function(coords, dx, dy) {
    if (nrow(coords) < 2L) return(NULL)
    faces <- vector("list", nrow(coords) - 1L)
    for (i in seq_len(nrow(coords) - 1L)) {
      x1 <- coords[i,      "X"]; y1 <- coords[i,      "Y"]
      x2 <- coords[i + 1L, "X"]; y2 <- coords[i + 1L, "Y"]
      ring <- rbind(
        c(x1, y1), c(x2, y2),
        c(x2 + dx, y2 + dy), c(x1 + dx, y1 + dy),
        c(x1, y1)
      )
      faces[[i]] <- sf::st_polygon(list(ring))
    }
    faces
  }

  pieces <- lapply(groups, function(g) {
    off_idx  <- match(g, offsets$region)
    if (is.na(off_idx)) return(NULL)
    dx       <- offsets$dx_m[off_idx]
    dy       <- offsets$dy_m[off_idx]
    distance <- sqrt(dx^2 + dy^2)
    if (!is.finite(distance) || distance <= min_distance) return(NULL)
    idx <- which(as.character(x[[region_col]]) == g)
    if (length(idx) == 0L) return(NULL)
    geom     <- suppressWarnings(sf::st_union(sf::st_geometry(x[idx, , drop = FALSE])))
    boundary <- suppressWarnings(sf::st_cast(sf::st_boundary(geom), "LINESTRING"))
    coords   <- sf::st_coordinates(boundary)
    if (!all(c("X", "Y", "L1") %in% colnames(coords))) return(NULL)
    faces_list <- split(as.data.frame(coords), coords[, "L1"])
    polys <- unlist(lapply(faces_list, make_ring_faces, dx = dx, dy = dy), recursive = FALSE)
    if (length(polys) == 0L) return(NULL)
    swept <- suppressWarnings(sf::st_union(sf::st_sfc(polys, crs = crs)))
    sf::st_sf(
      data.frame(region = g, dx_m = dx, dy_m = dy, distance = distance,
                 stringsAsFactors = FALSE),
      geometry = swept
    )
  })

  pieces <- Filter(Negate(is.null), pieces)

  if (length(pieces) == 0L) {
    return(sf::st_sf(
      data.frame(region = character(), dx_m = numeric(), dy_m = numeric(),
                 distance = numeric(), stringsAsFactors = FALSE),
      geometry = sf::st_sfc(crs = crs)
    ))
  }

  do.call(rbind, pieces)
}

make_connector_data <- function(anchor_labels, labels, end_gap = NULL) {
  if (nrow(labels) == 0L) {
    return(empty_connector_data())
  }
  anchor_labels <- normalize_labels(anchor_labels)
  labels <- normalize_labels(labels)
  labels$x_anchor <- anchor_labels$x[match(labels$label_id, anchor_labels$label_id)]
  labels$y_anchor <- anchor_labels$y[match(labels$label_id, anchor_labels$label_id)]
  labels <- labels[labels$connector & is.finite(labels$x_anchor) & is.finite(labels$y_anchor), , drop = FALSE]
  start_x <- ifelse(is.finite(labels$connector_start_x), labels$connector_start_x, labels$x_anchor)
  start_y <- ifelse(is.finite(labels$connector_start_y), labels$connector_start_y, labels$y_anchor)
  base <- data.frame(
    connector_id = labels$label_id,
    x = start_x,
    y = start_y,
    xend = labels$x,
    yend = labels$y,
    xmid = ifelse(is.finite(labels$connector_mid_x), labels$connector_mid_x, labels$x),
    ymid = ifelse(is.finite(labels$connector_mid_y), labels$connector_mid_y, start_y),
    connector_type = labels$connector_type,
    stringsAsFactors = FALSE
  )
  base <- trim_connector_endpoints(base, end_gap)
  moved <- is.finite(base$x) & is.finite(base$y) &
    is.finite(base$xend) & is.finite(base$yend) &
    (abs(base$x - base$xend) > .Machine$double.eps |
       abs(base$y - base$yend) > .Machine$double.eps)
  base <- base[moved, , drop = FALSE]
  list(
    straight = base[base$connector_type == "straight", c("x", "y", "xend", "yend"), drop = FALSE],
    elbow = base[base$connector_type == "elbow", , drop = FALSE],
    curve = base[base$connector_type == "curve", c("x", "y", "xend", "yend"), drop = FALSE],
    squiggle = base[base$connector_type == "squiggle", c("connector_id", "x", "y", "xend", "yend"), drop = FALSE]
  )
}

trim_connector_endpoints <- function(base, end_gap = NULL) {
  if (nrow(base) == 0L) {
    return(base)
  }
  if (is.null(end_gap)) {
    span_x <- diff(range(c(base$x, base$xend), finite = TRUE))
    span_y <- diff(range(c(base$y, base$yend), finite = TRUE))
    end_gap <- sqrt(span_x^2 + span_y^2) * 0.012
  }
  if (!is.numeric(end_gap) || length(end_gap) != 1L || !is.finite(end_gap) || end_gap < 0) {
    stop("`connector_end_gap` must be a non-negative number or NULL.", call. = FALSE)
  }
  dx <- base$xend - base$x
  dy <- base$yend - base$y
  length <- sqrt(dx^2 + dy^2)
  trim <- pmin(end_gap, length / 2)
  can_trim <- is.finite(length) & length > .Machine$double.eps
  base$xend[can_trim] <- base$xend[can_trim] - dx[can_trim] / length[can_trim] * trim[can_trim]
  base$yend[can_trim] <- base$yend[can_trim] - dy[can_trim] / length[can_trim] * trim[can_trim]
  base
}

empty_connector_data <- function() {
  empty <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  list(
    straight = empty,
    elbow = data.frame(connector_id = character(), x = numeric(), y = numeric(), xend = numeric(), yend = numeric(), xmid = numeric(), ymid = numeric(), connector_type = character()),
    curve = empty,
    squiggle = data.frame(connector_id = character(), x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  )
}

make_squiggle_data <- function(connectors, amplitude, waves, n = 80) {
  if (nrow(connectors) == 0L) {
    return(data.frame(connector_id = character(), x = numeric(), y = numeric()))
  }
  if (!is.numeric(amplitude) || length(amplitude) != 1L || !is.finite(amplitude) || amplitude < 0) {
    stop("`connector_squiggle_amplitude` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(waves) || length(waves) != 1L || !is.finite(waves) || waves <= 0) {
    stop("`connector_squiggle_waves` must be a positive number.", call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(connectors)), function(i) {
    row <- connectors[i, , drop = FALSE]
    t <- seq(0, 1, length.out = n)
    dx <- row$xend - row$x
    dy <- row$yend - row$y
    length <- sqrt(dx^2 + dy^2)
    if (!is.finite(length) || length <= .Machine$double.eps) {
      return(NULL)
    }
    nx <- -dy / length
    ny <- dx / length
    wiggle <- sin(t * waves * 2 * pi) * amplitude * sin(t * pi)
    data.frame(
      connector_id = row$connector_id,
      x = row$x + dx * t + nx * wiggle,
      y = row$y + dy * t + ny * wiggle,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

plot_limits <- function(adjusted, labels = NULL, connectors = empty_connector_data(), padding = 0.08) {
  if (!is.numeric(padding) || length(padding) != 1L || !is.finite(padding) || padding < 0) {
    stop("`label_padding` must be a non-negative number.", call. = FALSE)
  }
  bbox <- sf::st_bbox(adjusted)
  xs <- c(bbox[["xmin"]], bbox[["xmax"]])
  ys <- c(bbox[["ymin"]], bbox[["ymax"]])
  if (!is.null(labels) && nrow(labels) > 0L) {
    xs <- c(xs, labels$x)
    ys <- c(ys, labels$y)
  }
  for (connector in connectors) {
    if (inherits(connector, "sf") && nrow(connector) > 0L) {
      bb <- sf::st_bbox(connector)
      xs <- c(xs, bb[["xmin"]], bb[["xmax"]])
      ys <- c(ys, bb[["ymin"]], bb[["ymax"]])
    } else if (is.data.frame(connector) && nrow(connector) > 0L) {
      x_cols <- intersect(c("x", "xend", "xmid"), names(connector))
      y_cols <- intersect(c("y", "yend", "ymid"), names(connector))
      xs <- c(xs, unlist(connector[x_cols], use.names = FALSE))
      ys <- c(ys, unlist(connector[y_cols], use.names = FALSE))
    }
  }
  xs <- xs[is.finite(xs)]
  ys <- ys[is.finite(ys)]
  xlim <- range(xs)
  ylim <- range(ys)
  x_pad <- diff(xlim) * padding
  y_pad <- diff(ylim) * padding
  if (!is.finite(x_pad) || x_pad == 0) x_pad <- 1
  if (!is.finite(y_pad) || y_pad == 0) y_pad <- 1
  list(xlim = xlim + c(-x_pad, x_pad), ylim = ylim + c(-y_pad, y_pad))
}

wrap_label_text <- function(text, width) {
  if (!is.numeric(width) || length(width) != 1L || !is.finite(width) || width <= 0) {
    stop("`box_wrap` must be a positive number.", call. = FALSE)
  }
  vapply(
    as.character(text),
    function(x) paste(strwrap(x, width = width), collapse = "\n"),
    character(1)
  )
}

apply_region_offsets_to_labels <- function(labels, region_offsets) {
  labels <- normalize_labels(labels)
  region_offsets <- normalize_offsets(region_offsets, source = "`region_offsets`")
  match_idx <- match(labels$region, region_offsets$region)
  has_offset <- !is.na(match_idx)
  labels$x[has_offset] <- labels$x[has_offset] + region_offsets$dx_m[match_idx[has_offset]]
  labels$y[has_offset] <- labels$y[has_offset] + region_offsets$dy_m[match_idx[has_offset]]
  shiftable <- c("connector_start", "connector_mid")
  for (prefix in shiftable) {
    x_col <- paste0(prefix, "_x")
    y_col <- paste0(prefix, "_y")
    if (all(c(x_col, y_col) %in% names(labels))) {
      finite <- has_offset & is.finite(labels[[x_col]]) & is.finite(labels[[y_col]])
      labels[[x_col]][finite] <- labels[[x_col]][finite] + region_offsets$dx_m[match_idx[finite]]
      labels[[y_col]][finite] <- labels[[y_col]][finite] + region_offsets$dy_m[match_idx[finite]]
    }
  }
  labels
}
