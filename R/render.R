#' Render an adjusted draggable plot with labels
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_offsets Region offsets as a data frame, CSV path, or `NULL`.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for default region-label text. Defaults to
#'   `region_col`.
#' @param label_offsets Label state as a data frame, CSV path, or `NULL`.
#' @param labels Optional label table from `make_region_labels()` or
#'   `as_drag_labels()`. Use `FALSE` to omit labels from the static render.
#' @param region_palette Optional named vector of fill colors keyed by region.
#' @param region_labels Optional named vector of legend labels keyed by region.
#' @param show_legend Show the region fill legend.  When the number of distinct
#'   regions exceeds `max_legend_keys`, the legend is suppressed regardless of
#'   this setting (with an informational message).
#' @param max_legend_keys Maximum number of legend keys before the legend is
#'   automatically hidden.  Defaults to `25`.  Set `Inf` to always show.
#' @param legend_position Position of the fill legend in static exports.
#'   One of `"bottom"`, `"top"`, `"left"`, `"right"`, or `"none"`.
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
#' @param connector_curvature Curvature used by `"curve"` connectors.
#' @param connector_squiggle_amplitude,connector_squiggle_waves Appearance of
#'   `"squiggle"` connectors in static exports.
#'   `connector_squiggle_amplitude` is in the same coordinate units as the
#'   projected CRS (metres for EPSG:3857). The default of `12000` is tuned for
#'   country-scale datasets; scale it up or down to match your data extent.
#' @param connector_end_gap Distance, in plot units, to trim connector endpoints
#'   away from label centers. Defaults to an automatic value based on plot size.
#' @param label_padding Proportional padding added around displaced labels and
#'   connectors to avoid clipping static exports.
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
#' # The primary workflow is drag_map_prototype(open = TRUE): open the
#' # interactive HTML, drag regions and labels to your liking, then
#' # copy or download the offset CSVs.  Only call render_dragged_map()
#' # if you also need a reproducible ggplot2 image from those offsets.
#' \dontrun{
#' # Step 1 (always): open the interactive draggable plot.
#' drag_map_prototype(poly, region_col = "region", open = TRUE)
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
                               labels = NULL,
                               region_palette = NULL,
                               region_labels = NULL,
                               show_legend = TRUE,
                               max_legend_keys = 25L,
                               legend_position = c("bottom", "top", "left", "right", "none"),
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
                               connector_curvature = 0.18,
                               connector_squiggle_amplitude = 12000,
                               connector_squiggle_waves = 4,
                               connector_end_gap = NULL,
                               label_padding = 0.08,
                               file = NULL,
                               width = 8,
                               height = 6,
                               dpi = 300) {
  legend_position <- match.arg(legend_position)
  label_marker_shape <- match.arg(label_marker_shape)
  if (identical(legend_position, "none")) {
    show_legend <- FALSE
  }
  if (!isTRUE(show_label_marker)) {
    label_marker_shape <- "none"
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
  show_labels <- !identical(labels, FALSE)
  base_labels <- if (is.null(labels)) {
    make_region_labels(x, region_col = region_col, label_col = label_col)
  } else if (identical(labels, FALSE)) {
    data.frame(label_id = character(), region = character(), label = character(), x = numeric(), y = numeric())
  } else {
    as_drag_labels(labels)
  }
  anchor_labels <- apply_region_offsets_to_labels(base_labels, region_offsets)
  labels <- apply_label_state(anchor_labels, label_offsets)
  connectors <- if (show_labels) make_connector_data(
    anchor_labels,
    labels,
    end_gap = connector_end_gap
  ) else empty_connector_data()
  limits <- plot_limits(adjusted, if (show_labels) labels else NULL, connectors, padding = label_padding)

  regions <- natural_sort(unique(as.character(adjusted[[region_col]])))
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
      name = "Region",
      labels = if (is.null(region_labels)) ggplot2::waiver() else region_labels,
      guide = if (show_legend) "legend" else "none"
    )
  } else {
    ggplot2::scale_fill_manual(
      values = region_palette,
      breaks = regions,
      labels = if (is.null(region_labels)) ggplot2::waiver() else region_labels[regions],
      name = "Region",
      guide = if (show_legend) "legend" else "none"
    )
  }

  plot <- ggplot2::ggplot() +
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
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = legend_position,
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
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
          linetype = connector_linetype
        )
    }
    if (nrow(connectors$elbow) > 0L) {
      plot <- plot +
        ggplot2::geom_segment(
          data = connectors$elbow,
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xmid, yend = .data$ymid),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype
        ) +
        ggplot2::geom_segment(
          data = connectors$elbow,
          ggplot2::aes(x = .data$xmid, y = .data$ymid, xend = .data$xend, yend = .data$yend),
          color = connector_color,
          linewidth = connector_linewidth,
          linetype = connector_linetype
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
          curvature = connector_curvature
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
          linetype = connector_linetype
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
        plot <- plot +
          ggplot2::geom_text(
            data = point_labels,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, color = .data$label_color),
            fontface = "bold",
            size = label_size
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
    if (nrow(connector) > 0L) {
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
