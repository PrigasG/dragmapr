#' Render an adjusted map with draggable-style labels
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_offsets Region offsets as a data frame, CSV path, or `NULL`.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for label text. Defaults to `region_col`.
#' @param label_offsets Label offsets as a data frame, CSV path, or `NULL`.
#' @param labels Optional label table from `make_labels()`.
#' @param region_palette Optional named vector of fill colors keyed by region.
#' @param region_labels Optional named vector of legend labels keyed by region.
#' @param show_legend Show the region fill legend.
#' @param title Optional plot title.
#' @param file Optional output image path. When supplied, `ggplot2::ggsave()`
#'   saves the plot.
#' @param width,height,dpi Output settings used when `file` is supplied.
#'
#' @return A `ggplot` object.
#' @export
render_dragged_map <- function(x,
                               region_offsets = NULL,
                               region_col,
                               label_col = region_col,
                               label_offsets = NULL,
                               labels = NULL,
                               region_palette = NULL,
                               region_labels = NULL,
                               show_legend = TRUE,
                               title = NULL,
                               file = NULL,
                               width = 8,
                               height = 6,
                               dpi = 300) {
  if (is.null(region_offsets)) {
    region_offsets <- data.frame(
      region = sort(unique(as.character(x[[region_col]]))),
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
  base_labels <- if (is.null(labels)) {
    make_labels(x, region_col = region_col, label_col = label_col)
  } else {
    normalize_labels(labels)
  }
  labels <- apply_region_offsets_to_labels(base_labels, region_offsets)
  labels <- apply_label_offsets(labels, label_offsets)

  regions <- sort(unique(as.character(adjusted[[region_col]])))
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
    ggplot2::geom_point(
      data = labels,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 3.2,
      shape = 21,
      fill = "white",
      color = "black",
      stroke = 0.8
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      fontface = "bold",
      size = 3.4
    ) +
    fill_scale +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::labs(title = title)

  if (!is.null(file)) {
    ggplot2::ggsave(file, plot, width = width, height = height, dpi = dpi)
  }
  plot
}

apply_region_offsets_to_labels <- function(labels, region_offsets) {
  labels <- normalize_labels(labels)
  region_offsets <- normalize_offsets(region_offsets, source = "`region_offsets`")
  match_idx <- match(labels$region, region_offsets$region)
  has_offset <- !is.na(match_idx)
  labels$x[has_offset] <- labels$x[has_offset] + region_offsets$dx_m[match_idx[has_offset]]
  labels$y[has_offset] <- labels$y[has_offset] + region_offsets$dy_m[match_idx[has_offset]]
  labels
}
