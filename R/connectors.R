#' Region connector data
#'
#' Builds renderer-neutral movement connector rows from source geometry and a
#' `dragmapr_state`. Each row describes the line from a region's original anchor
#' to its state-adjusted anchor.
#'
#' @param x An `sf` object, a `dragmapr_layout`, or a layout-like object with sf
#'   geometry.
#' @param state A `dragmapr_state`.
#' @param region_col Region column in `x`. Defaults to the binding metadata
#'   stored in `state`.
#' @param threshold_m Minimum movement distance to mark a connector visible.
#' @param include_unmoved Include zero-distance rows.
#' @param as_sf Return an `sf` line layer instead of a plain data frame.
#'
#' @return A data frame or `sf` object.
#' @export
region_connectors <- function(x,
                              state,
                              region_col = NULL,
                              threshold_m = 1,
                              include_unmoved = FALSE,
                              as_sf = FALSE) {
  state <- validate_dragmapr_state(state)
  sf_obj <- connector_sf_input(x)
  validate_dragmapr_sf(sf_obj)
  region_col <- state_region_col(state, region_col)
  if (!region_col %in% names(sf_obj)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  threshold_m <- connector_threshold(threshold_m)
  include_unmoved <- flag_scalar(include_unmoved, "`include_unmoved`")
  as_sf <- flag_scalar(as_sf, "`as_sf`")

  offsets <- state$region_offsets
  regions <- natural_sort(unique(as.character(sf_obj[[region_col]])))
  idx <- match(regions, offsets$region)
  dx <- ifelse(is.na(idx), 0, offsets$dx_m[idx])
  dy <- ifelse(is.na(idx), 0, offsets$dy_m[idx])
  anchors <- region_anchor_table(sf_obj, region_col)
  anchors <- anchors[match(regions, anchors$region), , drop = FALSE]
  distance <- sqrt(dx^2 + dy^2)
  out <- data.frame(
    region = regions,
    origin_x = anchors$x,
    origin_y = anchors$y,
    final_x = anchors$x + dx,
    final_y = anchors$y + dy,
    dx_m = dx,
    dy_m = dy,
    distance_m = distance,
    visible = distance >= threshold_m,
    stringsAsFactors = FALSE
  )
  if (!include_unmoved) {
    out <- out[out$visible, , drop = FALSE]
  }
  if (as_sf) {
    connector_lines_sf(out, crs = sf::st_crs(sf_obj), id_col = "region")
  } else {
    out
  }
}

#' Label connector data
#'
#' Builds renderer-neutral connector rows from label anchors and label offsets.
#'
#' @param labels Label data accepted by [as_drag_labels()].
#' @param state A `dragmapr_state` or label-offset data frame.
#' @param threshold_m Minimum connector distance to mark visible.
#' @param connector_type Optional connector type override.
#' @param include_unmoved Include zero-distance rows.
#' @param as_sf Return an `sf` line layer instead of a plain data frame.
#' @param crs Optional CRS for `as_sf = TRUE`.
#'
#' @return A data frame or `sf` object.
#' @export
label_connectors <- function(labels,
                             state,
                             threshold_m = 1,
                             connector_type = NULL,
                             include_unmoved = FALSE,
                             as_sf = FALSE,
                             crs = NA) {
  labels <- as_drag_labels(labels)
  offsets <- if (inherits(state, "dragmapr_state")) {
    validate_dragmapr_state(state)$label_offsets
  } else {
    normalize_label_state(state, source = "`state`")
  }
  threshold_m <- connector_threshold(threshold_m)
  include_unmoved <- flag_scalar(include_unmoved, "`include_unmoved`")
  as_sf <- flag_scalar(as_sf, "`as_sf`")
  if (!is.null(connector_type)) {
    connector_type <- match.arg(
      connector_type,
      c("straight", "elbow", "curve", "squiggle")
    )
  }

  idx <- match(labels$label_id, offsets$label_id)
  dx <- ifelse(is.na(idx), 0, offsets$dx_m[idx])
  dy <- ifelse(is.na(idx), 0, offsets$dy_m[idx])
  type <- connector_type %||% labels$connector_type
  start_x <- ifelse(is.finite(labels$connector_start_x), labels$connector_start_x, labels$x)
  start_y <- ifelse(is.finite(labels$connector_start_y), labels$connector_start_y, labels$y)
  final_x <- labels$x + dx
  final_y <- labels$y + dy
  distance <- sqrt((final_x - start_x)^2 + (final_y - start_y)^2)
  out <- data.frame(
    label_id = labels$label_id,
    region = labels$region,
    source_x = start_x,
    source_y = start_y,
    label_x = final_x,
    label_y = final_y,
    dx_m = dx,
    dy_m = dy,
    distance_m = distance,
    connector_type = type,
    visible = labels$connector & distance >= threshold_m,
    stringsAsFactors = FALSE
  )
  if (!include_unmoved) {
    out <- out[out$visible, , drop = FALSE]
  }
  if (as_sf) {
    names(out)[names(out) == "source_x"] <- "origin_x"
    names(out)[names(out) == "source_y"] <- "origin_y"
    names(out)[names(out) == "label_x"] <- "final_x"
    names(out)[names(out) == "label_y"] <- "final_y"
    connector_lines_sf(out, crs = crs, id_col = "label_id")
  } else {
    out
  }
}

#' Coerce connector rows to sf lines
#'
#' @param connectors Data frame returned by `region_connectors()` or
#'   `label_connectors()`.
#' @param crs Optional CRS.
#'
#' @return An `sf` line layer.
#' @export
connector_sf <- function(connectors, crs = NA) {
  if (!is.data.frame(connectors)) {
    stop("`connectors` must be a data frame.", call. = FALSE)
  }
  required <- c("origin_x", "origin_y", "final_x", "final_y")
  missing <- setdiff(required, names(connectors))
  if (length(missing)) {
    stop("`connectors` is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  id_col <- intersect(c("region", "label_id", "id"), names(connectors))
  id_col <- if (length(id_col)) id_col[[1]] else NULL
  connector_lines_sf(connectors, crs = crs, id_col = id_col)
}

connector_sf_input <- function(x) {
  if (inherits(x, "sf")) {
    return(x)
  }
  if (is.list(x)) {
    for (nm in c("sf_grouped", "sf", "data", "source")) {
      if (inherits(x[[nm]], "sf")) {
        return(x[[nm]])
      }
    }
  }
  stop("`x` must be an sf object or layout-like object with sf geometry.", call. = FALSE)
}

region_anchor_table <- function(x, region_col) {
  keys <- as.character(x[[region_col]])
  grouped_keys <- natural_sort(unique(keys))
  geoms <- lapply(grouped_keys, function(key) {
    idx <- which(keys == key)
    suppressWarnings(sf::st_union(sf::st_geometry(x)[idx]))[[1L]]
  })
  grouped <- sf::st_sf(
    region = grouped_keys,
    geometry = sf::st_sfc(geoms, crs = sf::st_crs(x))
  )
  points <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(grouped)))
  xy <- sf::st_coordinates(points)
  data.frame(
    region = as.character(grouped$region),
    x = xy[, 1],
    y = xy[, 2],
    stringsAsFactors = FALSE
  )
}

connector_lines_sf <- function(connectors, crs = NA, id_col = NULL) {
  geom <- lapply(seq_len(nrow(connectors)), function(i) {
    sf::st_linestring(matrix(
      c(
        connectors$origin_x[i], connectors$origin_y[i],
        connectors$final_x[i], connectors$final_y[i]
      ),
      ncol = 2,
      byrow = TRUE
    ))
  })
  out <- connectors
  out$geometry <- sf::st_sfc(geom, crs = crs)
  sf::st_as_sf(out)
}

connector_threshold <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x < 0) {
    stop("`threshold_m` must be a single non-negative number.", call. = FALSE)
  }
  x
}
