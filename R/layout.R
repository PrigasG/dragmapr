#' Suggest automatic region offsets for a draggable map
#'
#' Computes a starting layout for [drag_map_prototype()] or
#' [render_dragged_map()] without manual dragging. The result is a data frame
#' of offsets that can be refined interactively in the browser.
#'
#' @details
#' Available layout methods:
#' \describe{
#'   \item{`"radial"`}{Each region is pushed outward from the collective
#'     centroid in the direction of its own centroid. Regions that are already
#'     spread out are pushed further than regions near the centre.}
#'   \item{`"horizontal"`}{Regions are ranked by centroid x-coordinate and
#'     spread evenly along the x-axis. Useful for left-to-right layouts.}
#'   \item{`"vertical"`}{Regions are ranked by centroid y-coordinate and spread
#'     evenly along the y-axis.}
#'   \item{`"grid"`}{Regions are arranged in a rectangular grid ordered by
#'     centroid position (top-to-bottom, left-to-right).}
#'   \item{`"none"`}{Returns zero offsets for every region. Useful as a
#'     placeholder when you want to start from an undisplaced state.}
#' }
#'
#' The `scale` argument multiplies all computed offsets uniformly. Increase it
#' to spread regions further apart; decrease it for a tighter layout.
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column in `x` defining draggable groups.
#' @param method Layout algorithm. One of `"radial"` (default),
#'   `"horizontal"`, `"vertical"`, `"grid"`, or `"none"`.
#' @param scale Positive numeric multiplier applied to all offsets. Defaults to
#'   `1`. Values greater than `1` push regions further apart.
#'
#' @return A data frame with `region`, `dx_m`, and `dy_m` columns, one row per
#'   unique region value. Suitable for use as `region_offsets` in
#'   [drag_map_prototype()] or [render_dragged_map()].
#' @export
#' @seealso [drag_map_prototype()] to open the interactive helper with the
#'   suggested layout as a starting point; [apply_offsets()] to apply offsets
#'   directly to an `sf` object.
#' @examples
#' poly <- sf::st_sf(
#'   region = c("A", "B", "C", "D"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     sf::st_polygon(list(rbind(c(1e5,1e5),c(2e5,1e5),c(2e5,2e5),c(1e5,2e5),c(1e5,1e5)))),
#'     sf::st_polygon(list(rbind(c(1e5,0),c(2e5,0),c(2e5,1e5),c(1e5,1e5),c(1e5,0)))),
#'     crs = 3857
#'   )
#' )
#' suggest_offsets(poly, region_col = "region", method = "radial")
#' suggest_offsets(poly, region_col = "region", method = "horizontal")
#' suggest_offsets(poly, region_col = "region", method = "grid")
suggest_offsets <- function(x,
                            region_col,
                            method = c("radial", "horizontal", "vertical", "grid", "none"),
                            scale  = 1) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found in `x`.", call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop(
      "Project `x` before computing offsets. ",
      "Use prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857).",
      call. = FALSE
    )
  }
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) || scale <= 0) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }
  method <- match.arg(method)

  regions <- unique(as.character(x[[region_col]]))
  regions <- regions[!is.na(regions) & nzchar(regions)]
  n       <- length(regions)

  if (n == 0L) {
    stop("No valid region values found in region_col '", region_col, "'.", call. = FALSE)
  }

  offsets <- data.frame(region = regions, dx_m = 0, dy_m = 0,
                        stringsAsFactors = FALSE)

  if (method == "none" || n == 1L) return(offsets)

  # Per-region centroids (suppress warning for point / empty geometries)
  cents <- lapply(regions, function(r) {
    sub_sf <- x[as.character(x[[region_col]]) == r, , drop = FALSE]
    suppressWarnings(
      sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_geometry(sub_sf))))
    )
  })
  cx <- vapply(cents, function(m) if (nrow(m) > 0L) m[1L, "X"] else NA_real_, numeric(1L))
  cy <- vapply(cents, function(m) if (nrow(m) > 0L) m[1L, "Y"] else NA_real_, numeric(1L))

  if (any(is.na(cx) | is.na(cy))) {
    warning("Could not compute centroids for some regions; those offsets will be zero.",
            call. = FALSE)
    cx[is.na(cx)] <- mean(cx, na.rm = TRUE)
    cy[is.na(cy)] <- mean(cy, na.rm = TRUE)
  }

  bb   <- sf::st_bbox(x)
  diag <- sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2)
  # Push distance: ~30% of bbox diagonal scaled per sqrt(n) and user scale
  push <- unname(diag) * 0.3 * scale / max(sqrt(n - 1L), 1)

  result <- switch(
    method,
    radial     = .layout_radial(cx, cy, push),
    horizontal = .layout_linear(cx, cy, push, axis = "x"),
    vertical   = .layout_linear(cx, cy, push, axis = "y"),
    grid       = .layout_grid(cx, cy, push, n)
  )

  offsets$dx_m <- result$dx
  offsets$dy_m <- result$dy
  offsets
}

# ---- Internal layout algorithms ------------------------------------------------

.layout_radial <- function(cx, cy, push) {
  hub_x  <- mean(cx)
  hub_y  <- mean(cy)
  angles <- atan2(cy - hub_y, cx - hub_x)
  # Scale individual pushes by relative distance so already-spread regions
  # move further out
  dists  <- sqrt((cx - hub_x)^2 + (cy - hub_y)^2)
  max_d  <- max(dists, 1)
  factor <- 0.5 + 0.5 * dists / max_d  # range [0.5, 1.0]
  list(
    dx = cos(angles) * push * factor,
    dy = sin(angles) * push * factor
  )
}

.layout_linear <- function(cx, cy, push, axis = c("x", "y")) {
  axis <- match.arg(axis)
  vals <- if (axis == "x") cx else cy
  rank_v <- rank(vals, ties.method = "first")
  spread <- (rank_v - (length(vals) + 1L) / 2) * push
  list(
    dx = if (axis == "x") spread else rep(0, length(cx)),
    dy = if (axis == "y") spread else rep(0, length(cy))
  )
}

.layout_grid <- function(cx, cy, push, n) {
  n_cols <- max(1L, ceiling(sqrt(n)))
  n_rows <- ceiling(n / n_cols)
  # Order regions top-to-bottom, left-to-right by centroid
  ord <- order(-cy, cx)

  col_pos <- ((seq_len(n) - 1L) %% n_cols) - (n_cols - 1L) / 2
  row_pos <- ((seq_len(n) - 1L) %/% n_cols) - (n_rows - 1L) / 2

  dx_ord <- col_pos * push * 2
  dy_ord <- -row_pos * push * 2

  inv_ord <- order(ord)
  list(dx = dx_ord[inv_ord], dy = dy_ord[inv_ord])
}
