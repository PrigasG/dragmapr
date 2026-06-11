# Local elastic hierarchy transitions ------------------------------------------
#
# Strategy: "stable skeleton + local elastic insert".
# The parent layout stays put; children bloom from the clicked parent's
# centroid, overshoot slightly (easeOutBack), and settle into place. A dotted
# group-drag boundary belongs to the expanded group and moves the branch.
# Global relayout is deliberately unsupported: it destroys mental-map stability.

#' Options for local elastic hierarchy transitions
#'
#' Builds a validated list of animation and behaviour settings for
#' [build_elastic_transition()]. Defaults follow the user-tested values from
#' the dragmapr transition prototype: a 550 ms ease-out-back bloom, a dotted
#' group-drag boundary around the expanded group, and a stable parent
#' skeleton.
#'
#' @details
#' The transition model is always *stable skeleton + local elastic insert*:
#' the parent layout is preserved and children expand locally from the parent
#' centroid. `global_relayout` exists only as an explicit guard rail; setting
#' it to `TRUE` is an error because recomputing the whole layout on every
#' expansion destroys the user's mental map.
#'
#' @param mode Transition style. `"local_elastic"` (default) blooms children
#'   from the parent centroid with elastic overshoot; `"local_insert"` is an
#'   alias kept for forward compatibility and currently behaves identically.
#' @param duration_ms Single positive number. Total animation duration in
#'   milliseconds. Defaults to `550`.
#' @param overshoot Single non-negative number controlling the elastic
#'   "snap" of the ease-out-back curve. `0` disables overshoot entirely;
#'   the default `1.70158` gives the classic overshoot of about 10 percent.
#' @param max_stretch Single positive number. Upper cap on the per-feature
#'   stretch strength (movement distance relative to the layout diagonal).
#'   Defaults to `0.35`.
#' @param n_frames Single integer (>= 2). Number of animation frames produced
#'   for static/`ggplot2` playback. Defaults to `30`.
#' @param preserve_skeleton Logical. Keep the parent layout stable while a
#'   branch expands. Must remain `TRUE`; included so the contract is explicit
#'   and machine-readable.
#' @param global_relayout Logical. Must be `FALSE`. Provided only so that
#'   attempts to opt into a Dagre-style full relayout fail with a clear
#'   explanation.
#' @param show_ghost Logical. Render the collapsed parent as a faint ghost
#'   behind its expanded children. Defaults to `TRUE`.
#' @param show_trails Logical. Render dotted movement trails from the parent
#'   centroid to each child's final position. Defaults to `FALSE`.
#' @param reset_boundary Logical. Legacy alias for `group_drag_frame`, kept
#'   for backwards compatibility. Defaults to `TRUE`.
#' @param group_drag_frame Logical. Generate the dotted group-drag boundary
#'   around each expanded group. Defaults to the value of `reset_boundary`.
#' @param boundary_behavior What a plain click on the dotted frame does in
#'   the interactive helper: `"drag"` (default; the frame is a group drag
#'   handle and a click compresses the branch), `"reset"` (legacy reset
#'   behaviour), or `"none"`. Forced to `"none"` when `group_drag_frame`
#'   is `FALSE`.
#' @param boundary_drag_threshold Single non-negative number. Pointer
#'   movement (in pixels) below which a frame gesture counts as a click
#'   rather than a drag. Defaults to `8`.
#' @param boundary_label Single string used as the helper label for the
#'   dotted group frame. Defaults to `"Drag to"`, allowing the interactive
#'   helper to show labels such as `"Drag to Essex"`.
#' @param collapse_duration_ms Single positive number. Browser-side reversible
#'   leaf-return animation duration in milliseconds. Defaults to `260`.
#' @param collapse_visual_freeze_ms Single non-negative number. How long the
#'   Studio should defer label and legend refresh messages while collapsing.
#'   Defaults to `340`.
#' @param collapse_scale Number from `0` to `1`. Final scale of children during
#'   a legacy collapse animation. Defaults to `0.16`.
#' @param collapse_opacity Number from `0` to `1`. Final opacity of children
#'   during a legacy collapse animation. Defaults to `0.04`.
#' @param boundary_padding Single positive number. Boundary padding as a
#'   fraction of the child layout's bounding-box diagonal. Defaults to
#'   `0.025` (2.5 percent).
#' @param drag_boundary_with_group Logical. The group-drag boundary is part of the
#'   expanded group and must move with it when dragged; it is never a static
#'   overlay. Defaults to `TRUE`.
#'
#' @return A named list of class `"dragmapr_transition_options"`.
#' @export
#' @seealso [build_elastic_transition()], [make_group_boundaries()].
#' @examples
#' transition_options()
#' transition_options(duration_ms = 800, overshoot = 0)
#' try(transition_options(global_relayout = TRUE))
transition_options <- function(mode = c("local_elastic", "local_insert"),
                               duration_ms = 550,
                               overshoot = 1.70158,
                               max_stretch = 0.35,
                               n_frames = 30,
                               preserve_skeleton = TRUE,
                               global_relayout = FALSE,
                               show_ghost = TRUE,
                               show_trails = FALSE,
                               reset_boundary = TRUE,
                               group_drag_frame = reset_boundary,
                               boundary_behavior = c("drag", "reset", "none"),
                               boundary_drag_threshold = 8,
                               boundary_label = "Drag to",
                               collapse_duration_ms = 260,
                               collapse_visual_freeze_ms = 340,
                               collapse_scale = 0.16,
                               collapse_opacity = 0.04,
                               boundary_padding = 0.025,
                               drag_boundary_with_group = TRUE) {
  mode <- match.arg(mode)
  boundary_behavior <- match.arg(boundary_behavior)

  if (isTRUE(global_relayout)) {
    stop(
      "Global relayout is not supported: recomputing the whole layout on ",
      "every expansion breaks mental-map stability. Keep `global_relayout ",
      "= FALSE`; dragmapr always uses a stable skeleton with local elastic ",
      "insertion.",
      call. = FALSE
    )
  }

  if (!isTRUE(preserve_skeleton)) {
    stop(
      "`preserve_skeleton` must be TRUE. The parent layout is the user's ",
      "mental map; only the expanded branch may move.",
      call. = FALSE
    )
  }

  .check_number(duration_ms, "duration_ms", min = 1)
  .check_number(overshoot, "overshoot", min = 0)
  .check_number(max_stretch, "max_stretch", positive = TRUE)
  .check_number(n_frames, "n_frames", min = 2)

  if (n_frames != as.integer(n_frames)) {
    stop("`n_frames` must be a whole number (e.g. 30).", call. = FALSE)
  }

  .check_number(boundary_drag_threshold, "boundary_drag_threshold", min = 0)
  if (!is.character(boundary_label) || length(boundary_label) != 1L ||
      is.na(boundary_label)) {
    stop("`boundary_label` must be a single string.", call. = FALSE)
  }
  .check_number(collapse_duration_ms, "collapse_duration_ms", min = 1)
  .check_number(collapse_visual_freeze_ms, "collapse_visual_freeze_ms", min = 0)
  .check_number(collapse_scale, "collapse_scale", min = 0)
  .check_number(collapse_opacity, "collapse_opacity", min = 0)
  if (collapse_scale > 1) {
    stop("`collapse_scale` must be between 0 and 1.", call. = FALSE)
  }
  if (collapse_opacity > 1) {
    stop("`collapse_opacity` must be between 0 and 1.", call. = FALSE)
  }
  .check_number(boundary_padding, "boundary_padding", positive = TRUE)

  for (flag in c(
    "show_ghost", "show_trails", "reset_boundary",
    "group_drag_frame", "drag_boundary_with_group"
  )) {
    val <- get(flag, inherits = FALSE)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      stop("`", flag, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  if (!isTRUE(group_drag_frame)) {
    boundary_behavior <- "none"
  }

  structure(
    list(
      mode                     = mode,
      duration_ms              = duration_ms,
      overshoot                = overshoot,
      max_stretch              = max_stretch,
      n_frames                 = as.integer(n_frames),
      preserve_skeleton        = TRUE,
      global_relayout          = FALSE,
      show_ghost               = show_ghost,
      show_trails              = show_trails,

      # Legacy name retained for older code.
      reset_boundary           = isTRUE(group_drag_frame),

      # New name and behavior.
      group_drag_frame         = isTRUE(group_drag_frame),
      boundary_behavior        = boundary_behavior,
      boundary_drag_threshold  = boundary_drag_threshold,
      boundary_label           = unname(boundary_label),
      collapse_duration_ms     = collapse_duration_ms,
      collapse_visual_freeze_ms = collapse_visual_freeze_ms,
      collapse_scale           = collapse_scale,
      collapse_opacity         = collapse_opacity,

      boundary_padding         = boundary_padding,
      drag_boundary_with_group = drag_boundary_with_group
    ),
    class = "dragmapr_transition_options"
  )
}

#' Build a local elastic parent-to-child transition
#'
#' Prepares everything needed to animate child regions blooming out of their
#' parent region: per-child anchor and final positions, relative motion
#' strengths, ready-to-plot animation frames, and the dotted group-drag
#' boundary for each expanded group. The parent layout itself is never moved
#' (stable skeleton); only the expanded branch animates.
#'
#' @details
#' Children whose parent id has no match in `parent_sf` fall back to their own
#' centroid as the anchor (they fade in place instead of blooming), and a
#' warning lists the unmatched ids so data problems are visible early.
#'
#' The returned `frames` interpolate each child from a small seed at the
#' parent centroid (frame 1) to its exact final geometry (last frame) using an
#' ease-out-back curve, so the last frame equals `child_sf` and can be used as
#' the settled state.
#'
#' @param child_sf An `sf` object with the child regions in their final
#'   (expanded) positions.
#' @param parent_sf An `sf` object with the parent regions.
#' @param parent_col Column in `child_sf` holding each child's parent id.
#' @param parent_id_col Column in `parent_sf` holding the parent id. Defaults
#'   to `parent_col`, which covers the common case where both layers share the
#'   same column name (e.g. `"COUNTY"`).
#' @param options A list created by [transition_options()].
#'
#' @return A list of class `"dragmapr_transition"` with elements:
#' \describe{
#'   \item{`anchored`}{`child_sf` plus `anchor_x`, `anchor_y`, `final_x`,
#'     `final_y`, `move_dist`, `move_ratio`, `stretch_strength`, and
#'     `duration_ms` columns.}
#'   \item{`frames`}{An `sf` object of `n_frames` stacked copies of the
#'     children with `frame_id`, `frame_progress`, and `frame_eased` columns
#'     and interpolated geometry, suitable for `ggplot2` playback or export.}
#'   \item{`boundaries`}{The dotted group-drag boundary per parent group
#'     from [make_group_boundaries()], or `NULL` when
#'     `options$group_drag_frame` is `FALSE`.}
#'   \item{`options`}{The validated options used.}
#' }
#' @export
#' @seealso [transition_options()], [make_group_boundaries()],
#'   [layout_metrics()], [inherit_layout()] for carrying drag offsets from
#'   parent to child groupings.
#' @examples
#' rect <- function(x0, x1, y0, y1) {
#'   sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
#' }
#' parents <- sf::st_sf(
#'   county = c("A", "B"),
#'   geometry = sf::st_sfc(rect(0, 3, 0, 3), rect(4, 7, 0, 3), crs = 3857)
#' )
#' children <- sf::st_sf(
#'   county = c("A", "A", "B", "B"),
#'   mun    = c("A1", "A2", "B1", "B2"),
#'   geometry = sf::st_sfc(
#'     rect(-1, 1, 0, 3), rect(2, 4, 0, 3),
#'     rect(3, 5, 0, 3), rect(6, 8, 0, 3),
#'     crs = 3857
#'   )
#' )
#' tr <- build_elastic_transition(
#'   children, parents,
#'   parent_col = "county",
#'   options    = transition_options(n_frames = 5)
#' )
#' names(tr)
#' head(tr$anchored[, c("county", "mun", "anchor_x", "final_x")])
#' tr$boundaries
build_elastic_transition <- function(child_sf,
                                     parent_sf,
                                     parent_col,
                                     parent_id_col = parent_col,
                                     options = transition_options()) {
  .check_sf(child_sf, "child_sf")
  .check_sf(parent_sf, "parent_sf")
  .check_column(child_sf, parent_col, "parent_col", "child_sf")
  .check_column(parent_sf, parent_id_col, "parent_id_col", "parent_sf")
  if (!inherits(options, "dragmapr_transition_options")) {
    stop(
      "`options` must be created by transition_options(), e.g. ",
      "build_elastic_transition(..., options = transition_options()).",
      call. = FALSE
    )
  }

  child_crs  <- sf::st_crs(child_sf)
  parent_crs <- sf::st_crs(parent_sf)
  if (!is.na(child_crs) && !is.na(parent_crs) && child_crs != parent_crs) {
    stop(
      "`child_sf` and `parent_sf` use different coordinate reference ",
      "systems. Transform one first, e.g. parent_sf <- sf::st_transform(",
      "parent_sf, sf::st_crs(child_sf)).",
      call. = FALSE
    )
  }

  anchored <- .transition_anchors(child_sf, parent_sf, parent_col, parent_id_col)
  anchored <- .transition_motion(anchored, options)

  frames <- .transition_frames(
    anchored,
    n_frames  = options$n_frames,
    overshoot = options$overshoot
  )

  boundaries <- NULL
  if (isTRUE(options$group_drag_frame)) {
    boundaries <- make_group_boundaries(
      child_sf,
      group_col = parent_col,
      padding   = .bbox_diagonal(child_sf) * options$boundary_padding
    )
  }

  structure(
    list(
      anchored   = anchored,
      frames     = frames,
      boundaries = boundaries,
      options    = options
    ),
    class = "dragmapr_transition"
  )
}

#' Build dotted group-drag boundaries for expanded groups
#'
#' Computes one padded rectangular boundary per group. The boundary is returned as real geometry with the group's id so
#' it can be drawn inside the same container as the children and therefore
#' moves with the group when dragged. In Spatial Studio this boundary is a
#' drag handle, not a collapse control.
#'
#' @param sf_obj An `sf` object containing the (expanded) child regions.
#' @param group_col Column in `sf_obj` identifying the expanded group, usually
#'   the parent id column.
#' @param padding Single positive number in map units, or `NULL` (default) to
#'   use 2.5 percent of the bounding-box diagonal of `sf_obj`.
#'
#' @return An `sf` object with one row per group and columns `group_id`,
#'   `xmin`, `xmax`, `ymin`, `ymax` (padded bounds), plus the rectangle
#'   geometry.
#' @export
#' @seealso [build_elastic_transition()], which calls this automatically when
#'   `group_drag_frame = TRUE`.
#' @examples
#' rect <- function(x0, x1, y0, y1) {
#'   sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
#' }
#' children <- sf::st_sf(
#'   county = c("A", "A", "B"),
#'   geometry = sf::st_sfc(
#'     rect(0, 1, 0, 1), rect(2, 3, 0, 1), rect(5, 6, 0, 1),
#'     crs = 3857
#'   )
#' )
#' make_group_boundaries(children, group_col = "county")
make_group_boundaries <- function(sf_obj, group_col, padding = NULL) {
  .check_sf(sf_obj, "sf_obj")
  .check_column(sf_obj, group_col, "group_col", "sf_obj")
  if (is.null(padding)) {
    padding <- .bbox_diagonal(sf_obj) * 0.025
  }
  .check_number(padding, "padding", min = 0)

  ids <- as.character(sf_obj[[group_col]])
  groups <- split(seq_len(nrow(sf_obj)), ids)

  rows <- lapply(names(groups), function(g) {
    bb <- sf::st_bbox(sf_obj[groups[[g]], , drop = FALSE])
    xmin <- unname(bb["xmin"]) - padding
    xmax <- unname(bb["xmax"]) + padding
    ymin <- unname(bb["ymin"]) - padding
    ymax <- unname(bb["ymax"]) + padding
    rect <- sf::st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin),
      c(ymin, ymin, ymax, ymax, ymin)
    )))
    sf::st_sf(
      group_id = g,
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      geometry = sf::st_sfc(rect, crs = sf::st_crs(sf_obj)),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Measure mental-map stability between two layouts
#'
#' Compares region centroids before and after a transition and reports how far
#' regions drifted. Use it to confirm that expanding one branch left the rest
#' of the map stable: pass the non-expanded (sibling) regions as `before` and
#' `after`. Lower drift means the user's mental map survived.
#'
#' @param before An `sf` object with the layout before the transition.
#' @param after An `sf` object with the layout after the transition.
#' @param id_col Column present in both objects identifying each region.
#'
#' @return A named list with `mean_drift` and `max_drift` (map units),
#'   `stability` (0-100; 100 means no drift, 0 means mean drift of one third
#'   of the `before` bounding-box diagonal or more), and `n_matched` (regions
#'   compared). Regions missing from `after` are dropped with a warning.
#' @export
#' @seealso [build_elastic_transition()].
#' @examples
#' rect <- function(x0, x1, y0, y1) {
#'   sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
#' }
#' before <- sf::st_sf(
#'   region = c("A", "B"),
#'   geometry = sf::st_sfc(rect(0, 1, 0, 1), rect(2, 3, 0, 1), crs = 3857)
#' )
#' after <- before
#' sf::st_geometry(after)[2] <- sf::st_geometry(after)[[2]] + c(0.5, 0)
#' layout_metrics(before, after, id_col = "region")
layout_metrics <- function(before, after, id_col) {
  .check_sf(before, "before")
  .check_sf(after, "after")
  .check_column(before, id_col, "id_col", "before")
  .check_column(after, id_col, "id_col", "after")

  before_ids <- as.character(before[[id_col]])
  after_ids  <- as.character(after[[id_col]])
  idx <- match(before_ids, after_ids)

  if (anyNA(idx)) {
    missing_ids <- unique(before_ids[is.na(idx)])
    warning(
      "Dropping ", length(missing_ids), " region(s) not present in `after`: ",
      paste(utils::head(missing_ids, 5L), collapse = ", "),
      if (length(missing_ids) > 5L) ", ..." else "",
      call. = FALSE
    )
  }
  keep <- which(!is.na(idx))
  if (length(keep) == 0L) {
    stop(
      "No regions in `before` match `after` on column '", id_col, "'. ",
      "Check that both layouts use the same id values.",
      call. = FALSE
    )
  }

  b_xy <- .centroid_xy(before[keep, , drop = FALSE])
  a_xy <- .centroid_xy(after[idx[keep], , drop = FALSE])
  drift <- sqrt((a_xy[, 1] - b_xy[, 1])^2 + (a_xy[, 2] - b_xy[, 2])^2)

  diag <- .bbox_diagonal(before)
  if (!is.finite(diag) || diag <= 0) diag <- 1
  mean_drift <- mean(drift)

  list(
    mean_drift = mean_drift,
    max_drift  = max(drift),
    stability  = max(0, round(100 * (1 - (mean_drift / diag) * 3))),
    n_matched  = length(keep)
  )
}

# ---- Internal helpers --------------------------------------------------------

# Ease-out-back: settles at 1 with a slight elastic overshoot on the way.
#' @noRd
.ease_out_back <- function(t, overshoot = 1.70158) {
  t <- t - 1
  1 + t * t * ((overshoot + 1) * t + overshoot)
}

# Diagonal of the bounding box, used as the scale-free size reference.
#' @noRd
.bbox_diagonal <- function(x) {
  bb <- sf::st_bbox(x)
  width  <- unname(bb["xmax"] - bb["xmin"])
  height <- unname(bb["ymax"] - bb["ymin"])
  sqrt(width^2 + height^2)
}

# Centroid coordinates without lon-lat warnings for planar demo data.
#' @noRd
.centroid_xy <- function(x) {
  suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(x))))
}

# Attach anchor (parent centroid) and final (own centroid) positions.
#' @noRd
.transition_anchors <- function(child_sf, parent_sf, parent_col, parent_id_col) {
  child_xy  <- .centroid_xy(child_sf)
  parent_xy <- .centroid_xy(parent_sf)

  child_sf$final_x <- child_xy[, 1]
  child_sf$final_y <- child_xy[, 2]

  idx <- match(
    as.character(child_sf[[parent_col]]),
    as.character(parent_sf[[parent_id_col]])
  )
  child_sf$anchor_x <- parent_xy[idx, 1]
  child_sf$anchor_y <- parent_xy[idx, 2]

  unmatched <- is.na(idx)
  if (any(unmatched)) {
    bad_ids <- unique(as.character(child_sf[[parent_col]])[unmatched])
    warning(
      "No parent found in `parent_sf` for ", length(bad_ids), " value(s) of '",
      parent_col, "': ",
      paste(utils::head(bad_ids, 5L), collapse = ", "),
      if (length(bad_ids) > 5L) ", ..." else "",
      ". These children anchor to their own centroid instead of blooming.",
      call. = FALSE
    )
    child_sf$anchor_x[unmatched] <- child_sf$final_x[unmatched]
    child_sf$anchor_y[unmatched] <- child_sf$final_y[unmatched]
  }

  child_sf
}

# Per-feature movement distance, relative strength, and duration.
#' @noRd
.transition_motion <- function(anchored, options) {
  diag <- .bbox_diagonal(anchored)
  if (!is.finite(diag) || diag <= 0) diag <- 1

  anchored$move_dist <- sqrt(
    (anchored$final_x - anchored$anchor_x)^2 +
      (anchored$final_y - anchored$anchor_y)^2
  )
  anchored$move_ratio       <- anchored$move_dist / diag
  anchored$stretch_strength <- pmin(options$max_stretch, anchored$move_ratio)
  anchored$duration_ms      <- rep(options$duration_ms, nrow(anchored))
  anchored
}

# Stacked animation frames: children grow from a small seed at the parent
# centroid (frame 1) to their exact final geometry (last frame).
#' @noRd
.transition_frames <- function(anchored, n_frames, overshoot,
                               start_scale = 0.15) {
  geoms <- sf::st_geometry(anchored)
  crs <- sf::st_crs(anchored)
  progress <- seq(0, 1, length.out = n_frames)

  frames <- vector("list", n_frames)
  for (i in seq_len(n_frames)) {
    t <- progress[i]
    eased  <- .ease_out_back(t, overshoot)
    growth <- start_scale + (1 - start_scale) * min(t * 1.08, 1)

    frame_sf <- anchored
    frame_sf$frame_id       <- i
    frame_sf$frame_progress <- t
    frame_sf$frame_eased    <- eased

    pos_x <- anchored$anchor_x + (anchored$final_x - anchored$anchor_x) * eased
    pos_y <- anchored$anchor_y + (anchored$final_y - anchored$anchor_y) * eased

    new_geom <- vector("list", length(geoms))
    for (j in seq_along(geoms)) {
      centred <- (geoms[[j]] - c(anchored$final_x[j], anchored$final_y[j])) *
        growth
      new_geom[[j]] <- centred + c(pos_x[j], pos_y[j])
    }
    sf::st_geometry(frame_sf) <- sf::st_sfc(new_geom, crs = crs)
    frames[[i]] <- frame_sf
  }

  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

# ---- Internal validation -----------------------------------------------------

#' @noRd
.check_sf <- function(x, arg) {
  if (!inherits(x, "sf")) {
    stop(
      "`", arg, "` must be an sf object. ",
      "Read spatial files with sf::st_read() or prepare_dragmapr_sf().",
      call. = FALSE
    )
  }
  if (nrow(x) == 0L) {
    stop("`", arg, "` has no rows.", call. = FALSE)
  }
  invisible(x)
}

#' @noRd
.check_column <- function(x, col, arg, data_arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a single column name.", call. = FALSE)
  }
  if (!col %in% names(x)) {
    stop(
      "Column '", col, "' not found in `", data_arg, "`. Available columns: ",
      paste(setdiff(names(x), attr(x, "sf_column")), collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(col)
}

#' @noRd
.check_number <- function(x, arg, min = -Inf, positive = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && is.finite(x) &&
    (!positive || x > 0) && x >= min
  if (!ok) {
    stop(
      "`", arg, "` must be a single ",
      if (positive) "positive " else "",
      "finite number",
      if (is.finite(min)) paste0(" of at least ", format(min)) else "",
      ".",
      call. = FALSE
    )
  }
  invisible(x)
}
