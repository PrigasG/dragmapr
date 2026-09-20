#' Nudge a region or label offset
#'
#' Adds a small increment to the current offset of one region or label in a
#' [d_state()]. This is the scripted equivalent of dragging slightly in the
#' browser: existing movement is preserved and the delta is added on top.
#' Unknown ids gain a new row, matching [update_region_offset()].
#'
#' @param state A `dragmapr_state` object.
#' @param id Region id (when `target = "region"`) or label id (when
#'   `target = "label"`) to nudge.
#' @param dx_m,dy_m Distances in metres added to the current offset. Each must
#'   be a single finite number.
#' @param target One of `"region"` (default) or `"label"`.
#'
#' @return An updated `dragmapr_state` with a bumped version.
#' @export
#' @examples
#' state <- d_state(
#'   region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0)
#' )
#' state <- d_nudge(state, "A", dx_m = 5)
#' state$region_offsets
d_nudge <- function(state,
                    id,
                    dx_m = 0,
                    dy_m = 0,
                    target = c("region", "label")) {
  state <- validate_dragmapr_state(state)
  target <- match.arg(target)
  dx_m <- numeric_offset_scalar(dx_m, "dx_m")
  dy_m <- numeric_offset_scalar(dy_m, "dy_m")
  if (target == "region") {
    update_region_offset(
      state, region = id, dx_m = dx_m, dy_m = dy_m, mode = "increment"
    )
  } else {
    update_label_offset(
      state, label_id = id, dx_m = dx_m, dy_m = dy_m, mode = "increment"
    )
  }
}

#' Snap state offsets to a grid
#'
#' Rounds every region and label offset in a [d_state()] to the nearest
#' multiple of `grid`, tidying hand-dragged layouts into even increments.
#'
#' @param state A `dragmapr_state` object.
#' @param grid Single positive number in metres. Offsets snap to the nearest
#'   multiple of `grid`.
#'
#' @return An updated `dragmapr_state` with a bumped version.
#' @export
#' @examples
#' state <- d_state(
#'   region_offsets = data.frame(region = "A", dx_m = 1234, dy_m = -567)
#' )
#' state <- d_snap_offsets(state, grid = 1000)
#' state$region_offsets
d_snap_offsets <- function(state, grid = 1000) {
  state <- validate_dragmapr_state(state)
  if (!is.numeric(grid) || length(grid) != 1L || !is.finite(grid) || grid <= 0) {
    stop("`grid` must be a single positive number.", call. = FALSE)
  }
  snap <- function(x) round(x / grid) * grid
  state$region_offsets$dx_m <- snap(state$region_offsets$dx_m)
  state$region_offsets$dy_m <- snap(state$region_offsets$dy_m)
  state$label_offsets$dx_m <- snap(state$label_offsets$dx_m)
  state$label_offsets$dy_m <- snap(state$label_offsets$dy_m)
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}
