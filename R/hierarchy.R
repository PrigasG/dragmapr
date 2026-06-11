#' Build composite group keys from multiple columns
#'
#' Creates a character vector of composite group identifiers by combining values
#' from two or more columns of a data frame or `sf` object. The result is
#' suitable as the `region` column of an offset data frame, and is the key
#' building block for parent-to-child layout inheritance in hierarchical spatial
#' datasets where child names repeat across parents.
#'
#' When `length(cols) == 1`, the values of that column are returned directly
#' (after whitespace trimming and NA replacement). When `length(cols) > 1`,
#' values are joined as `"col1=val1 | col2=val2 | ..."`.
#'
#' @param x A data frame or `sf` object.
#' @param cols A character vector of column names to combine into a composite
#'   key. At least one column is required. Columns must exist in `x`.
#'
#' @return A character vector of length `nrow(x)`.
#' @export
#' @seealso [inherit_layout()] to propagate parent offsets to child groups,
#'   [create_layout_snapshot()] to capture a layout for later restoration.
#' @examples
#' df <- data.frame(
#'   county = c("Essex", "Essex", "Morris"),
#'   mun    = c("Fairfield", "Caldwell", "Fairfield")
#' )
#' # Single column: values returned as-is
#' make_hierarchy_key(df, "county")
#'
#' # Two columns: composite key disambiguates repeated child names
#' make_hierarchy_key(df, c("county", "mun"))
make_hierarchy_key <- function(x, cols) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  cols <- as.character(cols)
  if (length(cols) == 0L) {
    stop("`cols` must name at least one column.", call. = FALSE)
  }
  missing_cols <- setdiff(cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Column(s) not found in `x`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (length(cols) == 1L) return(.hierarchy_clean(x[[cols]]))
  parts <- lapply(cols, function(col) paste0(col, "=", .hierarchy_clean(x[[col]])))
  do.call(paste, c(parts, sep = " | "))
}

#' Inherit region offsets from a coarser to a finer grouping column
#'
#' When switching from a parent grouping (e.g., county) to a child grouping
#' (e.g., municipality), `inherit_layout()` maps the parent's drag offsets to
#' every child region that belongs to each parent. Each child inherits the mean
#' offset of the parent group it belongs to.
#'
#' When child names repeat across parents (e.g., "Fairfield" exists in both
#' Essex and Morris counties), pass both columns in `to` so that composite keys
#' are used: `to = c("county", "mun")`. The resulting `region` values will be
#' composite strings like `"county=Essex | mun=Fairfield"`, which can be passed
#' directly to `drag_map_prototype()` or `render_dragged_map()` via
#' `make_hierarchy_key()`.
#'
#' @param x An `sf` object or data frame containing both `from` and `to`
#'   columns.
#' @param from A character vector of one or more column names defining the
#'   coarser (parent) grouping. The `region` values in `parent_offsets` must
#'   match the keys produced by `make_hierarchy_key(x, from)`.
#' @param to A character vector of one or more column names defining the finer
#'   (child) grouping. The `region` column of the returned data frame will
#'   contain the keys produced by `make_hierarchy_key(x, to)`.
#' @param parent_offsets A data frame with `region`, `dx_m`, and `dy_m`
#'   columns keyed to the `from` grouping, as returned by [read_offsets()] or
#'   the Spatial Studio CSV export. Rows whose `region` does not match any
#'   parent key in `x` are silently ignored.
#'
#' @return A data frame with `region`, `dx_m`, and `dy_m` columns. Rows whose
#'   parent has no offset entry receive zero movement.
#' @export
#' @seealso [make_hierarchy_key()] for composite key construction,
#'   [create_layout_snapshot()] for capturing and restoring layouts.
#' @examples
#' poly <- sf::st_sf(
#'   county = c("Essex", "Essex", "Morris", "Morris"),
#'   mun    = c("Fairfield", "Caldwell", "Fairfield", "Roxbury"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     sf::st_polygon(list(rbind(c(1e5,1e5),c(2e5,1e5),c(2e5,2e5),c(1e5,2e5),c(1e5,1e5)))),
#'     sf::st_polygon(list(rbind(c(1e5,0),c(2e5,0),c(2e5,1e5),c(1e5,1e5),c(1e5,0)))),
#'     crs = 3857
#'   )
#' )
#' county_offsets <- data.frame(
#'   region = c("Essex", "Morris"),
#'   dx_m   = c(50000, -50000),
#'   dy_m   = c(0, 0)
#' )
#' # "Fairfield" repeats, so use composite to= key
#' mun_offsets <- inherit_layout(
#'   poly,
#'   from           = "county",
#'   to             = c("county", "mun"),
#'   parent_offsets = county_offsets
#' )
#' mun_offsets
inherit_layout <- function(x, from, to, parent_offsets) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  from <- as.character(from)
  to   <- as.character(to)
  missing_from <- setdiff(from, names(x))
  missing_to   <- setdiff(to,   names(x))
  if (length(missing_from) > 0L) {
    stop("Column(s) not found in `x` for `from`: ",
         paste(missing_from, collapse = ", "), call. = FALSE)
  }
  if (length(missing_to) > 0L) {
    stop("Column(s) not found in `x` for `to`: ",
         paste(missing_to, collapse = ", "), call. = FALSE)
  }
  parent_offsets <- normalize_offsets(parent_offsets, source = "`parent_offsets`")

  from_keys <- make_hierarchy_key(x, from)
  to_keys   <- make_hierarchy_key(x, to)

  match_idx <- match(from_keys, parent_offsets$region)
  row_dx    <- ifelse(is.na(match_idx), 0, parent_offsets$dx_m[match_idx])
  row_dy    <- ifelse(is.na(match_idx), 0, parent_offsets$dy_m[match_idx])
  row_dx[!is.finite(row_dx)] <- 0
  row_dy[!is.finite(row_dy)] <- 0

  out <- stats::aggregate(
    cbind(dx_m, dy_m) ~ region,
    data = data.frame(region = to_keys, dx_m = row_dx, dy_m = row_dy,
                      stringsAsFactors = FALSE),
    FUN = mean
  )
  out$region <- as.character(out$region)
  out[, c("region", "dx_m", "dy_m")]
}

#' Save a layout snapshot for a grouped sf dataset
#'
#' Captures the current drag offsets for a given grouping and returns a named
#' list. Snapshots can be passed to [inherit_layout()] when switching to a
#' finer grouping, or stored and later recovered with
#' [restore_layout_snapshot()].
#'
#' @param x A data frame or `sf` object.
#' @param group Column name (scalar character) defining the current grouping.
#' @param offsets A data frame with `region`, `dx_m`, and `dy_m` columns, a
#'   CSV path to such a file, or `NULL` for an all-zero starting state.
#'
#' @return A named list with elements `group` (character), `offsets` (data
#'   frame or `NULL`), and `timestamp` (POSIXct).
#' @export
#' @seealso [restore_layout_snapshot()], [inherit_layout()].
#' @examples
#' poly <- sf::st_sf(
#'   county = c("A", "B"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     crs = 3857
#'   )
#' )
#' offsets <- data.frame(region = c("A", "B"), dx_m = c(10000, -10000), dy_m = 0)
#' snap <- create_layout_snapshot(poly, group = "county", offsets = offsets)
#' snap$group
#' snap$offsets
create_layout_snapshot <- function(x, group, offsets = NULL) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  group <- as.character(group)
  if (length(group) != 1L || !nzchar(group)) {
    stop("`group` must be a single non-empty column name.", call. = FALSE)
  }
  if (!group %in% names(x)) {
    stop("Column '", group, "' not found in `x`.", call. = FALSE)
  }
  if (!is.null(offsets)) {
    if (is.character(offsets) && length(offsets) == 1L) {
      offsets <- read_offsets(offsets)
    }
    offsets <- normalize_offsets(offsets, source = "`offsets`")
  }
  list(group = group, offsets = offsets, timestamp = Sys.time())
}

#' Restore a layout snapshot
#'
#' Extracts the offset data frame from a snapshot created by
#' [create_layout_snapshot()].
#'
#' @param snapshot A named list as returned by [create_layout_snapshot()].
#'
#' @return A data frame with `region`, `dx_m`, and `dy_m` columns, or `NULL`
#'   if the snapshot was created with `offsets = NULL`.
#' @export
#' @seealso [create_layout_snapshot()].
#' @examples
#' offsets <- data.frame(region = c("A", "B"), dx_m = c(10000, -10000), dy_m = 0)
#' snap <- create_layout_snapshot(
#'   data.frame(county = c("A", "B")),
#'   group   = "county",
#'   offsets = offsets
#' )
#' restore_layout_snapshot(snap)
restore_layout_snapshot <- function(snapshot) {
  if (!is.list(snapshot) || !"offsets" %in% names(snapshot)) {
    stop(
      "`snapshot` must be a named list created by create_layout_snapshot().",
      call. = FALSE
    )
  }
  snapshot$offsets
}

# Internal: normalise a raw column to clean character values
.hierarchy_clean <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "(missing)"
  x
}
