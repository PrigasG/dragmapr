#' Derive one default label per draggable region
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for label text. Defaults to `region_col`.
#' @param point One of `"point_on_surface"` or `"centroid"`.
#'
#' @return A drag label table with `label_id`, `region`, `label`, `x`, and `y`.
#' @seealso [as_drag_labels()] for user-supplied labels; [as_drag_annotations()]
#'   for draggable info boxes; [apply_label_state()] to restore saved positions;
#'   [drag_map_prototype()] which accepts the result as its `labels` argument.
#' @export
#' @examples
#' poly <- sf::st_sf(
#'   region = c("North", "South"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     crs = 3857
#'   )
#' )
#' make_region_labels(poly, region_col = "region")
make_region_labels <- function(x,
                               region_col,
                               label_col = region_col,
                               point = c("point_on_surface", "centroid")) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  if (!label_col %in% names(x)) {
    stop("label_col '", label_col, "' not found.", call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop(
      "Project `x` before deriving label anchors in plot coordinates. ",
      "Use prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857) ",
      "to convert to a projected CRS first.",
      call. = FALSE
    )
  }
  point <- match.arg(point)

  regions <- natural_sort(unique(as.character(x[[region_col]])))
  rows <- lapply(regions, function(region) {
    idx <- as.character(x[[region_col]]) == region
    geom <- sf::st_union(sf::st_geometry(x)[idx])
    anchor <- if (point == "centroid") {
      sf::st_centroid(geom)
    } else {
      sf::st_point_on_surface(geom)
    }
    xy <- sf::st_coordinates(anchor)[1, ]
    labels <- unique(as.character(x[[label_col]][idx]))
    label <- labels[!is.na(labels) & nzchar(labels)][1]
    if (is.na(label) || !nzchar(label)) {
      label <- region
    }
    data.frame(
      label_id = region,
      region = region,
      label = label,
      x = xy[[1]],
      y = xy[[2]],
      stringsAsFactors = FALSE
    )
  })

  as_drag_labels(do.call(rbind, rows))
}

#' Coerce data to a drag label table
#'
#' Use this for user-supplied labels. Extra columns are preserved so interactive
#' applications can carry styling, tooltip, or grouping metadata alongside the
#' core label position columns.
#'
#' @param labels A data frame with `label_id`, `region`, `label`, `x`, and `y`.
#'   Optional columns include `label_type` (`"label"` or `"box"`),
#'   `width_px` and `height_px` for draggable info boxes, and `connector`,
#'   `connector_type`, `connector_start_x`, `connector_start_y`,
#'   `connector_mid_x`, and `connector_mid_y` for leader lines.
#'
#' @return A normalized label data frame.
#' @export
#' @examples
#' as_drag_labels(data.frame(
#'   label_id = "note-1", region = "North", label = "Check this",
#'   x = 50000, y = 150000, tooltip = "extra metadata"
#' ))
as_drag_labels <- function(labels) {
  normalize_labels(labels)
}

#' Coerce data to draggable annotation boxes
#'
#' Annotation boxes use the same position and state workflow as labels, but
#' render as larger text boxes for notes, callouts, or location descriptions.
#'
#' @param labels A data frame with `label_id`, `region`, `label`, `x`, and `y`.
#' @param width_px,height_px Default browser box dimensions for rows that do not
#'   already include `width_px` or `height_px`.
#' @param connector Add connector lines from anchors to annotation boxes.
#' @param connector_type One of `"straight"`, `"elbow"`, `"curve"`, or
#'   `"squiggle"`.
#'
#' @return A normalized label data frame with `label_type = "box"`.
#' @export
#' @examples
#' as_drag_annotations(data.frame(
#'   label_id = "note-1", region = "North",
#'   label = "This note can hold more text than a short label.",
#'   x = 50000, y = 150000
#' ))
as_drag_annotations <- function(labels,
                                width_px = 150,
                                height_px = 72,
                                connector = FALSE,
                                connector_type = c("straight", "elbow", "curve", "squiggle")) {
  if (!is.data.frame(labels)) {
    stop("`labels` must be a data frame before it can be used as annotation boxes.", call. = FALSE)
  }
  if (!is.numeric(width_px) || length(width_px) != 1L || !is.finite(width_px) || width_px <= 0) {
    stop("`width_px` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(height_px) || length(height_px) != 1L || !is.finite(height_px) || height_px <= 0) {
    stop("`height_px` must be a positive number.", call. = FALSE)
  }
  if (!is.logical(connector) || length(connector) != 1L || is.na(connector)) {
    stop("`connector` must be TRUE or FALSE.", call. = FALSE)
  }
  connector_type <- match.arg(connector_type)
  if (!"label_type" %in% tolower(names(labels))) {
    labels$label_type <- "box"
  }
  if (!"width_px" %in% tolower(names(labels))) {
    labels$width_px <- width_px
  }
  if (!"height_px" %in% tolower(names(labels))) {
    labels$height_px <- height_px
  }
  # Always apply the caller's connector/connector_type values. normalize_labels
  # (called upstream by make_region_labels) pre-fills these columns with FALSE /
  # "straight", so the "column absent" guard would silently drop the explicit
  # argument. Unconditional assignment is the correct semantic here - the caller
  # explicitly chose these values for the annotation boxes.
  labels$connector      <- connector
  labels$connector_type <- connector_type
  out <- normalize_labels(labels)
  out$label_type <- "box"
  missing_width <- !is.finite(out$width_px) | out$width_px <= 0
  missing_height <- !is.finite(out$height_px) | out$height_px <= 0
  out$width_px[missing_width] <- width_px
  out$height_px[missing_height] <- height_px
  normalize_labels(out)
}

#' Read draggable label state from CSV
#'
#' @param path Path to a CSV with `label_id`, `region`, `dx_m`, and `dy_m`.
#'
#' @return A normalized label state data frame.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeLines(c("label_id,region,dx_m,dy_m", "North,North,500,200"), path)
#' read_label_state(path)
read_label_state <- function(path) {
  state <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  normalize_label_state(state, source = "Label state file")
}

#' Apply draggable label state to a label table
#'
#' @param labels A data frame from `make_region_labels()` or `as_drag_labels()`.
#' @param label_state A data frame with `label_id`, `region`, `dx_m`, and `dy_m`,
#'   a CSV path, or `NULL`.
#'
#' @return A label data frame with adjusted `x` and `y`.
#' @export
#' @examples
#' labels <- as_drag_labels(data.frame(
#'   label_id = "North", region = "North", label = "North",
#'   x = 50000, y = 150000, stringsAsFactors = FALSE
#' ))
#' state <- data.frame(
#'   label_id = "North", region = "North", dx_m = 5000, dy_m = -2000,
#'   stringsAsFactors = FALSE
#' )
#' apply_label_state(labels, state)
apply_label_state <- function(labels, label_state = NULL) {
  labels <- normalize_labels(labels)
  if (is.null(label_state)) {
    return(labels)
  }
  if (is.character(label_state) && length(label_state) == 1L) {
    label_state <- read_label_state(label_state)
  }
  if (!is.data.frame(label_state)) {
    stop("`label_state` must be a data frame, CSV path, or NULL.", call. = FALSE)
  }
  label_state <- normalize_label_state(label_state, source = "`label_state`")

  match_idx <- match(labels$label_id, label_state$label_id)
  has_state <- !is.na(match_idx)
  labels$x[has_state] <- labels$x[has_state] + label_state$dx_m[match_idx[has_state]]
  labels$y[has_state] <- labels$y[has_state] + label_state$dy_m[match_idx[has_state]]
  labels
}

#' Derive one label anchor per draggable region
#'
#' `make_labels()` is retained as an alias for [make_region_labels()].
#'
#' @inheritParams make_region_labels
#' @return A drag label table.
#' @export
make_labels <- function(x,
                        region_col,
                        label_col = region_col,
                        point = c("point_on_surface", "centroid")) {
  make_region_labels(x, region_col = region_col, label_col = label_col, point = point)
}

#' Read label offsets from CSV
#'
#' `read_label_offsets()` is retained as an alias for [read_label_state()].
#'
#' @inheritParams read_label_state
#' @return A normalized label state data frame.
#' @export
read_label_offsets <- function(path) {
  read_label_state(path)
}

#' Apply label-specific offsets to a label table
#'
#' `apply_label_offsets()` is retained as an alias for [apply_label_state()].
#'
#' @param labels A data frame from `make_region_labels()` or `as_drag_labels()`.
#' @param label_offsets A data frame with `label_id`, `region`, `dx_m`, and
#'   `dy_m`, a CSV path, or `NULL`.
#' @return A label data frame with adjusted `x` and `y`.
#' @export
apply_label_offsets <- function(labels, label_offsets = NULL) {
  apply_label_state(labels, label_offsets)
}

# ---- Internal natural-sort helpers ----------------------------------------
# Sort character vectors so embedded integers are ordered numerically:
#   "1", "2", ..., "10"  rather than  "1", "10", "2", ...
# These are package-internal only (not exported).

natural_sort_key_r <- function(x) {
  vapply(as.character(x), function(value) {
    m <- gregexpr("[0-9]+", value, perl = TRUE)[[1L]]
    if (m[1L] < 0L) return(value)
    nums <- regmatches(value, list(m))[[1L]]
    padded <- sprintf("%020.0f", as.numeric(nums))
    regmatches(value, list(m)) <- list(padded)
    value
  }, character(1L), USE.NAMES = FALSE)
}

natural_sort <- function(x) {
  x[order(natural_sort_key_r(as.character(x)), method = "radix")]
}

# ---------------------------------------------------------------------------

normalize_labels <- function(labels) {
  if (!is.data.frame(labels)) {
    stop("`labels` must be a data frame with columns label_id, region, label, x, and y.", call. = FALSE)
  }
  original_names <- names(labels)
  names(labels) <- tolower(names(labels))
  required <- c("label_id", "region", "label", "x", "y")
  missing <- setdiff(required, names(labels))
  if (length(missing) > 0L) {
    stop("`labels` is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  labels$label_id <- trimws(as.character(labels$label_id))
  labels$region <- trimws(as.character(labels$region))
  labels$label <- as.character(labels$label)
  labels$x <- suppressWarnings(as.numeric(labels$x))
  labels$y <- suppressWarnings(as.numeric(labels$y))
  if (!"label_type" %in% names(labels)) {
    labels$label_type <- rep("label", nrow(labels))
    original_names <- c(original_names, "label_type")
  }
  labels$label_type <- trimws(tolower(as.character(labels$label_type)))
  labels$label_type[is.na(labels$label_type) | labels$label_type == ""] <- "label"
  if (!"width_px" %in% names(labels)) {
    labels$width_px <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "width_px")
  }
  if (!"height_px" %in% names(labels)) {
    labels$height_px <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "height_px")
  }
  if (!"connector" %in% names(labels)) {
    labels$connector <- rep(FALSE, nrow(labels))
    original_names <- c(original_names, "connector")
  }
  if (!"connector_type" %in% names(labels)) {
    labels$connector_type <- rep("straight", nrow(labels))
    original_names <- c(original_names, "connector_type")
  }
  if (!"connector_mid_x" %in% names(labels)) {
    labels$connector_mid_x <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "connector_mid_x")
  }
  if (!"connector_mid_y" %in% names(labels)) {
    labels$connector_mid_y <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "connector_mid_y")
  }
  if (!"connector_start_x" %in% names(labels)) {
    labels$connector_start_x <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "connector_start_x")
  }
  if (!"connector_start_y" %in% names(labels)) {
    labels$connector_start_y <- rep(NA_real_, nrow(labels))
    original_names <- c(original_names, "connector_start_y")
  }
  labels$width_px <- suppressWarnings(as.numeric(labels$width_px))
  labels$height_px <- suppressWarnings(as.numeric(labels$height_px))
  labels$connector <- parse_logical_column(labels$connector, "`labels$connector`")
  labels$connector_type <- trimws(tolower(as.character(labels$connector_type)))
  labels$connector_type[is.na(labels$connector_type) | labels$connector_type == ""] <- "straight"
  labels$connector_start_x <- suppressWarnings(as.numeric(labels$connector_start_x))
  labels$connector_start_y <- suppressWarnings(as.numeric(labels$connector_start_y))
  labels$connector_mid_x <- suppressWarnings(as.numeric(labels$connector_mid_x))
  labels$connector_mid_y <- suppressWarnings(as.numeric(labels$connector_mid_y))
  names(labels) <- tolower(original_names)

  if (any(is.na(labels$label_id) | labels$label_id == "")) {
    stop("`labels` contains empty label_id values.", call. = FALSE)
  }
  if (anyDuplicated(labels$label_id)) {
    dupes <- unique(labels$label_id[duplicated(labels$label_id)])
    stop("`labels` contains duplicate label_id(s): ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(labels$region) | labels$region == "")) {
    stop("`labels` contains empty region values.", call. = FALSE)
  }
  if (any(!is.finite(labels$x) | !is.finite(labels$y))) {
    stop("`labels` contains non-numeric or missing coordinates.", call. = FALSE)
  }
  bad_type <- setdiff(unique(labels$label_type), c("label", "box"))
  if (length(bad_type) > 0L) {
    stop("`labels$label_type` must be either 'label' or 'box'. Problem value(s): ",
         paste(bad_type, collapse = ", "), call. = FALSE)
  }
  is_box <- labels$label_type == "box"
  if (any(is_box & (!is.finite(labels$width_px) | labels$width_px <= 0))) {
    stop("Annotation box labels need a positive `width_px` value.", call. = FALSE)
  }
  if (any(is_box & (!is.finite(labels$height_px) | labels$height_px <= 0))) {
    stop("Annotation box labels need a positive `height_px` value.", call. = FALSE)
  }
  bad_connector <- setdiff(unique(labels$connector_type), c("straight", "elbow", "curve", "squiggle"))
  if (length(bad_connector) > 0L) {
    stop("`labels$connector_type` must be one of 'straight', 'elbow', 'curve', or 'squiggle'. Problem value(s): ",
         paste(bad_connector, collapse = ", "), call. = FALSE)
  }
  one_start <- xor(is.finite(labels$connector_start_x), is.finite(labels$connector_start_y))
  if (any(labels$connector & one_start)) {
    stop("Connector start points need both `connector_start_x` and `connector_start_y`, or neither.", call. = FALSE)
  }
  one_midpoint <- xor(is.finite(labels$connector_mid_x), is.finite(labels$connector_mid_y))
  if (any(labels$connector & one_midpoint)) {
    stop("Connector breakpoints need both `connector_mid_x` and `connector_mid_y`, or neither.", call. = FALSE)
  }
  labels
}

parse_logical_column <- function(x, source) {
  if (is.logical(x)) {
    out <- x
  } else {
    text <- trimws(tolower(as.character(x)))
    out <- rep(NA, length(text))
    out[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
    out[is.na(text) | text %in% c("false", "f", "0", "no", "n", "")] <- FALSE
  }
  if (any(is.na(out))) {
    stop(source, " must contain TRUE/FALSE values.", call. = FALSE)
  }
  out
}

normalize_label_state <- function(state, source) {
  names(state) <- tolower(names(state))
  required <- c("label_id", "region", "dx_m", "dy_m")
  missing <- setdiff(required, names(state))
  if (length(missing) > 0L) {
    stop(source, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    label_id = trimws(as.character(state$label_id)),
    region = trimws(as.character(state$region)),
    dx_m = suppressWarnings(as.numeric(state$dx_m)),
    dy_m = suppressWarnings(as.numeric(state$dy_m)),
    stringsAsFactors = FALSE
  )
  if (any(is.na(out$label_id) | out$label_id == "")) {
    stop(source, " contains empty label_id values.", call. = FALSE)
  }
  if (anyDuplicated(out$label_id)) {
    dupes <- unique(out$label_id[duplicated(out$label_id)])
    stop(source, " contains duplicate label_id(s): ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(out$region) | out$region == "")) {
    stop(source, " contains empty region values.", call. = FALSE)
  }
  if (any(!is.finite(out$dx_m) | !is.finite(out$dy_m))) {
    stop(source, " contains non-numeric or missing state values.", call. = FALSE)
  }
  out
}

normalize_label_offsets <- function(offsets, source) {
  normalize_label_state(offsets, source)
}
