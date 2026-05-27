#' Derive one label anchor per draggable region
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column defining regions.
#' @param label_col Column used for label text. Defaults to `region_col`.
#' @param point One of `"point_on_surface"` or `"centroid"`.
#'
#' @return A data frame with `label_id`, `region`, `label`, `x`, and `y`.
#' @export
make_labels <- function(x, region_col, label_col = region_col, point = c("point_on_surface", "centroid")) {
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
    stop("Project `x` before deriving metre label anchors.", call. = FALSE)
  }
  point <- match.arg(point)

  regions <- sort(unique(as.character(x[[region_col]])))
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

  do.call(rbind, rows)
}

#' Read label offsets from CSV
#'
#' @param path Path to a CSV with `label_id`, `region`, `dx_m`, and `dy_m`.
#'
#' @return A data frame with normalized label offset columns.
#' @export
read_label_offsets <- function(path) {
  offsets <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  normalize_label_offsets(offsets, source = "Label offset file")
}

#' Apply label-specific offsets to a label table
#'
#' @param labels A data frame from `make_labels()`.
#' @param label_offsets A data frame with `label_id`, `region`, `dx_m`, and
#'   `dy_m`, or a CSV path. `NULL` leaves labels unchanged.
#'
#' @return A label data frame with adjusted `x` and `y`.
#' @export
apply_label_offsets <- function(labels, label_offsets = NULL) {
  labels <- normalize_labels(labels)
  if (is.null(label_offsets)) {
    return(labels)
  }
  if (is.character(label_offsets) && length(label_offsets) == 1L) {
    label_offsets <- read_label_offsets(label_offsets)
  }
  if (!is.data.frame(label_offsets)) {
    stop("`label_offsets` must be a data frame, CSV path, or NULL.", call. = FALSE)
  }
  label_offsets <- normalize_label_offsets(label_offsets, source = "`label_offsets`")

  match_idx <- match(labels$label_id, label_offsets$label_id)
  has_offset <- !is.na(match_idx)
  labels$x[has_offset] <- labels$x[has_offset] + label_offsets$dx_m[match_idx[has_offset]]
  labels$y[has_offset] <- labels$y[has_offset] + label_offsets$dy_m[match_idx[has_offset]]
  labels
}

normalize_labels <- function(labels) {
  if (!is.data.frame(labels)) {
    stop("`labels` must be a data frame.", call. = FALSE)
  }
  names(labels) <- tolower(names(labels))
  required <- c("label_id", "region", "label", "x", "y")
  missing <- setdiff(required, names(labels))
  if (length(missing) > 0L) {
    stop("`labels` is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    label_id = trimws(as.character(labels$label_id)),
    region = trimws(as.character(labels$region)),
    label = as.character(labels$label),
    x = suppressWarnings(as.numeric(labels$x)),
    y = suppressWarnings(as.numeric(labels$y)),
    stringsAsFactors = FALSE
  )
  if (any(is.na(out$label_id) | out$label_id == "")) {
    stop("`labels` contains empty label_id values.", call. = FALSE)
  }
  if (anyDuplicated(out$label_id)) {
    dupes <- unique(out$label_id[duplicated(out$label_id)])
    stop("`labels` contains duplicate label_id(s): ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  if (any(is.na(out$region) | out$region == "")) {
    stop("`labels` contains empty region values.", call. = FALSE)
  }
  if (any(!is.finite(out$x) | !is.finite(out$y))) {
    stop("`labels` contains non-numeric or missing coordinates.", call. = FALSE)
  }
  out
}

normalize_label_offsets <- function(offsets, source) {
  names(offsets) <- tolower(names(offsets))
  required <- c("label_id", "region", "dx_m", "dy_m")
  missing <- setdiff(required, names(offsets))
  if (length(missing) > 0L) {
    stop(source, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    label_id = trimws(as.character(offsets$label_id)),
    region = trimws(as.character(offsets$region)),
    dx_m = suppressWarnings(as.numeric(offsets$dx_m)),
    dy_m = suppressWarnings(as.numeric(offsets$dy_m)),
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
    stop(source, " contains non-numeric or missing offsets.", call. = FALSE)
  }
  out
}
