#' Inspect editable spatial features
#'
#' Builds a plain data frame for Shiny tables, review screens, and scripted
#' edit workflows. Geometry is summarized instead of expanded so users can
#' decide which features to remove without accidentally dropping the active
#' geometry column.
#'
#' @param x An `sf` object.
#' @param key_col Optional column containing stable feature ids. When omitted,
#'   `.feature_id` is the row number as a character string.
#' @param include_geometry Include the active geometry list-column. Defaults to
#'   `FALSE` for table display.
#'
#' @return A data frame with `.feature_id`, `.row`, `.geometry_type`, bbox
#'   columns, and the non-geometry attributes from `x`.
#' @export
spatial_feature_table <- function(x, key_col = NULL, include_geometry = FALSE) {
  validate_dragmapr_sf(x)
  key_col <- normalize_optional_column(key_col, names(x), "`key_col`")
  include_geometry <- flag_scalar(include_geometry, "`include_geometry`")

  geom_col <- attr(x, "sf_column")
  attrs <- sf::st_drop_geometry(x)
  geometry <- sf::st_geometry(x)
  bbox <- t(vapply(seq_along(geometry), function(i) {
    bb <- tryCatch(sf::st_bbox(geometry[i]), error = function(e) NULL)
    if (is.null(bb) || any(!is.finite(bb))) {
      return(c(xmin = NA_real_, ymin = NA_real_, xmax = NA_real_, ymax = NA_real_))
    }
    unname(bb[c("xmin", "ymin", "xmax", "ymax")])
  }, numeric(4L)))
  colnames(bbox) <- c(".xmin", ".ymin", ".xmax", ".ymax")

  out <- data.frame(
    .feature_id = if (is.null(key_col)) as.character(seq_len(nrow(x))) else as.character(attrs[[key_col]]),
    .row = seq_len(nrow(x)),
    .geometry_type = as.character(sf::st_geometry_type(x)),
    bbox,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out <- cbind(out, attrs, stringsAsFactors = FALSE)
  names(out) <- make.unique(names(out))
  if (isTRUE(include_geometry)) {
    out[[geom_col]] <- geometry
  }
  out
}

#' @rdname spatial_feature_table
#' @export
view_spatial_features <- spatial_feature_table

#' Remove or keep spatial features
#'
#' @param x An `sf` object.
#' @param ids Feature ids to remove or keep. Matched against `key_col` when
#'   supplied, otherwise against row numbers. Character row ids such as `"3"`
#'   are accepted when `key_col = NULL`.
#' @param key_col Optional stable id column.
#' @param invert When `FALSE`, remove matching features. When `TRUE`, keep only
#'   matching features.
#' @param prune_empty Drop empty geometries after filtering.
#'
#' @return An `sf` object with the same CRS and geometry column as `x`.
#' @export
remove_spatial_features <- function(x,
                                    ids,
                                    key_col = NULL,
                                    invert = FALSE,
                                    prune_empty = TRUE) {
  validate_dragmapr_sf(x)
  key_col <- normalize_optional_column(key_col, names(x), "`key_col`")
  invert <- flag_scalar(invert, "`invert`")
  prune_empty <- flag_scalar(prune_empty, "`prune_empty`")
  if (missing(ids) || is.null(ids) || length(ids) == 0L) {
    if (!isTRUE(invert)) {
      return(x)
    }
    return(sf::st_as_sf(x[FALSE, , drop = FALSE]))
  }

  keys <- if (is.null(key_col)) {
    as.character(seq_len(nrow(x)))
  } else {
    as.character(sf::st_drop_geometry(x)[[key_col]])
  }
  ids <- as.character(ids)
  match_rows <- keys %in% ids
  keep <- if (isTRUE(invert)) match_rows else !match_rows
  out <- x[keep, , drop = FALSE]
  if (isTRUE(prune_empty) && nrow(out) > 0L) {
    empty <- sf::st_is_empty(out)
    out <- out[!empty, , drop = FALSE]
  }
  sf::st_as_sf(out)
}

#' @rdname remove_spatial_features
#' @export
delete_spatial_features <- remove_spatial_features

#' @rdname remove_spatial_features
#' @export
keep_spatial_features <- function(x, ids, key_col = NULL, prune_empty = TRUE) {
  remove_spatial_features(
    x,
    ids = ids,
    key_col = key_col,
    invert = TRUE,
    prune_empty = prune_empty
  )
}

#' Add or replace spatial features
#'
#' @param x An `sf` object.
#' @param features An `sf` object containing new or replacement features.
#' @param ids Existing feature ids to replace. Matched like
#'   [remove_spatial_features()].
#' @param key_col Optional stable id column.
#'
#' @return An `sf` object.
#' @export
add_spatial_features <- function(x, features) {
  validate_dragmapr_sf(x)
  validate_dragmapr_sf(features, arg = "features")
  features <- align_spatial_features(x, features)
  out <- rbind(x, features)
  sf::st_as_sf(out)
}

#' @rdname add_spatial_features
#' @export
replace_spatial_features <- function(x, ids, features, key_col = NULL) {
  add_spatial_features(
    remove_spatial_features(x, ids = ids, key_col = key_col),
    features
  )
}

align_spatial_features <- function(x, features) {
  target_crs <- sf::st_crs(x)
  feature_crs <- sf::st_crs(features)
  if (!is.na(target_crs) && !is.na(feature_crs) && target_crs != feature_crs) {
    features <- sf::st_transform(features, target_crs)
  } else if (is.na(feature_crs) && !is.na(target_crs)) {
    sf::st_crs(features) <- target_crs
  }

  geom_col <- attr(x, "sf_column")
  x_attrs <- sf::st_drop_geometry(x)
  feature_attrs <- sf::st_drop_geometry(features)
  missing_cols <- setdiff(names(x_attrs), names(feature_attrs))
  extra_cols <- setdiff(names(feature_attrs), names(x_attrs))
  for (nm in missing_cols) {
    feature_attrs[[nm]] <- typed_na_column(x_attrs[[nm]], nrow(features))
  }
  feature_attrs <- feature_attrs[, names(x_attrs), drop = FALSE]
  if (length(extra_cols) > 0L) {
    warning(
      "Dropping feature column(s) not present in `x`: ",
      paste(extra_cols, collapse = ", "),
      call. = FALSE
    )
  }
  feature_attrs[[geom_col]] <- sf::st_geometry(features)
  sf::st_as_sf(feature_attrs, sf_column_name = geom_col, crs = target_crs)
}

typed_na_column <- function(x, n) {
  x[rep.int(NA_integer_, n)]
}

normalize_optional_column <- function(col, choices, arg) {
  if (is.null(col)) {
    return(NULL)
  }
  col <- as.character(col)
  if (length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop(arg, " must be NULL or a single column name.", call. = FALSE)
  }
  if (!col %in% choices) {
    stop(arg, " '", col, "' not found.", call. = FALSE)
  }
  col
}
