#' Create keyed dragmapr feature styles
#'
#' Normalizes lightweight, already-classified visual overrides so interactive
#' widgets, static renders, state JSON, and project bundles can preserve the
#' same editorial styling. Data classification and palette selection remain
#' application responsibilities.
#'
#' @param data Data frame of keyed styles, or `NULL` for an empty table.
#' @param id_col Column in `data` identifying regions/features.
#'
#' @return A `dragmapr_styles` data frame with columns `region`, `fill`,
#'   `stroke`, `stroke_width`, `opacity`, `label_visible`, and `highlight`.
#' @export
d_styles <- function(data = NULL, id_col = "region") {
  schema <- list(
    region = character(),
    fill = character(),
    stroke = character(),
    stroke_width = numeric(),
    opacity = numeric(),
    label_visible = logical(),
    highlight = logical()
  )
  if (is.null(data)) {
    return(structure(as.data.frame(schema, stringsAsFactors = FALSE),
                     class = c("dragmapr_styles", "data.frame")))
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.character(id_col) || length(id_col) != 1L || is.na(id_col) ||
      !nzchar(id_col) || !id_col %in% names(data)) {
    stop("`id_col` must name one column in `data`.", call. = FALSE)
  }
  region <- trimws(as.character(data[[id_col]]))
  if (anyNA(region) || any(!nzchar(region)) || anyDuplicated(region)) {
    stop("Style identifiers must be unique, non-missing, and non-empty.",
         call. = FALSE)
  }
  value <- function(name, default) {
    if (name %in% names(data)) data[[name]] else rep(default, nrow(data))
  }
  fill <- as.character(value("fill", NA_character_))
  stroke <- as.character(value("stroke", NA_character_))
  validate_style_colors(fill, "fill")
  validate_style_colors(stroke, "stroke")
  stroke_width <- suppressWarnings(as.numeric(value("stroke_width", NA_real_)))
  opacity <- suppressWarnings(as.numeric(value("opacity", NA_real_)))
  label_visible <- normalize_style_logical(value("label_visible", NA), "label_visible")
  highlight <- normalize_style_logical(value("highlight", NA), "highlight")
  if (any(!is.na(stroke_width) & (!is.finite(stroke_width) | stroke_width < 0))) {
    stop("`stroke_width` values must be non-negative finite numbers or NA.",
         call. = FALSE)
  }
  if (any(!is.na(opacity) & (!is.finite(opacity) | opacity < 0 | opacity > 1))) {
    stop("`opacity` values must be between 0 and 1 or NA.", call. = FALSE)
  }
  out <- data.frame(
    region = region,
    fill = fill,
    stroke = stroke,
    stroke_width = stroke_width,
    opacity = opacity,
    label_visible = label_visible,
    highlight = highlight,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$region), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("dragmapr_styles", "data.frame"))
}

#' Merge keyed feature styles
#'
#' @param base,update Objects created by [d_styles()]. Rows in `update`
#'   replace matching rows in `base`.
#' @return A `dragmapr_styles` table.
#' @export
merge_dragmapr_styles <- function(base, update) {
  base <- d_styles(base)
  update <- d_styles(update)
  kept <- base[!base$region %in% update$region, , drop = FALSE]
  d_styles(rbind(kept, update))
}

validate_style_colors <- function(x, name) {
  values <- unique(x[!is.na(x) & nzchar(x)])
  bad <- values[!vapply(values, function(value) {
    !inherits(try(grDevices::col2rgb(value), silent = TRUE), "try-error")
  }, logical(1))]
  if (length(bad)) {
    stop("Invalid `", name, "` colour value(s): ",
         paste(utils::head(bad, 8L), collapse = ", "), call. = FALSE)
  }
  invisible(x)
}

normalize_style_logical <- function(x, name) {
  if (is.logical(x)) return(x)
  if (is.numeric(x) && all(is.na(x) | x %in% c(0, 1))) return(as.logical(x))
  if (is.character(x)) {
    normalized <- tolower(trimws(x))
    normalized[normalized == ""] <- NA_character_
    if (all(is.na(normalized) | normalized %in% c("true", "false", "1", "0"))) {
      return(ifelse(
        is.na(normalized), NA,
        normalized %in% c("true", "1")
      ))
    }
  }
  stop("`", name, "` values must be TRUE, FALSE, or NA.", call. = FALSE)
}
