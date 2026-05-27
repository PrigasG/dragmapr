#' Read region offsets from CSV
#'
#' @param path Path to a CSV with `region`, `dx_m`, and `dy_m` columns.
#'
#' @return A data frame with normalized `region`, `dx_m`, and `dy_m` columns.
#' @export
read_offsets <- function(path) {
  offsets <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(offsets) <- tolower(names(offsets))

  normalize_offsets(offsets, source = "Offset file")
}

#' Apply rigid offsets to grouped sf geometries
#'
#' @param x An `sf` object in a projected CRS.
#' @param offsets A data frame with `region`, `dx_m`, and `dy_m`, or a CSV path.
#' @param region_col Column in `x` defining draggable groups.
#'
#' @return An `sf` object with geometries translated by group.
#' @export
apply_offsets <- function(x, offsets, region_col) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  if (is.character(offsets) && length(offsets) == 1L) {
    offsets <- read_offsets(offsets)
  }
  if (!is.data.frame(offsets)) {
    stop("`offsets` must be a data frame or CSV path.", call. = FALSE)
  }
  offsets <- normalize_offsets(offsets, source = "`offsets`")
  if (sf::st_is_longlat(x)) {
    stop("Project `x` before applying metre offsets.", call. = FALSE)
  }

  out <- x
  for (i in seq_len(nrow(offsets))) {
    idx <- as.character(out[[region_col]]) == offsets$region[i]
    if (!any(idx)) next
    sf::st_geometry(out)[idx] <- sf::st_geometry(out)[idx] + c(offsets$dx_m[i], offsets$dy_m[i])
  }
  sf::st_as_sf(out)
}

normalize_offsets <- function(offsets, source) {
  names(offsets) <- tolower(names(offsets))
  required <- c("region", "dx_m", "dy_m")
  missing <- setdiff(required, names(offsets))
  if (length(missing) > 0L) {
    stop(source, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    region = trimws(as.character(offsets$region)),
    dx_m = suppressWarnings(as.numeric(offsets$dx_m)),
    dy_m = suppressWarnings(as.numeric(offsets$dy_m)),
    stringsAsFactors = FALSE
  )

  if (any(is.na(out$region) | out$region == "")) {
    stop(source, " contains empty region values.", call. = FALSE)
  }
  if (anyDuplicated(out$region)) {
    dupes <- unique(out$region[duplicated(out$region)])
    stop(source, " contains duplicate region(s): ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  if (any(!is.finite(out$dx_m) | !is.finite(out$dy_m))) {
    stop(source, " contains non-numeric or missing offsets.", call. = FALSE)
  }

  out
}
