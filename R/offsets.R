#' Read region offsets from CSV
#'
#' @param path Path to a CSV with `region`, `dx_m`, and `dy_m` columns.
#'
#' @return A data frame with normalized `region`, `dx_m`, and `dy_m` columns.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeLines(c("region,dx_m,dy_m", "North,1000,-500", "South,0,0"), path)
#' read_offsets(path)
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
#' @examples
#' regions <- sf::st_sf(
#'   region = c("A", "B"),
#'   geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(100, 100)),
#'                         crs = 3857)
#' )
#' offsets <- data.frame(region = "A", dx_m = 5000, dy_m = -2000)
#' apply_offsets(regions, offsets, region_col = "region")
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
    stop(
      "Project `x` before applying metre offsets. ",
      "Use prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857) ",
      "to convert to a projected CRS.",
      call. = FALSE
    )
  }

  feature_regions <- as.character(x[[region_col]])
  match_idx <- match(feature_regions, offsets$region)
  has_offset <- !is.na(match_idx)

  out <- x
  if (any(has_offset)) {
    geoms <- sf::st_geometry(out)
    geoms[has_offset] <- sf::st_sfc(
      Map(
        function(geom, dx, dy) geom + c(dx, dy),
        geoms[has_offset],
        offsets$dx_m[match_idx[has_offset]],
        offsets$dy_m[match_idx[has_offset]]
      ),
      crs = sf::st_crs(geoms)
    )
    sf::st_geometry(out) <- geoms
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
