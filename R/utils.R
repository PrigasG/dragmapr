`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Validate that `x` is an sf object with an intact active geometry column.
# Shared by every public entry point that accepts sf geometry so malformed
# inputs fail with the same clear message instead of a deep, cryptic error.
validate_dragmapr_sf <- function(x, arg = "x") {
  if (!inherits(x, "sf")) {
    stop("`", arg, "` must be an sf object.", call. = FALSE)
  }
  geom_col <- attr(x, "sf_column")
  if (is.null(geom_col) || length(geom_col) != 1L || is.na(geom_col) ||
      !nzchar(geom_col) || !geom_col %in% names(x)) {
    stop(
      "`", arg, "` does not have a valid active sf geometry column. ",
      "The geometry column may have been dropped or renamed; restore it with ",
      "sf::st_geometry(", arg, ") <- \"geometry_column_name\".",
      call. = FALSE
    )
  }
  if (!inherits(x[[geom_col]], "sfc")) {
    stop(
      "`", arg, "` has an invalid active sf geometry column (`", geom_col,
      "`); it must contain an sfc vector.",
      call. = FALSE
    )
  }
  invisible(x)
}
