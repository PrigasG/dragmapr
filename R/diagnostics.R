#' Summarise a spatial dataset for use with dragmapr
#'
#' Inspects an `sf` object and reports geometry type, coordinate reference
#' system, feature count, candidate grouping columns (including empty-value
#' counts), detected parent-to-child column hierarchies (including cases where
#' child names repeat across parents), and invalid geometry counts. The output
#' guides column selection in [drag_map_prototype()] and helps diagnose
#' unexpected behaviour when uploading custom spatial files to Spatial Studio.
#'
#' @param x An `sf` object.
#' @param region_col Optional column name. When supplied, hierarchy details are
#'   reported relative to that column and it is highlighted in the summary.
#' @param quiet Logical. When `TRUE`, the summary is not printed and the result
#'   list is returned invisibly without any output.
#'
#' @return An invisible named list with components:
#'   \describe{
#'     \item{`geometry_type`}{Character vector of unique geometry type names.}
#'     \item{`crs`}{CRS identifier string (e.g. `"EPSG:3857"` or
#'       `"Unknown CRS"`).}
#'     \item{`is_longlat`}{Logical — `TRUE` when the CRS is geographic.}
#'     \item{`n_features`}{Integer row count.}
#'     \item{`candidate_cols`}{Character vector of candidate grouping columns.}
#'     \item{`column_details`}{Named list with `n_unique` and `n_empty` per
#'       candidate column.}
#'     \item{`n_invalid_geometries`}{Number of features with invalid geometry.}
#'     \item{`hierarchy_pairs`}{List of detected parent-to-child column pairs,
#'       each with `parent`, `child`, `n_parent`, `n_child`, and
#'       `child_repeats_across_parents`.}
#'     \item{`recommended_path`}{Suggested grouping path string, or `NULL`.}
#'   }
#' @export
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
#' diag <- d_diagnostics(poly)
d_diagnostics <- function(x, region_col = NULL, quiet = FALSE) {
  if (!inherits(x, "sf")) stop("`x` must be an sf object.", call. = FALSE)

  geom_col  <- attr(x, "sf_column")
  data_cols <- setdiff(names(x), geom_col)
  n         <- nrow(x)

  # --- Geometry ---
  geom_types <- unique(as.character(sf::st_geometry_type(x)))
  crs_obj    <- sf::st_crs(x)
  epsg       <- tryCatch(crs_obj$epsg, error = function(e) NA_integer_)
  crs_str    <- if (!is.na(epsg) && is.numeric(epsg)) {
    paste0("EPSG:", as.integer(epsg))
  } else {
    nm <- tryCatch(crs_obj$Name, error = function(e) NULL)
    if (!is.null(nm) && nzchar(nm)) nm else "Unknown CRS"
  }
  is_longlat <- isTRUE(sf::st_is_longlat(x))

  # --- Invalid geometries ---
  valid_vec  <- tryCatch(sf::st_is_valid(x), error = function(e) rep(NA, n))
  n_invalid  <- if (all(is.na(valid_vec))) NA_integer_ else
    as.integer(sum(!valid_vec, na.rm = TRUE))

  # --- Candidate grouping columns ---
  # Character / factor / integer columns with 2..90 % unique values
  candidate_cols <- character(0)
  col_details    <- list()
  for (col in data_cols) {
    vals <- x[[col]]
    if (!is.character(vals) && !is.factor(vals) && !is.integer(vals)) next
    cleaned  <- trimws(as.character(vals))
    cleaned[is.na(cleaned) | !nzchar(cleaned)] <- NA_character_
    n_unique <- length(unique(stats::na.omit(cleaned)))
    n_empty  <- sum(is.na(cleaned))
    if (n_unique < 2L || n_unique > n * 0.9) next
    candidate_cols <- c(candidate_cols, col)
    col_details[[col]] <- list(n_unique = n_unique, n_empty = n_empty)
  }

  # --- Hierarchy pairs ---
  hierarchy_pairs <- list()
  if (length(candidate_cols) >= 2L) {
    for (i in seq_len(length(candidate_cols) - 1L)) {
      for (j in seq(i + 1L, length(candidate_cols))) {
        a <- candidate_cols[[i]]
        b <- candidate_cols[[j]]
        a_vals <- trimws(as.character(x[[a]]))
        b_vals <- trimws(as.character(x[[b]]))
        n_a <- col_details[[a]]$n_unique
        n_b <- col_details[[b]]$n_unique
        if (n_a == n_b) next  # ambiguous direction; skip
        parent <- if (n_a < n_b) a else b
        child  <- if (n_a < n_b) b else a
        p_vals <- if (n_a < n_b) a_vals else b_vals
        c_vals <- if (n_a < n_b) b_vals else a_vals
        tbl <- unique(data.frame(p = p_vals, c = c_vals, stringsAsFactors = FALSE))
        child_repeats <- any(table(tbl$c) > 1L)
        hierarchy_pairs[[length(hierarchy_pairs) + 1L]] <- list(
          parent                     = parent,
          child                      = child,
          n_parent                   = if (n_a < n_b) n_a else n_b,
          n_child                    = if (n_a < n_b) n_b else n_a,
          child_repeats_across_parents = child_repeats
        )
      }
    }
  }

  # --- Recommended path ---
  focus <- if (!is.null(region_col) && region_col %in% candidate_cols) region_col else NULL
  rec_path <- NULL
  if (!is.null(focus)) {
    rec_path <- focus
    children <- Filter(function(hp) hp$parent == focus, hierarchy_pairs)
    if (length(children) > 0L) {
      rec_path <- paste(focus, "->", children[[1L]]$child)
    }
  } else if (length(hierarchy_pairs) > 0L) {
    hp <- hierarchy_pairs[[1L]]
    rec_path <- paste(hp$parent, "->", hp$child)
  } else if (length(candidate_cols) > 0L) {
    rec_path <- candidate_cols[[1L]]
  }

  result <- list(
    geometry_type              = geom_types,
    crs                        = crs_str,
    is_longlat                 = is_longlat,
    n_features                 = n,
    candidate_cols             = candidate_cols,
    column_details             = col_details,
    n_invalid_geometries       = n_invalid,
    hierarchy_pairs            = hierarchy_pairs,
    recommended_path           = rec_path
  )

  if (!quiet) {
    .diag_print(result, region_col)
  }

  invisible(result)
}

.diag_print <- function(d, region_col) {
  rule <- strrep("-", 42)
  cat("dragmapr spatial diagnostics\n")
  cat(rule, "\n")
  cat(sprintf("Geometry type      : %s\n", paste(d$geometry_type, collapse = ", ")))
  cat(sprintf("CRS                : %s%s\n", d$crs,
              if (d$is_longlat) "  (geographic - project before dragging)" else "  (projected)"))
  cat(sprintf("Features           : %d\n", d$n_features))
  n_inv <- d$n_invalid_geometries
  cat(sprintf("Invalid geometries : %s\n",
              if (is.na(n_inv)) "unable to check" else as.character(n_inv)))

  cat("\nCandidate grouping columns:\n")
  if (length(d$candidate_cols) == 0L) {
    cat("  (none found - add a character or factor column to define regions)\n")
  } else {
    for (col in d$candidate_cols) {
      det  <- d$column_details[[col]]
      note <- if (det$n_empty > 0L) paste0(", ", det$n_empty, " empty") else ""
      flag <- if (!is.null(region_col) && col == region_col) " *" else ""
      cat(sprintf("  - %-20s (%d unique%s)%s\n", col, det$n_unique, note, flag))
    }
  }

  if (length(d$hierarchy_pairs) > 0L) {
    cat("\nDetected column hierarchies:\n")
    for (hp in d$hierarchy_pairs) {
      rep_note <- if (hp$child_repeats_across_parents)
        "  [child names repeat across parents - use make_hierarchy_key()]" else ""
      cat(sprintf("  - %s -> %s  (%d -> %d groups)%s\n",
                  hp$parent, hp$child, hp$n_parent, hp$n_child, rep_note))
    }
  }

  if (!is.null(d$recommended_path)) {
    cat(sprintf("\nRecommended grouping : %s\n", d$recommended_path))
  }

  if (d$is_longlat) {
    cat("\n! CRS is geographic (lon/lat).\n")
    cat("  Call prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857)\n")
    cat("  before passing to drag_map_prototype() or render_dragged_map().\n")
  }

  cat(rule, "\n")
  invisible(NULL)
}
