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

#' Detect parent-child grouping columns
#'
#' Finds candidate hierarchy pairs in a data frame or `sf` object. A pair is
#' treated as parent-child when the child column has more groups than the
#' parent column and most child values sit inside one parent group.
#'
#' @param x A data frame or `sf` object.
#' @param cols Optional character vector of columns to inspect. `NULL` uses
#'   non-geometry atomic columns.
#' @param max_child_groups Maximum number of child groups to recommend for
#'   interactive bloom. Pairs above this limit are returned but flagged.
#' @param min_confidence Minimum confidence for the `recommended` flag.
#'
#' @return A data frame with one row per detected pair.
#' @export
#' @examples
#' df <- data.frame(
#'   county = c("A", "A", "B", "B"),
#'   town = c("A1", "A2", "B1", "B2")
#' )
#' detect_hierarchy_columns(df)
detect_hierarchy_columns <- function(x, cols = NULL, max_child_groups = 600L,
                                     min_confidence = 0.72) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  if (!is.numeric(max_child_groups) || length(max_child_groups) != 1L ||
      !is.finite(max_child_groups) || max_child_groups < 2) {
    stop("`max_child_groups` must be a single number greater than 1.", call. = FALSE)
  }
  if (!is.numeric(min_confidence) || length(min_confidence) != 1L ||
      !is.finite(min_confidence) || min_confidence < 0 || min_confidence > 1) {
    stop("`min_confidence` must be a number between 0 and 1.", call. = FALSE)
  }
  cols <- .hierarchy_candidate_cols(x, cols)
  empty <- data.frame(
    parent = character(), child = character(), n_parent = integer(),
    n_child = integer(), composite_groups = integer(),
    child_repeats_across_parents = logical(), nesting_score = numeric(),
    confidence = numeric(), recommended = logical(), reason = character(),
    stringsAsFactors = FALSE
  )
  if (length(cols) < 2L) return(empty)

  out <- list()
  for (i in seq_len(length(cols) - 1L)) {
    for (j in seq.int(i + 1L, length(cols))) {
      a <- cols[[i]]
      b <- cols[[j]]
      av <- .hierarchy_clean(x[[a]])
      bv <- .hierarchy_clean(x[[b]])
      na <- length(unique(av))
      nb <- length(unique(bv))
      if (na == nb || na < 1L || nb < 1L) next
      parent <- if (na < nb) a else b
      child <- if (na < nb) b else a
      pv <- if (na < nb) av else bv
      cv <- if (na < nb) bv else av
      np <- min(na, nb)
      nc <- max(na, nb)
      pair <- .score_hierarchy_pair(pv, cv)
      too_many <- nc > max_child_groups
      confidence <- pair$confidence
      reason <- if (pair$nesting_score < min_confidence) {
        "Child values are split across too many parent groups."
      } else if (too_many) {
        paste0("Child has ", nc, " groups, above the interactive bloom limit.")
      } else if (pair$child_repeats_across_parents) {
        "Child names repeat across parents; use composite parent-child keys."
      } else {
        "Child values nest inside the parent column."
      }
      out[[length(out) + 1L]] <- data.frame(
        parent = parent,
        child = child,
        n_parent = as.integer(np),
        n_child = as.integer(nc),
        composite_groups = as.integer(pair$composite_groups),
        child_repeats_across_parents = pair$child_repeats_across_parents,
        nesting_score = pair$nesting_score,
        confidence = confidence,
        recommended = confidence >= min_confidence && !too_many,
        reason = reason,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0L) return(empty)
  out <- do.call(rbind, out)
  out <- out[order(-out$recommended, -out$confidence, out$n_child, out$parent, out$child), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Recommend a hierarchy for dragmapr
#'
#' Picks a parent column and, when available, a child column suitable for
#' bloom/leaf-flip animation.
#'
#' @param x A data frame or `sf` object.
#' @param parent_col Optional preferred parent column.
#' @param max_child_groups Maximum number of child groups to recommend.
#'
#' @return A list with `parent`, `child`, `confidence`, `reason`, and `pairs`.
#' @export
recommend_dragmapr_hierarchy <- function(x, parent_col = NULL,
                                         max_child_groups = 600L) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  cols <- .hierarchy_candidate_cols(x, NULL)
  if (length(cols) == 0L) {
    return(list(parent = NULL, child = NULL, confidence = 0,
                reason = "No usable grouping columns were found.", pairs = detect_hierarchy_columns(x)))
  }
  if (!is.null(parent_col)) {
    parent_col <- as.character(parent_col)
    if (length(parent_col) != 1L || !parent_col %in% names(x)) {
      stop("`parent_col` must be one column name in `x`.", call. = FALSE)
    }
  }
  pairs <- detect_hierarchy_columns(x, cols = cols, max_child_groups = max_child_groups)
  candidates <- pairs
  if (!is.null(parent_col) && nrow(candidates) > 0L) {
    candidates <- candidates[candidates$parent == parent_col, , drop = FALSE]
  }
  candidates <- candidates[candidates$recommended, , drop = FALSE]
  if (nrow(candidates) > 0L) {
    best <- candidates[1L, , drop = FALSE]
    return(list(parent = best$parent, child = best$child,
                confidence = best$confidence, reason = best$reason,
                pairs = pairs))
  }
  parent <- parent_col
  if (is.null(parent)) {
    parent <- .default_hierarchy_parent(x, cols)
  }
  list(parent = parent, child = NULL, confidence = 0,
       reason = "No child column clearly nests inside the parent column.",
       pairs = pairs)
}

#' Validate a parent-child bloom hierarchy
#'
#' Checks whether `child_col` can subdivide `parent_col` for leaf-flip bloom.
#'
#' @param x A data frame or `sf` object.
#' @param parent_col Parent grouping column.
#' @param child_col Child grouping column.
#' @param max_child_groups Maximum number of child groups allowed.
#'
#' @return A list with `valid`, `message`, `parent_key`, `child_key`, and
#'   summary counts.
#' @export
validate_bloom_hierarchy <- function(x, parent_col, child_col,
                                     max_child_groups = 600L) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or sf object.", call. = FALSE)
  }
  parent_col <- as.character(parent_col)
  child_col <- as.character(child_col)
  if (length(parent_col) != 1L || !nzchar(parent_col) || !parent_col %in% names(x)) {
    stop("`parent_col` must be one column name in `x`.", call. = FALSE)
  }
  if (length(child_col) != 1L || !nzchar(child_col) || !child_col %in% names(x)) {
    stop("`child_col` must be one column name in `x`.", call. = FALSE)
  }
  parent_key <- make_hierarchy_key(x, parent_col)
  child_value <- .hierarchy_clean(x[[child_col]])
  child_key <- make_hierarchy_key(x, unique(c(parent_col, child_col)))
  score <- .score_hierarchy_pair(parent_key, child_value)
  n_parent <- length(unique(parent_key))
  n_child <- length(unique(child_key))
  valid <- TRUE
  msg <- "Child column can bloom from the parent column."
  if (identical(parent_col, child_col)) {
    valid <- FALSE
    msg <- "`child_col` must be different from `parent_col`."
  } else if (n_child <= n_parent) {
    valid <- FALSE
    msg <- "`child_col` does not create more groups than `parent_col`."
  } else if (score$nesting_score < 0.72) {
    valid <- FALSE
    msg <- "`child_col` does not nest cleanly inside `parent_col`."
  } else if (n_child > max_child_groups) {
    valid <- FALSE
    msg <- paste0("`child_col` creates ", n_child,
                  " child groups. Choose a coarser child column or raise `max_child_groups`.")
  } else if (score$child_repeats_across_parents) {
    msg <- "Child names repeat across parents. Composite keys will be used."
  }
  list(
    valid = valid,
    message = msg,
    parent_col = parent_col,
    child_col = child_col,
    n_parent = n_parent,
    n_child = n_child,
    nesting_score = score$nesting_score,
    child_repeats_across_parents = score$child_repeats_across_parents,
    parent_key = parent_key,
    child_key = child_key
  )
}

#' Build leaf-flip transition data
#'
#' Prepares an `sf` object and transition list for parent-to-child leaf-flip
#' bloom in [drag_map_prototype()].
#'
#' @param x An `sf` object.
#' @param parent_col Parent grouping column.
#' @param child_col Child grouping column.
#' @param expanded Optional parent values to start expanded.
#' @param dissolve Add dissolved parent shell features for collapsed parents.
#' @param max_child_groups Maximum number of child groups allowed.
#' @param animation Animation style. Use `"branch_bloom"` for the clean group
#'   bloom or `"leaf_flip"` for the temporary leaf-proxy flip.
#' @param duration_ms Animation duration in milliseconds.
#' @param easing Animation easing name.
#' @param show_parent_ghost Logical. Show a faint parent shell while expanded.
#' @param parent_ghost_opacity Parent ghost opacity when shown.
#' @param leaf_flip_strength Leaf proxy rotation strength.
#' @param leaf_child_scale Starting scale for child polygons in leaf mode.
#' @param leaf_expand_duration_factor Leaf expand speed multiplier.
#' @param leaf_collapse_duration_factor Leaf collapse speed multiplier.
#' @param boundary Logical. Add a dotted drag boundary for expanded groups.
#' @param boundary_behavior Boundary mode. Use `"drag"` or `"none"`.
#' @param boundary_drag_threshold Pixel threshold before a boundary drag counts.
#' @param boundary_label Text prefix for the dotted drag boundary.
#' @param parent_key_col,child_key_col,shell_col Internal column names to add.
#'
#' @return A list with `sf`, `transition`, `parent_key_col`, `child_key_col`,
#'   `shell_col`, and `validation`.
#' @export
build_branch_transition_data <- function(x, parent_col, child_col,
                                         expanded = NULL, dissolve = TRUE,
                                         max_child_groups = 600L,
                                         animation = c("branch_bloom", "leaf_flip"),
                                         duration_ms = 375,
                                         easing = c("cubic-out", "cubic-in-out", "linear"),
                                         show_parent_ghost = FALSE,
                                         parent_ghost_opacity = 0.18,
                                         leaf_flip_strength = 0.16,
                                         leaf_child_scale = 0.86,
                                         leaf_expand_duration_factor = 0.82,
                                         leaf_collapse_duration_factor = 0.58,
                                         boundary = TRUE,
                                         boundary_behavior = c("drag", "none"),
                                         boundary_drag_threshold = 8,
                                         boundary_label = "Drag to",
                                         parent_key_col = "..dragmapr_parent_key..",
                                         child_key_col = "..dragmapr_child_key..",
                                         shell_col = "..dragmapr_parent_shell..") {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  animation <- match.arg(animation)
  easing <- match.arg(easing)
  boundary_behavior <- match.arg(boundary_behavior)
  .check_branch_number(duration_ms, "duration_ms", min = 1)
  .check_branch_number(parent_ghost_opacity, "parent_ghost_opacity", min = 0, max = 1)
  .check_branch_number(leaf_flip_strength, "leaf_flip_strength", min = 0, max = 1)
  .check_branch_number(leaf_child_scale, "leaf_child_scale", min = 0.60, max = 1)
  .check_branch_number(leaf_expand_duration_factor, "leaf_expand_duration_factor", min = 1e-9, max = 1.25)
  .check_branch_number(leaf_collapse_duration_factor, "leaf_collapse_duration_factor", min = 1e-9, max = 1.25)
  .check_branch_number(boundary_drag_threshold, "boundary_drag_threshold", min = 0)
  for (nm in c("dissolve", "show_parent_ghost", "boundary")) {
    value <- get(nm)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", nm, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.character(boundary_label) || length(boundary_label) != 1L ||
      is.na(boundary_label)) {
    stop("`boundary_label` must be a single string.", call. = FALSE)
  }
  validation <- validate_bloom_hierarchy(
    x, parent_col, child_col, max_child_groups = max_child_groups
  )
  if (!isTRUE(validation$valid)) {
    stop(validation$message, call. = FALSE)
  }
  out <- x
  for (nm in c(parent_key_col, child_key_col, shell_col)) {
    if (nm %in% names(out)) {
      stop("Column `", nm, "` already exists in `x`. Choose a different internal column name.", call. = FALSE)
    }
  }
  out[[parent_key_col]] <- validation$parent_key
  out[[child_key_col]] <- validation$child_key
  transition <- list(
    mode = "branch_bloom",
    controller = "branch_animator",
    animation = animation,
    animation_mode = animation,
    effect = animation,
    child_region_col = child_key_col,
    expanded = as.character(expanded %||% character()),
    duration_ms = unname(duration_ms),
    easing = easing,
    overshoot = 0,
    stagger_ms = if (identical(animation, "leaf_flip")) 55 else 0,
    show_parent_ghost = isTRUE(show_parent_ghost),
    parent_ghost_opacity = unname(parent_ghost_opacity),
    leaf_flip_strength = unname(leaf_flip_strength),
    leaf_child_scale = unname(leaf_child_scale),
    leaf_expand_duration_factor = unname(leaf_expand_duration_factor),
    leaf_collapse_duration_factor = unname(leaf_collapse_duration_factor),
    boundary = isTRUE(boundary),
    boundary_behavior = boundary_behavior,
    boundary_label = unname(boundary_label),
    boundary_drag_threshold = unname(boundary_drag_threshold)
  )
  if (isTRUE(dissolve)) {
    out[[shell_col]] <- 0L
    shell_rows <- lapply(split(seq_len(nrow(out)), out[[parent_key_col]]), function(idx) {
      geom <- tryCatch(
        suppressWarnings(sf::st_union(sf::st_geometry(out)[idx])),
        error = function(e) NULL
      )
      if (is.null(geom) || length(geom) == 0L) return(NULL)
      row <- out[idx[1L], , drop = FALSE]
      sf::st_geometry(row) <- geom
      row[[shell_col]] <- 1L
      row
    })
    shell_rows <- Filter(Negate(is.null), shell_rows)
    if (length(shell_rows) > 0L) {
      out <- rbind(out, do.call(rbind, shell_rows))
      transition$shell_col <- shell_col
    }
  }
  list(
    sf = out,
    transition = transition,
    parent_key_col = parent_key_col,
    child_key_col = child_key_col,
    shell_col = if (isTRUE(dissolve)) shell_col else NULL,
    validation = validation
  )
}

#' Build parent and child labels for branch bloom
#'
#' Creates the dual-label table understood by the branch-bloom browser helper.
#' Parent labels are safe for normal use. Child labels are optional and mainly
#' useful for testing because many child labels can make branch animation feel
#' busy or slow.
#'
#' @param x An `sf` object prepared by [build_branch_transition_data()], or the
#'   `sf` element from that return value.
#' @param parent_key_col,child_key_col,shell_col Column names holding the
#'   parent key, child key, and parent-shell flag.
#' @param parent_label_col,child_label_col Optional columns used for label text.
#'   Defaults to the parent and child key columns.
#' @param show_parent_labels,show_child_labels Include parent or child labels.
#' @param max_label_chars Maximum number of characters before label text is
#'   shortened with `...`.
#' @param connector,label_type Values written to the returned label table.
#'
#' @return A data frame suitable for the `labels` argument of
#'   [drag_map_prototype()]. Returns a zero-row label table when both label
#'   levels are disabled.
#' @export
make_branch_bloom_labels <- function(x,
                                     parent_key_col = "..dragmapr_parent_key..",
                                     child_key_col = "..dragmapr_child_key..",
                                     shell_col = "..dragmapr_parent_shell..",
                                     parent_label_col = parent_key_col,
                                     child_label_col = child_key_col,
                                     show_parent_labels = TRUE,
                                     show_child_labels = FALSE,
                                     max_label_chars = 36,
                                     connector = FALSE,
                                     label_type = "label") {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  for (nm in c(parent_key_col, child_key_col, shell_col,
               parent_label_col, child_label_col)) {
    if (!nm %in% names(x)) {
      stop("Column `", nm, "` not found in `x`.", call. = FALSE)
    }
  }
  for (nm in c("show_parent_labels", "show_child_labels", "connector")) {
    value <- get(nm)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", nm, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.numeric(max_label_chars) || length(max_label_chars) != 1L ||
      !is.finite(max_label_chars) || max_label_chars < 4) {
    stop("`max_label_chars` must be a single number of at least 4.", call. = FALSE)
  }
  if (!is.character(label_type) || length(label_type) != 1L ||
      is.na(label_type) || !nzchar(label_type)) {
    stop("`label_type` must be a single non-empty string.", call. = FALSE)
  }

  shell_flag <- suppressWarnings(as.integer(x[[shell_col]]))
  shell_flag[is.na(shell_flag)] <- 0L
  child_sf <- x[shell_flag == 0L, , drop = FALSE]
  shell_sf <- x[shell_flag == 1L, , drop = FALSE]
  pieces <- list()

  if (isTRUE(show_parent_labels) && nrow(shell_sf) > 0L) {
    parent_labels <- make_region_labels(
      shell_sf,
      region_col = parent_key_col,
      label_col  = parent_label_col
    )
    parent_labels$label_id <- paste0("parent::", parent_labels$label_id)
    parent_labels$label_level <- "parent"
    parent_labels$label_parent <- parent_labels$region
    pieces[[length(pieces) + 1L]] <- parent_labels
  }

  if (isTRUE(show_child_labels) && nrow(child_sf) > 0L) {
    child_labels <- make_region_labels(
      child_sf,
      region_col = child_key_col,
      label_col  = child_label_col
    )
    lookup <- unique(data.frame(
      region = as.character(child_sf[[child_key_col]]),
      parent = as.character(child_sf[[parent_key_col]]),
      stringsAsFactors = FALSE
    ))
    child_labels$label_id <- paste0("child::", child_labels$label_id)
    child_labels$label_level <- "child"
    child_labels$label_parent <- lookup$parent[match(as.character(child_labels$region), lookup$region)]
    pieces[[length(pieces) + 1L]] <- child_labels
  }

  if (length(pieces) == 0L) return(.empty_branch_bloom_labels())
  labels <- do.call(rbind, pieces)
  labels$label <- .short_branch_label(labels$label, as.integer(max_label_chars))
  labels$connector <- isTRUE(connector)
  labels$label_type <- label_type
  labels
}

#' Summarise CRS meaning for dragmapr
#'
#' Returns plain-language CRS metadata and what the units mean for drag offsets.
#'
#' @param x An `sf` object.
#'
#' @return A list with CRS id, name, type, units, bounds, and message.
#' @export
summarise_spatial_crs <- function(x) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  crs <- tryCatch(sf::st_crs(x), error = function(e) NA)
  has_crs <- !is.na(crs)
  epsg <- tryCatch(crs$epsg, error = function(e) NA_integer_)
  name <- tryCatch(crs$Name, error = function(e) NULL)
  units <- tryCatch(crs$units_gdal, error = function(e) NULL)
  longlat <- tryCatch(sf::st_is_longlat(x), error = function(e) NA)
  bbox <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
  type <- if (!has_crs) "Unknown" else if (isTRUE(longlat)) "Geographic" else "Projected"
  units_label <- if (!is.null(units) && !is.na(units) && nzchar(units)) {
    units
  } else if (isTRUE(longlat)) {
    "degrees"
  } else if (has_crs) {
    "map units"
  } else {
    "unknown"
  }
  message <- if (!has_crs) {
    "No CRS was found. Assign a projected CRS before editing offsets."
  } else if (isTRUE(longlat)) {
    "This CRS uses longitude and latitude. Reproject before using drag distances as metres."
  } else if (grepl("metre|meter", units_label, ignore.case = TRUE)) {
    "Offsets are interpreted in metres."
  } else {
    paste0("Offsets use this CRS's map units: ", units_label, ".")
  }
  list(
    id = if (!is.na(epsg) && is.finite(epsg)) paste0("EPSG:", as.integer(epsg)) else "No EPSG",
    epsg = if (!is.na(epsg) && is.finite(epsg)) as.integer(epsg) else NA_integer_,
    name = if (!is.null(name) && nzchar(name)) name else type,
    type = type,
    units = units_label,
    is_longlat = isTRUE(longlat),
    bounds = if (!is.null(bbox)) unclass(bbox) else NULL,
    message = message
  )
}

#' Profile a spatial upload for dragmapr
#'
#' Combines CRS information, geometry counts, candidate hierarchies, and a
#' recommended parent-child bloom setup.
#'
#' @param x An `sf` object.
#' @param parent_col Optional preferred parent column.
#'
#' @return A list with `crs`, `geometry_type`, `n_features`, `hierarchy`, and
#'   `animation`.
#' @export
profile_spatial_upload <- function(x, parent_col = NULL) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  rec <- recommend_dragmapr_hierarchy(x, parent_col = parent_col)
  valid <- NULL
  if (!is.null(rec$parent) && !is.null(rec$child)) {
    valid <- validate_bloom_hierarchy(x, rec$parent, rec$child)
  }
  list(
    crs = summarise_spatial_crs(x),
    geometry_type = unique(as.character(sf::st_geometry_type(x))),
    n_features = nrow(x),
    n_invalid_geometries = {
      v <- tryCatch(sf::st_is_valid(x), error = function(e) rep(NA, nrow(x)))
      if (all(is.na(v))) NA_integer_ else as.integer(sum(!v, na.rm = TRUE))
    },
    hierarchy = rec,
    animation = list(
      can_leaf_flip = !is.null(valid) && isTRUE(valid$valid),
      parent_col = rec$parent,
      child_col = rec$child,
      message = if (!is.null(valid)) valid$message else rec$reason
    )
  )
}

# Internal: normalise a raw column to clean character values
.hierarchy_clean <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "(missing)"
  x
}

.hierarchy_candidate_cols <- function(x, cols = NULL) {
  geom_col <- attr(x, "sf_column")
  if (is.null(cols)) {
    cols <- setdiff(names(x), geom_col)
  }
  cols <- as.character(cols)
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0L) {
    stop("Column(s) not found in `x`: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  keep <- vapply(cols, function(col) {
    v <- x[[col]]
    is.atomic(v) && !inherits(v, "sfc") && length(unique(stats::na.omit(.hierarchy_clean(v)))) > 1L
  }, logical(1L))
  cols[keep]
}

.score_hierarchy_pair <- function(parent_values, child_values) {
  parent_values <- .hierarchy_clean(parent_values)
  child_values <- .hierarchy_clean(child_values)
  pairs <- unique(data.frame(parent = parent_values, child = child_values,
                             stringsAsFactors = FALSE))
  n_child <- length(unique(child_values))
  by_child <- split(pairs$parent, pairs$child)
  parent_counts <- vapply(by_child, function(v) length(unique(v)), integer(1L))
  nesting_score <- if (nrow(pairs) == 0L) 0 else min(1, n_child / nrow(pairs))
  child_repeats <- any(parent_counts > 1L)
  composite_groups <- nrow(pairs)
  spread_penalty <- if (length(parent_counts) == 0L) 1 else mean(pmin(parent_counts - 1L, 4L)) / 4
  confidence <- max(0, min(1, nesting_score - 0.25 * spread_penalty))
  list(
    nesting_score = nesting_score,
    confidence = confidence,
    child_repeats_across_parents = child_repeats,
    composite_groups = composite_groups
  )
}

.default_hierarchy_parent <- function(x, cols) {
  if (length(cols) == 0L) return(NULL)
  lower <- tolower(cols)
  priority <- c("region", "group", "county", "district", "zone", "state", "province")
  for (p in priority) {
    hit <- which(lower == p)
    if (length(hit) > 0L) return(cols[[hit[[1L]]]])
  }
  n_unique <- vapply(cols, function(col) length(unique(.hierarchy_clean(x[[col]]))), integer(1L))
  cols[[which.min(n_unique)]]
}

.check_branch_number <- function(x, name, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < min || x > max) {
    if (is.finite(min) && is.finite(max)) {
      msg <- paste0("`", name, "` must be a number between ", min, " and ", max, ".")
    } else if (is.finite(min)) {
      msg <- paste0("`", name, "` must be a number greater than or equal to ", min, ".")
    } else {
      msg <- paste0("`", name, "` must be a finite number.")
    }
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

.short_branch_label <- function(x, n) {
  x <- as.character(x)
  too_long <- nchar(x) > n
  x[too_long] <- paste0(substr(x[too_long], 1L, n - 3L), "...")
  x
}

.empty_branch_bloom_labels <- function() {
  data.frame(
    label_id = character(),
    region = character(),
    label = character(),
    x = numeric(),
    y = numeric(),
    label_level = character(),
    label_parent = character(),
    connector = logical(),
    label_type = character(),
    stringsAsFactors = FALSE
  )
}
