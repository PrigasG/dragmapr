#' Create a dragmapr state object
#'
#' A `dragmapr_state` stores the mutable, editorial layout of a draggable map
#' separately from the source geometry and from display options. It is the
#' shared composition contract handed between layout producers (such as
#' `explodemap::as_dragmapr_state()`), the interactive editor, and static
#' renderers.
#'
#' The state is deliberately geometry-free: it carries *deltas*, not absolute
#' geometry, and is bound to features at apply time via a join key. That key is
#' the `region` column of the offset tables. For durable round-trips prefer a
#' stable code (e.g. a FIPS/ISO id) over a display name, and record which
#' geometry the state was composed against with `geometry_id`.
#'
#' @param level Character label for the active geography level.
#' @param region_offsets Data frame with `region`, `dx_m`, and `dy_m`.
#' @param label_offsets Data frame with `label_id`, `region`, `dx_m`, and
#'   `dy_m`.
#' @param expanded_groups Character vector of expanded parent groups.
#' @param view Optional list describing the client view, such as scale and
#'   translation.
#' @param version Integer state revision.
#' @param crs Optional coordinate reference system the metre offsets are
#'   expressed in. Accepts anything `sf::st_crs()` understands (an EPSG code,
#'   a PROJ/WKT string, or a `crs` object). Stored as an EPSG integer when
#'   available, otherwise as a WKT string, so it round-trips through JSON.
#'   Because `dx_m`/`dy_m` are only meaningful in a projected CRS, recording it
#'   here makes a saved state safe to reapply in a later session.
#' @param geometry_id Optional single string identifying the geometry this
#'   state was composed against (provenance / binding tag). Lets a downstream
#'   consumer detect when a state is being applied to a different dataset than
#'   it was authored for.
#' @param selected_feature Optional single string naming the feature currently
#'   selected in a dashboard or editor. Carried so a composition can be
#'   restored with focus intact.
#'
#' @return An object of class `"dragmapr_state"`.
#' @export
dragmapr_state <- function(level = "region",
                           region_offsets = NULL,
                           label_offsets = NULL,
                           expanded_groups = character(),
                           view = NULL,
                           version = 0L,
                           crs = NULL,
                           geometry_id = NULL,
                           selected_feature = NULL) {
  if (!is.character(level) || length(level) != 1L || is.na(level) || !nzchar(level)) {
    stop("`level` must be a single non-empty string.", call. = FALSE)
  }
  region_offsets <- if (is.null(region_offsets)) {
    data.frame(region = character(), dx_m = numeric(), dy_m = numeric())
  } else {
    normalize_offsets(region_offsets, source = "`region_offsets`")
  }
  label_offsets <- if (is.null(label_offsets)) {
    data.frame(label_id = character(), region = character(), dx_m = numeric(), dy_m = numeric())
  } else {
    normalize_label_state(label_offsets, source = "`label_offsets`")
  }
  expanded_groups <- as.character(expanded_groups %||% character())
  expanded_groups <- expanded_groups[!is.na(expanded_groups) & nzchar(expanded_groups)]
  version <- suppressWarnings(as.integer(version))
  if (length(version) != 1L || is.na(version) || version < 0L) {
    stop("`version` must be a non-negative whole number.", call. = FALSE)
  }
  if (!is.null(view) && !is.list(view)) {
    stop("`view` must be NULL or a list.", call. = FALSE)
  }
  crs <- normalize_state_crs(crs)
  geometry_id <- normalize_state_scalar(geometry_id, "geometry_id")
  selected_feature <- normalize_state_scalar(selected_feature, "selected_feature")

  structure(
    list(
      level = unname(level),
      region_offsets = region_offsets,
      label_offsets = label_offsets,
      expanded_groups = unique(expanded_groups),
      view = view,
      version = version,
      crs = crs,
      geometry_id = geometry_id,
      selected_feature = selected_feature
    ),
    class = "dragmapr_state"
  )
}

# Normalize a CRS to a JSON-safe, sf-reconsumable scalar (EPSG integer or WKT
# string). Returns NULL when no CRS is supplied.
normalize_state_crs <- function(crs) {
  if (is.null(crs)) {
    return(NULL)
  }
  parsed <- if (inherits(crs, "crs")) crs else sf::st_crs(crs)
  if (is.na(parsed)) {
    stop("`crs` is not a valid coordinate reference system.", call. = FALSE)
  }
  epsg <- parsed$epsg
  if (!is.null(epsg) && !is.na(epsg)) {
    return(as.integer(epsg))
  }
  parsed$wkt
}

# Monotonic revision bump. Returns one more than the highest supplied revision
# so a state produced by an accepted edit always sorts after its inputs. Used by
# every transform that derives a new state from edits (merge/inherit/collapse).
bump_revision <- function(...) {
  versions <- suppressWarnings(as.integer(c(...)))
  versions <- versions[!is.na(versions)]
  if (length(versions) == 0L) {
    return(1L)
  }
  max(versions) + 1L
}

# Normalize an optional scalar string field (NULL or a single non-empty string).
normalize_state_scalar <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be NULL or a single non-empty string.", call. = FALSE)
  }
  x
}

#' Validate a dragmapr state object
#'
#' @param state A `dragmapr_state` object.
#'
#' @return The validated state, invisibly.
#' @export
validate_dragmapr_state <- function(state) {
  if (!inherits(state, "dragmapr_state")) {
    stop("`state` must be created by dragmapr_state().", call. = FALSE)
  }
  dragmapr_state(
    level = state$level,
    region_offsets = state$region_offsets,
    label_offsets = state$label_offsets,
    expanded_groups = state$expanded_groups,
    view = state$view,
    version = state$version,
    crs = state$crs,
    geometry_id = state$geometry_id,
    selected_feature = state$selected_feature
  )
}

#' Compare dragmapr states
#'
#' `dragmapr_state_diff()` reports composition and optional interaction changes
#' between two states. `dragmapr_state_equal()` is the corresponding predicate.
#'
#' @param draft,canonical,a,b `dragmapr_state` objects.
#' @param tolerance Numeric tolerance for offset changes.
#' @param compare One of `"composition"`, `"interaction"`, or `"all"`.
#'
#' @return `dragmapr_state_diff()` returns a `dragmapr_state_diff` object.
#'   `dragmapr_state_equal()` returns `TRUE` or `FALSE`.
#' @export
dragmapr_state_diff <- function(draft,
                                canonical,
                                tolerance = 1,
                                compare = c("composition", "interaction", "all")) {
  draft <- validate_dragmapr_state(draft)
  canonical <- validate_dragmapr_state(canonical)
  compare <- match.arg(compare)
  tolerance <- suppressWarnings(as.numeric(tolerance))
  if (length(tolerance) != 1L || is.na(tolerance) || tolerance < 0) {
    stop("`tolerance` must be a non-negative number.", call. = FALSE)
  }

  region_changes <- diff_offset_table(
    draft$region_offsets,
    canonical$region_offsets,
    key = "region",
    tolerance = tolerance
  )
  label_changes <- diff_offset_table(
    draft$label_offsets,
    canonical$label_offsets,
    key = "label_id",
    tolerance = tolerance
  )

  selected_feature_changed <- !identical(draft$selected_feature, canonical$selected_feature)
  viewport_changed <- !identical(draft$view, canonical$view)
  expanded_groups_changed <- !setequal(draft$expanded_groups, canonical$expanded_groups)
  geometry_id_changed <- !identical(draft$geometry_id, canonical$geometry_id)
  crs_changed <- !identical(draft$crs, canonical$crs)
  version_changed <- !identical(draft$version, canonical$version)

  composition_changed <- nrow(region_changes) > 0L || nrow(label_changes) > 0L
  interaction_changed <- selected_feature_changed || viewport_changed || expanded_groups_changed
  changed <- switch(
    compare,
    composition = composition_changed,
    interaction = interaction_changed,
    all = composition_changed || interaction_changed || geometry_id_changed ||
      crs_changed || version_changed
  )

  structure(
    list(
      changed = changed,
      region_count = nrow(region_changes),
      label_count = nrow(label_changes),
      regions = region_changes,
      labels = label_changes,
      changed_regions = region_changes$region,
      changed_labels = label_changes$label_id,
      selected_feature_changed = selected_feature_changed,
      viewport_changed = viewport_changed,
      expanded_groups_changed = expanded_groups_changed,
      geometry_id_changed = geometry_id_changed,
      crs_changed = crs_changed,
      version_changed = version_changed,
      compare = compare,
      tolerance = tolerance
    ),
    class = "dragmapr_state_diff"
  )
}

#' @rdname dragmapr_state_diff
#' @export
dragmapr_state_equal <- function(a,
                                 b,
                                 tolerance = 1,
                                 compare = c("composition", "interaction", "all")) {
  !dragmapr_state_diff(a, b, tolerance = tolerance, compare = compare)$changed
}

diff_offset_table <- function(draft, canonical, key, tolerance) {
  all_keys <- sort(unique(c(as.character(draft[[key]]), as.character(canonical[[key]]))))
  draft <- draft[match(all_keys, as.character(draft[[key]])), , drop = FALSE]
  canonical <- canonical[match(all_keys, as.character(canonical[[key]])), , drop = FALSE]

  dx_draft <- zero_missing(draft$dx_m)
  dy_draft <- zero_missing(draft$dy_m)
  dx_base <- zero_missing(canonical$dx_m)
  dy_base <- zero_missing(canonical$dy_m)
  dx_change <- dx_draft - dx_base
  dy_change <- dy_draft - dy_base
  changed <- abs(dx_change) > tolerance | abs(dy_change) > tolerance

  out <- data.frame(
    key = all_keys[changed],
    dx_change = dx_change[changed],
    dy_change = dy_change[changed],
    draft_dx_m = dx_draft[changed],
    draft_dy_m = dy_draft[changed],
    canonical_dx_m = dx_base[changed],
    canonical_dy_m = dy_base[changed],
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "key"] <- key
  out
}

zero_missing <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
}

#' @export
print.dragmapr_state_diff <- function(x, ...) {
  cat("dragmapr state diff\n")
  cat("Changed: ", if (isTRUE(x$changed)) "yes" else "no", "\n", sep = "")
  cat("Regions moved: ", x$region_count, "\n", sep = "")
  cat("Labels moved: ", x$label_count, "\n", sep = "")
  invisible(x)
}

#' Summarise a dragmapr state
#'
#' @param object A `dragmapr_state` object.
#' @param ... Unused.
#'
#' @return A `summary.dragmapr_state` object.
#' @export
summary.dragmapr_state <- function(object, ...) {
  state <- validate_dragmapr_state(object)
  moved_regions <- state$region_offsets[
    abs(state$region_offsets$dx_m) > 0 | abs(state$region_offsets$dy_m) > 0,
    , drop = FALSE
  ]
  moved_labels <- state$label_offsets[
    abs(state$label_offsets$dx_m) > 0 | abs(state$label_offsets$dy_m) > 0,
    , drop = FALSE
  ]
  structure(
    list(
      geometry_id = state$geometry_id,
      revision = state$version,
      regions_moved = nrow(moved_regions),
      labels_moved = nrow(moved_labels),
      selected_feature = state$selected_feature,
      crs = state$crs,
      level = state$level
    ),
    class = "summary.dragmapr_state"
  )
}

#' @export
print.summary.dragmapr_state <- function(x, ...) {
  cat("dragmapr state\n")
  cat("Geometry: ", x$geometry_id %||% "<none>", "\n", sep = "")
  cat("Revision: ", x$revision, "\n", sep = "")
  cat("Regions moved: ", x$regions_moved, "\n", sep = "")
  cat("Labels moved: ", x$labels_moved, "\n", sep = "")
  cat("Selected feature: ", x$selected_feature %||% "<none>", "\n", sep = "")
  cat("CRS: ", x$crs %||% "<none>", "\n", sep = "")
  invisible(x)
}

#' Merge dragmapr state updates
#'
#' @param state Base `dragmapr_state`.
#' @param update Update `dragmapr_state`. Non-empty offset tables replace rows
#'   in `state` by key.
#'
#' @return A merged `dragmapr_state`. Its `version` is one greater than the
#'   higher of the two input revisions, so the merged state always sorts after
#'   both of its inputs.
#' @export
merge_dragmapr_state <- function(state, update) {
  state <- validate_dragmapr_state(state)
  update <- validate_dragmapr_state(update)

  region_offsets <- merge_state_rows(state$region_offsets, update$region_offsets, "region")
  label_offsets <- merge_state_rows(state$label_offsets, update$label_offsets, "label_id")
  dragmapr_state(
    level = update$level %||% state$level,
    region_offsets = region_offsets,
    label_offsets = label_offsets,
    expanded_groups = update$expanded_groups,
    view = update$view %||% state$view,
    version = bump_revision(state$version, update$version),
    crs = update$crs %||% state$crs,
    geometry_id = update$geometry_id %||% state$geometry_id,
    selected_feature = update$selected_feature %||% state$selected_feature
  )
}

merge_state_rows <- function(base, update, key) {
  if (nrow(update) == 0L) {
    return(base)
  }
  kept <- base[!base[[key]] %in% update[[key]], , drop = FALSE]
  out <- rbind(kept, update)
  out[order(out[[key]]), , drop = FALSE]
}

#' Snapshot or restore dragmapr state
#'
#' @param state A `dragmapr_state` object.
#' @param snapshot A list previously returned by `snapshot_dragmapr_state()`.
#'
#' @return `snapshot_dragmapr_state()` returns a plain list.
#'   `restore_dragmapr_state()` returns a `dragmapr_state`.
#' @export
snapshot_dragmapr_state <- function(state) {
  state <- validate_dragmapr_state(state)
  out <- unclass(state)
  # Drop NULL-valued optional fields so the JSON stays clean; restore defaults
  # them back to NULL, keeping older snapshots (without these keys) readable.
  out[!vapply(out, is.null, logical(1))]
}

#' @rdname snapshot_dragmapr_state
#' @export
restore_dragmapr_state <- function(snapshot) {
  if (!is.list(snapshot)) {
    stop("`snapshot` must be a list.", call. = FALSE)
  }
  dragmapr_state(
    level = snapshot$level %||% "region",
    region_offsets = restore_state_table(snapshot$region_offsets, "region"),
    label_offsets = restore_state_table(snapshot$label_offsets, "label"),
    expanded_groups = snapshot$expanded_groups %||% character(),
    view = snapshot$view,
    version = snapshot$version %||% 0L,
    crs = snapshot$crs %||% NULL,
    geometry_id = snapshot$geometry_id %||% NULL,
    selected_feature = snapshot$selected_feature %||% NULL
  )
}

restore_state_table <- function(x, type = c("region", "label")) {
  type <- match.arg(type)
  required <- switch(
    type,
    region = c("region", "dx_m", "dy_m"),
    label = c("label_id", "region", "dx_m", "dy_m")
  )
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(x) || nrow(x) == 0L) {
    out <- as.data.frame(stats::setNames(rep(list(character()), length(required)), required))
    for (nm in intersect(c("dx_m", "dy_m"), names(out))) out[[nm]] <- numeric()
    return(out[0L, , drop = FALSE])
  }
  names(x) <- tolower(names(x))
  if (!all(required %in% names(x))) {
    return(NULL)
  }
  x[, required, drop = FALSE]
}

#' Read and write dragmapr state JSON
#'
#' @param state A `dragmapr_state` object.
#' @param path File path.
#'
#' @return `write_dragmapr_state()` invisibly returns `path`;
#'   `read_dragmapr_state()` returns a `dragmapr_state`.
#' @export
write_dragmapr_state <- function(state, path) {
  state <- validate_dragmapr_state(state)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  jsonlite::write_json(
    snapshot_dragmapr_state(state),
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows"
  )
  invisible(path)
}

#' @rdname write_dragmapr_state
#' @export
read_dragmapr_state <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  restore_dragmapr_state(jsonlite::read_json(path, simplifyVector = TRUE))
}

#' Apply a dragmapr state to sf geometry
#'
#' @param x An `sf` object.
#' @param state A `dragmapr_state` object.
#' @param region_col Column in `x` defining draggable groups.
#'
#' @return An `sf` object with region offsets applied.
#' @export
apply_dragmapr_state <- function(x, state, region_col) {
  state <- validate_dragmapr_state(state)
  if (!is.null(state$crs) && inherits(x, "sf")) {
    target <- sf::st_crs(x)
    authored <- tryCatch(sf::st_crs(state$crs), error = function(e) NA)
    if (!is.na(authored) && !is.na(target) && authored != target) {
      warning(
        "State was composed in a different CRS than `x`. ",
        "Metre offsets may be misplaced; reproject `x` to the state CRS first.",
        call. = FALSE
      )
    }
  }
  apply_offsets(x, state$region_offsets, region_col = region_col)
}

#' Inherit drag offsets between hierarchy levels
#'
#' @param state A `dragmapr_state` object.
#' @param from,to Column names in `relation` describing the source and target
#'   levels.
#' @param relation Data frame mapping parent keys to child keys.
#'
#' @return A `dragmapr_state` at level `to`.
#' @export
inherit_drag_offsets <- function(state, from, to, relation) {
  state <- validate_dragmapr_state(state)
  relation <- validate_offset_relation(relation, from, to)
  parent_offsets <- state$region_offsets
  match_idx <- match(as.character(relation[[from]]), parent_offsets$region)
  row_dx <- ifelse(is.na(match_idx), 0, parent_offsets$dx_m[match_idx])
  row_dy <- ifelse(is.na(match_idx), 0, parent_offsets$dy_m[match_idx])

  out <- stats::aggregate(
    cbind(dx_m, dy_m) ~ region,
    data = data.frame(
      region = as.character(relation[[to]]),
      dx_m = row_dx,
      dy_m = row_dy,
      stringsAsFactors = FALSE
    ),
    FUN = mean
  )
  dragmapr_state(
    level = to,
    region_offsets = out[, c("region", "dx_m", "dy_m")],
    label_offsets = state$label_offsets,
    expanded_groups = state$expanded_groups,
    view = state$view,
    version = bump_revision(state$version),
    crs = state$crs,
    geometry_id = state$geometry_id,
    selected_feature = NULL
  )
}

#' Collapse drag offsets between hierarchy levels
#'
#' @param state A `dragmapr_state` object.
#' @param from,to Column names in `relation` describing the source and target
#'   levels.
#' @param relation Data frame mapping child keys to parent keys.
#' @param method Collapse method. Currently `"centroid"` averages child
#'   offsets for each parent.
#'
#' @return A `dragmapr_state` at level `to`.
#' @export
collapse_drag_offsets <- function(state, from, to, relation, method = "centroid") {
  state <- validate_dragmapr_state(state)
  method <- match.arg(method, "centroid")
  relation <- validate_offset_relation(relation, from, to)
  child_offsets <- state$region_offsets
  match_idx <- match(as.character(relation[[from]]), child_offsets$region)
  matched <- !is.na(match_idx)
  if (!any(matched)) {
    offsets <- data.frame(region = character(), dx_m = numeric(), dy_m = numeric())
  } else {
    offsets <- stats::aggregate(
      cbind(dx_m, dy_m) ~ region,
      data = data.frame(
        region = as.character(relation[[to]][matched]),
        dx_m = child_offsets$dx_m[match_idx[matched]],
        dy_m = child_offsets$dy_m[match_idx[matched]],
        stringsAsFactors = FALSE
      ),
      FUN = mean
    )
    offsets <- offsets[, c("region", "dx_m", "dy_m")]
  }
  dragmapr_state(
    level = to,
    region_offsets = offsets,
    label_offsets = state$label_offsets,
    expanded_groups = character(),
    view = state$view,
    version = bump_revision(state$version),
    crs = state$crs,
    geometry_id = state$geometry_id,
    selected_feature = NULL
  )
}

validate_offset_relation <- function(relation, from, to) {
  if (!is.data.frame(relation)) {
    stop("`relation` must be a data frame.", call. = FALSE)
  }
  from <- as.character(from)
  to <- as.character(to)
  if (length(from) != 1L || is.na(from) || !nzchar(from) ||
      length(to) != 1L || is.na(to) || !nzchar(to)) {
    stop("`from` and `to` must be single non-empty column names.", call. = FALSE)
  }
  missing <- setdiff(c(from, to), names(relation))
  if (length(missing) > 0L) {
    stop("`relation` is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  relation <- relation[!is.na(relation[[from]]) & !is.na(relation[[to]]), c(from, to), drop = FALSE]
  relation[[from]] <- as.character(relation[[from]])
  relation[[to]] <- as.character(relation[[to]])
  relation <- relation[nzchar(relation[[from]]) & nzchar(relation[[to]]), , drop = FALSE]
  unique(relation)
}
