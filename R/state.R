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
#' recorded in `region_col`, while `level` remains the human geography level
#' label. For durable round-trips prefer a stable code (e.g. a FIPS/ISO id)
#' over a display name, and record which geometry the state was composed
#' against with `geometry_id`.
#'
#' @param level Character label for the active geography level.
#' @param region_col Source-geometry column used to join `region_offsets$region`
#'   back to features. Defaults to `level` for old/simple states, but should be
#'   set explicitly for saved projects and package handoffs.
#' @param label_id_col Source or label-table column identifying draggable
#'   labels.
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
#' @param binding Optional list of binding metadata. When supplied,
#'   `binding$region_col` and `binding$label_id_col` are used as defaults for
#'   the corresponding top-level arguments.
#' @param schema_version State schema version. Stored with JSON snapshots so
#'   future migrations can read older projects.
#' @param package_version Package version that created the state.
#'
#' @return An object of class `"dragmapr_state"`.
#' @export
dragmapr_state <- function(level = "region",
                           region_col = NULL,
                           label_id_col = NULL,
                           region_offsets = NULL,
                           label_offsets = NULL,
                           expanded_groups = character(),
                           view = NULL,
                           version = 0L,
                           crs = NULL,
                           geometry_id = NULL,
                           selected_feature = NULL,
                           binding = NULL,
                           schema_version = "1.1.0",
                           package_version = as.character(utils::packageVersion("dragmapr"))) {
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
  binding <- normalize_state_binding(binding, region_col, label_id_col, level)
  region_col <- binding$region_col
  label_id_col <- binding$label_id_col
  schema_version <- normalize_state_scalar(schema_version, "schema_version")
  package_version <- normalize_state_scalar(package_version, "package_version")

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
      selected_feature = selected_feature,
      region_col = region_col,
      label_id_col = label_id_col,
      binding = binding,
      schema_version = schema_version,
      package_version = package_version
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

normalize_state_binding <- function(binding = NULL,
                                    region_col = NULL,
                                    label_id_col = NULL,
                                    level = "region") {
  if (!is.null(binding) && !is.list(binding)) {
    stop("`binding` must be NULL or a list.", call. = FALSE)
  }
  region_col <- region_col %||% binding$region_col %||% level
  label_id_col <- label_id_col %||% binding$label_id_col %||% "label_id"
  list(
    region_col = normalize_state_scalar(region_col, "region_col"),
    label_id_col = normalize_state_scalar(label_id_col, "label_id_col")
  )
}

state_region_col <- function(state, region_col = NULL, required = TRUE) {
  region_col <- region_col %||% state$region_col %||% state$binding$region_col %||% state$level
  if (isTRUE(required)) {
    region_col <- normalize_state_scalar(region_col, "region_col")
  }
  region_col
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
    selected_feature = state$selected_feature,
    region_col = state$region_col %||% state$binding$region_col %||% state$level,
    label_id_col = state$label_id_col %||% state$binding$label_id_col %||% "label_id",
    binding = state$binding,
    schema_version = state$schema_version %||% "1.1.0",
    package_version = state$package_version %||% "0.0.0"
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
  draft_regions <- as.character(draft$region_offsets$region)
  canonical_regions <- as.character(canonical$region_offsets$region)
  draft_labels <- as.character(draft$label_offsets$label_id)
  canonical_labels <- as.character(canonical$label_offsets$label_id)
  added_regions <- setdiff(draft_regions, canonical_regions)
  removed_regions <- setdiff(canonical_regions, draft_regions)
  added_labels <- setdiff(draft_labels, canonical_labels)
  removed_labels <- setdiff(canonical_labels, draft_labels)

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
      moved_regions = region_changes,
      moved_labels = label_changes,
      added_regions = added_regions,
      removed_regions = removed_regions,
      added_labels = added_labels,
      removed_labels = removed_labels,
      changed_regions = region_changes$region,
      changed_labels = label_changes$label_id,
      selected_feature_changed = selected_feature_changed,
      viewport_changed = viewport_changed,
      expanded_groups_changed = expanded_groups_changed,
      geometry_id_changed = geometry_id_changed,
      crs_changed = crs_changed,
      version_changed = version_changed,
      summary = list(
        region_changes = nrow(region_changes),
        label_changes = nrow(label_changes),
        added_regions = length(added_regions),
        removed_regions = length(removed_regions),
        added_labels = length(added_labels),
        removed_labels = length(removed_labels)
      ),
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
    region_col = update$region_col %||% state$region_col,
    label_id_col = update$label_id_col %||% state$label_id_col,
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
#' @param target_schema_version Schema version to migrate snapshots to before
#'   restoring.
#'
#' @return `snapshot_dragmapr_state()` returns a plain list.
#'   `restore_dragmapr_state()` returns a `dragmapr_state`.
#'   `migrate_dragmapr_state()` returns a migrated plain list.
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
migrate_dragmapr_state <- function(snapshot, target_schema_version = "1.1.0") {
  if (inherits(snapshot, "dragmapr_state")) {
    snapshot <- snapshot_dragmapr_state(snapshot)
  }
  if (!is.list(snapshot)) {
    stop("`snapshot` must be a list.", call. = FALSE)
  }
  target_schema_version <- normalize_state_scalar(target_schema_version, "target_schema_version")
  current <- snapshot$schema_version %||% "1.0.0"
  if (utils::compareVersion(current, target_schema_version) > 0L) {
    stop(
      "State schema version ", current, " is newer than this package supports (",
      target_schema_version, ").",
      call. = FALSE
    )
  }
  binding <- normalize_state_binding(
    snapshot$binding,
    snapshot$region_col,
    snapshot$label_id_col,
    snapshot$level %||% "region"
  )
  snapshot$region_col <- binding$region_col
  snapshot$label_id_col <- binding$label_id_col
  snapshot$binding <- binding
  snapshot$schema_version <- target_schema_version
  snapshot
}

#' @rdname snapshot_dragmapr_state
#' @export
restore_dragmapr_state <- function(snapshot) {
  if (!is.list(snapshot)) {
    stop("`snapshot` must be a list.", call. = FALSE)
  }
  snapshot <- migrate_dragmapr_state(snapshot)
  dragmapr_state(
    level = snapshot$level %||% "region",
    region_col = snapshot$region_col,
    label_id_col = snapshot$label_id_col,
    region_offsets = restore_state_table(snapshot$region_offsets, "region"),
    label_offsets = restore_state_table(snapshot$label_offsets, "label"),
    expanded_groups = snapshot$expanded_groups %||% character(),
    view = snapshot$view,
    version = snapshot$version %||% 0L,
    crs = snapshot$crs %||% NULL,
    geometry_id = snapshot$geometry_id %||% NULL,
    selected_feature = snapshot$selected_feature %||% NULL,
    binding = snapshot$binding %||% NULL,
    schema_version = snapshot$schema_version %||% "1.1.0",
    package_version = snapshot$package_version %||% "0.0.0"
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
#' @param region_col Column in `x` defining draggable groups. Defaults to the
#'   binding metadata stored in `state`.
#'
#' @return An `sf` object with region offsets applied.
#' @export
apply_dragmapr_state <- function(x, state, region_col = NULL) {
  state <- validate_dragmapr_state(state)
  region_col <- state_region_col(state, region_col)
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
    region_col = to,
    label_id_col = state$label_id_col,
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
    region_col = to,
    label_id_col = state$label_id_col,
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

#' Safely update region offsets in a dragmapr state
#'
#' @param state A `dragmapr_state`.
#' @param region Region ID to update.
#' @param dx_m,dy_m New or incremental offsets. `NULL` preserves the current
#'   value for that axis.
#' @param mode `"replace"` sets the supplied values; `"increment"` adds them to
#'   the current values.
#'
#' @return An updated `dragmapr_state` with a bumped version.
#' @export
update_region_offset <- function(state,
                                 region,
                                 dx_m = NULL,
                                 dy_m = NULL,
                                 mode = c("replace", "increment")) {
  state <- validate_dragmapr_state(state)
  region <- state_key_scalar(region, "region")
  mode <- match.arg(mode)
  state$region_offsets <- update_offset_table(
    state$region_offsets,
    key = "region",
    id = region,
    dx_m = dx_m,
    dy_m = dy_m,
    mode = mode,
    extra = list()
  )
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' Safely update label offsets in a dragmapr state
#'
#' @param state A `dragmapr_state`.
#' @param label_id Label ID to update.
#' @param dx_m,dy_m New or incremental offsets. `NULL` preserves the current
#'   value for that axis.
#' @param mode `"replace"` sets the supplied values; `"increment"` adds them to
#'   the current values.
#' @param region Optional region for a new label row. Defaults to `label_id`.
#'
#' @return An updated `dragmapr_state` with a bumped version.
#' @export
update_label_offset <- function(state,
                                label_id,
                                dx_m = NULL,
                                dy_m = NULL,
                                mode = c("replace", "increment"),
                                region = NULL) {
  state <- validate_dragmapr_state(state)
  label_id <- state_key_scalar(label_id, "label_id")
  region <- if (is.null(region)) label_id else state_key_scalar(region, "region")
  mode <- match.arg(mode)
  state$label_offsets <- update_offset_table(
    state$label_offsets,
    key = "label_id",
    id = label_id,
    dx_m = dx_m,
    dy_m = dy_m,
    mode = mode,
    extra = list(region = region)
  )
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' Reset offsets in a dragmapr state
#'
#' @param state A `dragmapr_state`.
#' @param region,label_id Region or label IDs to reset.
#'
#' @return An updated `dragmapr_state` with a bumped version.
#' @export
reset_region <- function(state, region) {
  state <- validate_dragmapr_state(state)
  ids <- state_key_vector(region, "region")
  state$region_offsets <- state$region_offsets[
    !as.character(state$region_offsets$region) %in% ids,
    ,
    drop = FALSE
  ]
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' @rdname reset_region
#' @export
reset_regions <- reset_region

#' @rdname reset_region
#' @export
reset_label <- function(state, label_id) {
  state <- validate_dragmapr_state(state)
  ids <- state_key_vector(label_id, "label_id")
  state$label_offsets <- state$label_offsets[
    !as.character(state$label_offsets$label_id) %in% ids,
    ,
    drop = FALSE
  ]
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' @rdname reset_region
#' @export
reset_labels <- function(state) {
  state <- validate_dragmapr_state(state)
  state$label_offsets <- state$label_offsets[0L, , drop = FALSE]
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' @rdname reset_region
#' @export
reset_all <- function(state) {
  state <- validate_dragmapr_state(state)
  state$region_offsets <- state$region_offsets[0L, , drop = FALSE]
  state$label_offsets <- state$label_offsets[0L, , drop = FALSE]
  state$expanded_groups <- character()
  state$selected_feature <- NULL
  state$version <- bump_revision(state$version)
  validate_dragmapr_state(state)
}

#' Validate compatibility between geometry and dragmapr state
#'
#' @param x An `sf` object or layout-like object.
#' @param state A `dragmapr_state`.
#' @param region_col Region column in `x`. Defaults to the binding metadata
#'   stored in `state`.
#' @param geometry_id Optional expected geometry ID or fingerprint.
#' @param strict Treat warnings as invalid.
#'
#' @return A `dragmapr_compatibility` object with structured findings.
#' @export
validate_state_compatibility <- function(x,
                                         state,
                                         region_col = NULL,
                                         geometry_id = NULL,
                                         strict = TRUE) {
  state <- validate_dragmapr_state(state)
  strict <- flag_scalar(strict, "`strict`")
  sf_obj <- coerce_compatibility_sf(x)
  region_col <- state_region_col(state, region_col)
  errors <- character()
  warnings <- character()
  recommendations <- character()

  if (!inherits(sf_obj, "sf")) {
    errors <- c(errors, "`x` must be an sf object or layout-like object with sf geometry.")
  } else if (!region_col %in% names(sf_obj)) {
    errors <- c(errors, paste0("region_col '", region_col, "' not found in geometry."))
  } else {
    geometry_regions <- unique(as.character(sf_obj[[region_col]]))
    state_regions <- unique(as.character(state$region_offsets$region))
    missing_regions <- setdiff(state_regions, geometry_regions)
    unused_regions <- setdiff(geometry_regions, state_regions)
    if (length(missing_regions)) {
      errors <- c(errors, paste0(
        "State contains region(s) not present in geometry: ",
        paste(utils::head(missing_regions, 8), collapse = ", "),
        if (length(missing_regions) > 8L) ", ..." else ""
      ))
    }
    if (length(unused_regions)) {
      warnings <- c(warnings, paste0(
        "Geometry contains region(s) with no state row; zero movement will be used for ",
        length(unused_regions), " region(s)."
      ))
    }
  }

  if (!is.null(state$crs) && inherits(sf_obj, "sf")) {
    authored <- tryCatch(sf::st_crs(state$crs), error = function(e) NA)
    target <- tryCatch(sf::st_crs(sf_obj), error = function(e) NA)
    if (!is.na(authored) && !is.na(target) && authored != target) {
      errors <- c(errors, "State CRS does not match geometry CRS.")
      recommendations <- c(recommendations, "Reproject the geometry to the state CRS or rebuild the state.")
    }
  }

  expected_geometry <- geometry_id %||% attr(x, "geometry_id", exact = TRUE)
  if (is.null(expected_geometry) && is.list(x) && !is.null(x$diagnostics$label)) {
    expected_geometry <- x$diagnostics$label
  }
  if (!is.null(expected_geometry) && !is.null(state$geometry_id) &&
      !identical(as.character(expected_geometry), as.character(state$geometry_id))) {
    errors <- c(errors, "State geometry_id does not match the supplied geometry.")
    recommendations <- c(recommendations, "Use the matching source geometry or migrate/rebuild the state.")
  }

  out <- list(
    valid = length(errors) == 0L && (!strict || length(warnings) == 0L),
    errors = unique(errors),
    warnings = unique(warnings),
    recommendations = unique(recommendations),
    metrics = list(
      state_regions = nrow(state$region_offsets),
      state_labels = nrow(state$label_offsets),
      geometry_regions = if (inherits(sf_obj, "sf") && region_col %in% names(sf_obj)) {
        length(unique(as.character(sf_obj[[region_col]])))
      } else {
        NA_integer_
      }
    ),
    strict = strict
  )
  structure(out, class = "dragmapr_compatibility")
}

#' @export
print.dragmapr_compatibility <- function(x, ...) {
  cat("dragmapr compatibility\n")
  cat("Status: ", if (isTRUE(x$valid)) "valid" else "invalid", "\n", sep = "")
  if (length(x$errors)) {
    cat("Errors:\n")
    cat(paste0("- ", x$errors, collapse = "\n"), "\n")
  }
  if (length(x$warnings)) {
    cat("Warnings:\n")
    cat(paste0("- ", x$warnings, collapse = "\n"), "\n")
  }
  invisible(x)
}

state_key_scalar <- function(x, arg) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
  }
  x
}

state_key_vector <- function(x, arg) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) {
    stop("`", arg, "` must contain at least one non-empty string.", call. = FALSE)
  }
  unique(x)
}

update_offset_table <- function(offsets, key, id, dx_m, dy_m, mode, extra) {
  idx <- match(id, as.character(offsets[[key]]))
  current_dx <- if (is.na(idx)) 0 else offsets$dx_m[[idx]]
  current_dy <- if (is.na(idx)) 0 else offsets$dy_m[[idx]]
  dx <- if (is.null(dx_m)) current_dx else numeric_offset_scalar(dx_m, "dx_m")
  dy <- if (is.null(dy_m)) current_dy else numeric_offset_scalar(dy_m, "dy_m")
  if (identical(mode, "increment")) {
    dx <- current_dx + if (is.null(dx_m)) 0 else numeric_offset_scalar(dx_m, "dx_m")
    dy <- current_dy + if (is.null(dy_m)) 0 else numeric_offset_scalar(dy_m, "dy_m")
  }
  row <- offsets[NA_integer_, , drop = FALSE]
  row[[key]] <- id
  if ("region" %in% names(row) && key != "region") {
    row$region <- extra$region %||% id
  }
  row$dx_m <- dx
  row$dy_m <- dy
  out <- offsets
  if (is.na(idx)) {
    out <- rbind(out, row[, names(out), drop = FALSE])
  } else {
    out[idx, names(row)] <- row
  }
  out[order(out[[key]]), , drop = FALSE]
}

numeric_offset_scalar <- function(x, arg) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x)) {
    stop("`", arg, "` must be a single finite number.", call. = FALSE)
  }
  x
}

coerce_compatibility_sf <- function(x) {
  if (inherits(x, "sf")) {
    return(x)
  }
  if (is.list(x)) {
    for (nm in c("sf_grouped", "sf", "data", "source")) {
      if (inherits(x[[nm]], "sf")) return(x[[nm]])
    }
  }
  NULL
}
