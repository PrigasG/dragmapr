#' Create a dragmapr hierarchy state
#'
#' A hierarchy state keeps one parent/root `dragmapr_state` together with any
#' child-level states created while drilling into the map. It is useful for apps
#' that move between levels such as divisions, states, counties, or
#' municipalities without losing each level's edits.
#'
#' @param root A root [d_state()].
#' @param children Named list of child [d_state()] objects.
#' @param active_path Character vector describing the active path through the
#'   hierarchy.
#' @param relationships Optional normalized table from
#'   [d_relationships()].
#' @param version Integer hierarchy revision.
#'
#' @return An object of class `"dragmapr_hierarchy_state"`.
#' @export
d_hierarchy_state <- function(root = d_state(),
                                     children = list(),
                                     active_path = character(),
                                     relationships = NULL,
                                     version = 0L) {
  root <- validate_dragmapr_state(root)
  if (!is.list(children) ||
      (length(children) > 0L && (is.null(names(children)) || any(!nzchar(names(children)))))) {
    stop("`children` must be a named list.", call. = FALSE)
  }
  children <- lapply(children, validate_dragmapr_state)
  relationships <- normalize_dragmapr_relationships(relationships)
  active_path <- as.character(active_path %||% character())
  active_path <- active_path[!is.na(active_path) & nzchar(active_path)]
  version <- suppressWarnings(as.integer(version))
  if (length(version) != 1L || is.na(version) || version < 0L) {
    stop("`version` must be a non-negative whole number.", call. = FALSE)
  }
  structure(
    list(
      root = root,
      children = children,
      active_path = active_path,
      relationships = relationships,
      version = version
    ),
    class = "dragmapr_hierarchy_state"
  )
}

#' Define generic hierarchy relationships
#'
#' Normalizes application vocabulary into a package-neutral edge table. The
#' package understands levels and stable identifiers; terms such as county,
#' parish, municipality, or district remain data values rather than hard-coded
#' assumptions.
#'
#' @param data Data frame containing parent and child identifiers. Pass `NULL`
#'   to create an empty relationship table. A previously normalized table can
#'   be revalidated without the remaining arguments.
#' @param parent_level,child_level Single level labels.
#' @param parent_id,child_id Column names in `data` containing stable IDs.
#'
#' @return A `dragmapr_relationships` data frame with `parent_level`,
#'   `parent_id`, `child_level`, and `child_id`.
#' @export
d_relationships <- function(data = NULL,
                                   parent_level = NULL,
                                   parent_id = NULL,
                                   child_level = NULL,
                                   child_id = NULL) {
  columns <- c("parent_level", "parent_id", "child_level", "child_id")
  if (is.null(data)) {
    out <- as.data.frame(stats::setNames(rep(list(character()), 4L), columns),
                         stringsAsFactors = FALSE)
    return(structure(out, class = c("dragmapr_relationships", "data.frame")))
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or NULL.", call. = FALSE)
  }
  if (all(columns %in% names(data)) &&
      all(vapply(list(parent_level, parent_id, child_level, child_id), is.null, logical(1)))) {
    out <- data[, columns, drop = FALSE]
  } else {
    parent_level <- hierarchy_scalar(parent_level, "parent_level")
    child_level <- hierarchy_scalar(child_level, "child_level")
    parent_id <- hierarchy_column(data, parent_id, "parent_id")
    child_id <- hierarchy_column(data, child_id, "child_id")
    out <- data.frame(
      parent_level = rep(parent_level, nrow(data)),
      parent_id = as.character(data[[parent_id]]),
      child_level = rep(child_level, nrow(data)),
      child_id = as.character(data[[child_id]]),
      stringsAsFactors = FALSE
    )
  }
  for (name in columns) {
    out[[name]] <- trimws(as.character(out[[name]]))
  }
  bad <- !stats::complete.cases(out) |
    !apply(out[, columns, drop = FALSE], 1L, function(row) all(nzchar(row)))
  if (any(bad)) {
    stop("Hierarchy relationships cannot contain missing or empty values.",
         call. = FALSE)
  }
  out <- unique(out)
  rownames(out) <- NULL
  structure(out, class = c("dragmapr_relationships", "data.frame"))
}

#' Query hierarchy relationships
#'
#' @param relationships A [d_relationships()] table or hierarchy state.
#' @param id Stable node identifier.
#' @param level Optional node level used to disambiguate repeated identifiers.
#'
#' @return `children_of()` and `parent_of()` return normalized edge subsets;
#'   `descendants_of()` and `ancestors_of()` return node tables; and
#'   `hierarchy_path()` returns the root-to-node IDs.
#' @export
children_of <- function(relationships, id, level = NULL) {
  rel <- hierarchy_relationship_table(relationships)
  id <- hierarchy_scalar(id, "id")
  keep <- rel$parent_id == id
  if (!is.null(level)) keep <- keep & rel$parent_level == hierarchy_scalar(level, "level")
  rel[keep, , drop = FALSE]
}

#' @rdname children_of
#' @export
parent_of <- function(relationships, id, level = NULL) {
  rel <- hierarchy_relationship_table(relationships)
  id <- hierarchy_scalar(id, "id")
  keep <- rel$child_id == id
  if (!is.null(level)) keep <- keep & rel$child_level == hierarchy_scalar(level, "level")
  rel[keep, , drop = FALSE]
}

#' @rdname children_of
#' @export
descendants_of <- function(relationships, id, level = NULL) {
  rel <- hierarchy_relationship_table(relationships)
  frontier <- data.frame(
    level = if (is.null(level)) NA_character_ else hierarchy_scalar(level, "level"),
    id = hierarchy_scalar(id, "id"), stringsAsFactors = FALSE
  )
  seen <- frontier[0L, , drop = FALSE]
  repeat {
    edges <- do.call(rbind, lapply(seq_len(nrow(frontier)), function(i) {
      children_of(rel, frontier$id[i], if (is.na(frontier$level[i])) NULL else frontier$level[i])
    }))
    if (is.null(edges) || nrow(edges) == 0L) break
    next_nodes <- unique(data.frame(level = edges$child_level, id = edges$child_id,
                                    stringsAsFactors = FALSE))
    key <- paste(next_nodes$level, next_nodes$id, sep = "\r")
    seen_key <- paste(seen$level, seen$id, sep = "\r")
    next_nodes <- next_nodes[!key %in% seen_key, , drop = FALSE]
    if (nrow(next_nodes) == 0L) break
    seen <- rbind(seen, next_nodes)
    frontier <- next_nodes
  }
  rownames(seen) <- NULL
  seen
}

#' @rdname children_of
#' @export
ancestors_of <- function(relationships, id, level = NULL) {
  rel <- hierarchy_relationship_table(relationships)
  current_id <- hierarchy_scalar(id, "id")
  current_level <- if (is.null(level)) NULL else hierarchy_scalar(level, "level")
  out <- data.frame(level = character(), id = character(), stringsAsFactors = FALSE)
  repeat {
    edge <- parent_of(rel, current_id, current_level)
    if (nrow(edge) == 0L) break
    if (nrow(edge) > 1L) {
      stop("Hierarchy node has more than one parent; a unique ancestry path is required.",
           call. = FALSE)
    }
    node <- data.frame(level = edge$parent_level, id = edge$parent_id,
                       stringsAsFactors = FALSE)
    if (paste(node$level, node$id) %in% paste(out$level, out$id)) {
      stop("Hierarchy relationships contain a cycle.", call. = FALSE)
    }
    out <- rbind(out, node)
    current_id <- node$id
    current_level <- node$level
  }
  rownames(out) <- NULL
  out
}

#' @rdname children_of
#' @export
hierarchy_path <- function(relationships, id, level = NULL) {
  ancestors <- ancestors_of(relationships, id, level)
  c(rev(ancestors$id), hierarchy_scalar(id, "id"))
}

#' Operate on a hierarchy state
#'
#' These pure helpers make hierarchy state operational without imposing UI
#' behavior. Paths are character vectors of branch IDs. Child states may be
#' keyed by the full slash-separated path or, for backward compatibility, by
#' the final path component.
#'
#' @param hierarchy A [d_hierarchy_state()].
#' @param path Character branch path. Defaults to the hierarchy's active path
#'   where applicable.
#' @param missing Value returned when no matching state exists.
#' @param dx_m,dy_m Incremental branch movement in projected map units.
#' @param base_offsets Optional algorithmic base offsets for the active state.
#'
#' @return Updated hierarchy states, active/parent states, or composed offset
#'   tables as described by each function.
#' @export
d_set_active_path <- function(hierarchy, path = character()) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  hierarchy$active_path <- hierarchy_path_vector(path)
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' @rdname d_set_active_path
#' @export
d_active_state <- function(hierarchy, path = hierarchy$active_path, missing = NULL) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  path <- hierarchy_path_vector(path)
  if (!length(path)) return(hierarchy$root)
  key <- hierarchy_child_key(hierarchy, path)
  if (is.null(key)) return(missing)
  hierarchy$children[[key]]
}

#' @rdname d_set_active_path
#' @export
d_parent_state <- function(hierarchy, path = hierarchy$active_path, missing = NULL) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  path <- hierarchy_path_vector(path)
  if (length(path) <= 1L) return(hierarchy$root)
  d_active_state(hierarchy, utils::head(path, -1L), missing = missing)
}

#' @rdname d_set_active_path
#' @export
d_branch_offsets <- function(hierarchy, path = hierarchy$active_path) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  path <- hierarchy_path_vector(path)
  rows <- lapply(seq_along(path), function(i) {
    container <- if (i == 1L) hierarchy$root else
      d_active_state(hierarchy, path[seq_len(i - 1L)])
    value <- hierarchy_offset_for(container, path[i])
    data.frame(
      depth = i,
      branch = path[i],
      dx_m = value[1],
      dy_m = value[2],
      stringsAsFactors = FALSE
    )
  })
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(
    depth = integer(), branch = character(), dx_m = numeric(), dy_m = numeric()
  )
  out$cumulative_dx_m <- cumsum(out$dx_m)
  out$cumulative_dy_m <- cumsum(out$dy_m)
  out
}

#' @rdname d_set_active_path
#' @export
d_effective_offsets <- function(hierarchy,
                                       path = hierarchy$active_path,
                                       base_offsets = NULL) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  path <- hierarchy_path_vector(path)
  state <- d_active_state(hierarchy, path, missing = NULL)
  if (is.null(state)) {
    stop("No child state is stored for the requested hierarchy path.", call. = FALSE)
  }
  branch <- d_branch_offsets(hierarchy, path)
  out <- effective_offsets(state, base_offsets = base_offsets)
  inherited_dx <- if (nrow(branch)) sum(branch$dx_m) else 0
  inherited_dy <- if (nrow(branch)) sum(branch$dy_m) else 0
  out$inherited_dx_m <- out$inherited_dx_m + inherited_dx
  out$inherited_dy_m <- out$inherited_dy_m + inherited_dy
  out$effective_dx_m <- out$effective_dx_m + inherited_dx
  out$effective_dy_m <- out$effective_dy_m + inherited_dy
  out
}

#' @rdname d_set_active_path
#' @export
d_move_branch <- function(hierarchy,
                                 path = hierarchy$active_path,
                                 dx_m = 0,
                                 dy_m = 0) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  path <- hierarchy_path_vector(path)
  if (!length(path)) stop("`path` must identify a branch to move.", call. = FALSE)
  if (length(path) == 1L) {
    hierarchy$root <- update_region_offset(
      hierarchy$root, hierarchy_branch_id(path[1]), dx_m, dy_m, mode = "increment"
    )
  } else {
    parent_path <- utils::head(path, -1L)
    key <- hierarchy_child_key(hierarchy, parent_path)
    if (is.null(key)) stop("No parent state is stored for the requested branch.",
                           call. = FALSE)
    hierarchy$children[[key]] <- update_region_offset(
      hierarchy$children[[key]], hierarchy_branch_id(utils::tail(path, 1L)),
      dx_m, dy_m, mode = "increment"
    )
  }
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' Snapshot or restore hierarchy state
#'
#' @param hierarchy A [d_hierarchy_state()].
#' @param snapshot Plain hierarchy list previously returned by
#'   `snapshot_dragmapr_hierarchy_state()`.
#' @return A plain list or restored hierarchy state.
#' @export
snapshot_dragmapr_hierarchy_state <- function(hierarchy) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  list(
    root = snapshot_dragmapr_state(hierarchy$root),
    children = lapply(hierarchy$children, snapshot_dragmapr_state),
    active_path = hierarchy$active_path,
    relationships = as.data.frame(hierarchy$relationships),
    version = hierarchy$version
  )
}

#' @rdname snapshot_dragmapr_hierarchy_state
#' @export
restore_dragmapr_hierarchy_state <- function(snapshot) {
  if (!is.list(snapshot)) stop("`snapshot` must be a list.", call. = FALSE)
  children <- snapshot$children %||% list()
  children <- lapply(children, restore_dragmapr_state)
  d_hierarchy_state(
    root = restore_dragmapr_state(snapshot$root),
    children = children,
    active_path = snapshot$active_path %||% character(),
    relationships = restore_hierarchy_relationships(snapshot$relationships),
    version = snapshot$version %||% 0L
  )
}

#' Manage child states in a dragmapr hierarchy
#'
#' @param hierarchy A `dragmapr_hierarchy_state`.
#' @param key Single child key.
#' @param state A child [d_state()].
#' @param missing Value returned when a child key is absent.
#'
#' @return A hierarchy state for setters/removers, or a `dragmapr_state` for
#'   `d_child_state()`.
#' @export
d_set_child_state <- function(hierarchy, key, state) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] <- validate_dragmapr_state(state)
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' @rdname d_set_child_state
#' @export
d_child_state <- function(hierarchy, key, missing = NULL) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] %||% missing
}

#' @rdname d_set_child_state
#' @export
d_remove_child_state <- function(hierarchy, key) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] <- NULL
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' @rdname d_hierarchy_state
#' @param hierarchy A `dragmapr_hierarchy_state`.
#' @export
validate_dragmapr_hierarchy_state <- function(hierarchy) {
  if (!inherits(hierarchy, "dragmapr_hierarchy_state")) {
    stop("`hierarchy` must be created by d_hierarchy_state().", call. = FALSE)
  }
  d_hierarchy_state(
    root = hierarchy$root,
    children = hierarchy$children,
    active_path = hierarchy$active_path,
    relationships = hierarchy$relationships,
    version = hierarchy$version
  )
}

normalize_dragmapr_relationships <- function(relationships) {
  if (is.null(relationships) ||
      (is.list(relationships) && !is.data.frame(relationships) && length(relationships) == 0L)) {
    return(d_relationships())
  }
  if (inherits(relationships, "dragmapr_relationships") || is.data.frame(relationships)) {
    return(d_relationships(as.data.frame(relationships)))
  }
  if (is.list(relationships)) {
    tables <- lapply(relationships, function(x) {
      if (!is.data.frame(x)) {
        stop(
          "Legacy `relationships` entries must be normalized data frames. ",
          "Use d_relationships() to create the relationship table.",
          call. = FALSE
        )
      }
      d_relationships(x)
    })
    if (!length(tables)) return(d_relationships())
    return(d_relationships(do.call(rbind, tables)))
  }
  stop("`relationships` must be created by d_relationships().", call. = FALSE)
}

restore_hierarchy_relationships <- function(x) {
  if (is.null(x) || length(x) == 0L) return(d_relationships())
  if (is.list(x) && !is.data.frame(x) &&
      all(vapply(x, is.list, logical(1)))) {
    x <- do.call(rbind, lapply(x, function(row) {
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
  }
  if (!is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(x)) return(d_relationships())
  d_relationships(x)
}

hierarchy_relationship_table <- function(x) {
  if (inherits(x, "dragmapr_hierarchy_state")) x <- x$relationships
  normalize_dragmapr_relationships(x)
}

hierarchy_scalar <- function(x, arg) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be one non-empty string.", call. = FALSE)
  }
  x
}

hierarchy_column <- function(data, x, arg) {
  x <- hierarchy_scalar(x, arg)
  if (!x %in% names(data)) {
    stop("`", arg, "` column '", x, "' was not found.", call. = FALSE)
  }
  x
}

hierarchy_path_vector <- function(path) {
  path <- as.character(path %||% character())
  path <- path[!is.na(path) & nzchar(path)]
  unname(path)
}

hierarchy_path_key <- function(path) paste(hierarchy_path_vector(path), collapse = "/")

hierarchy_child_key <- function(hierarchy, path) {
  path <- hierarchy_path_vector(path)
  candidates <- unique(c(hierarchy_path_key(path), utils::tail(path, 1L)))
  hit <- candidates[candidates %in% names(hierarchy$children)]
  if (length(hit)) hit[[1L]] else NULL
}

hierarchy_branch_id <- function(x) sub("^[^:]+:", "", as.character(x))

hierarchy_offset_for <- function(state, branch) {
  if (is.null(state)) return(c(0, 0))
  state <- validate_dragmapr_state(state)
  candidates <- unique(c(as.character(branch), hierarchy_branch_id(branch)))
  idx <- match(candidates, as.character(state$region_offsets$region))
  idx <- idx[!is.na(idx)][1L]
  if (is.na(idx) || !length(idx)) return(c(0, 0))
  c(state$region_offsets$dx_m[idx], state$region_offsets$dy_m[idx])
}
