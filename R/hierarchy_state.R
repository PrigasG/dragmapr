#' Create a dragmapr hierarchy state
#'
#' A hierarchy state keeps one parent/root `dragmapr_state` together with any
#' child-level states created while drilling into the map. It is useful for apps
#' that move between levels such as divisions, states, counties, or
#' municipalities without losing each level's edits.
#'
#' @param root A root [dragmapr_state()].
#' @param children Named list of child [dragmapr_state()] objects.
#' @param active_path Character vector describing the active path through the
#'   hierarchy.
#' @param relationships Optional list of parent-child lookup tables.
#' @param version Integer hierarchy revision.
#'
#' @return An object of class `"dragmapr_hierarchy_state"`.
#' @export
dragmapr_hierarchy_state <- function(root = dragmapr_state(),
                                     children = list(),
                                     active_path = character(),
                                     relationships = list(),
                                     version = 0L) {
  root <- validate_dragmapr_state(root)
  if (!is.list(children) ||
      (length(children) > 0L && (is.null(names(children)) || any(!nzchar(names(children)))))) {
    stop("`children` must be a named list.", call. = FALSE)
  }
  children <- lapply(children, validate_dragmapr_state)
  if (!is.list(relationships)) {
    stop("`relationships` must be a list.", call. = FALSE)
  }
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

#' Manage child states in a dragmapr hierarchy
#'
#' @param hierarchy A `dragmapr_hierarchy_state`.
#' @param key Single child key.
#' @param state A child [dragmapr_state()].
#' @param missing Value returned when a child key is absent.
#'
#' @return A hierarchy state for setters/removers, or a `dragmapr_state` for
#'   `dragmapr_child_state()`.
#' @export
dragmapr_set_child_state <- function(hierarchy, key, state) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] <- validate_dragmapr_state(state)
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' @rdname dragmapr_set_child_state
#' @export
dragmapr_child_state <- function(hierarchy, key, missing = NULL) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] %||% missing
}

#' @rdname dragmapr_set_child_state
#' @export
dragmapr_remove_child_state <- function(hierarchy, key) {
  hierarchy <- validate_dragmapr_hierarchy_state(hierarchy)
  key <- state_key_scalar(key, "key")
  hierarchy$children[[key]] <- NULL
  hierarchy$version <- bump_revision(hierarchy$version)
  validate_dragmapr_hierarchy_state(hierarchy)
}

#' @rdname dragmapr_hierarchy_state
#' @param hierarchy A `dragmapr_hierarchy_state`.
#' @export
validate_dragmapr_hierarchy_state <- function(hierarchy) {
  if (!inherits(hierarchy, "dragmapr_hierarchy_state")) {
    stop("`hierarchy` must be created by dragmapr_hierarchy_state().", call. = FALSE)
  }
  dragmapr_hierarchy_state(
    root = hierarchy$root,
    children = hierarchy$children,
    active_path = hierarchy$active_path,
    relationships = hierarchy$relationships,
    version = hierarchy$version
  )
}
