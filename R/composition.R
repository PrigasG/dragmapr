#' Compose algorithmic, inherited, and manual offsets
#'
#' Establishes the cross-package composition contract: algorithmic base
#' movement (for example from `explodemap`), inherited ancestor movement, and
#' local manual edits remain separately inspectable while their sum becomes the
#' effective movement applied to geometry.
#'
#' @param base Optional base-offset table, an `explodemap_child_layout`, or a
#'   list containing an `offsets` table. Accepted movement columns are
#'   `base_dx_m`/`base_dy_m` or `dx_m`/`dy_m`.
#' @param state A [d_state()] supplying local manual offsets.
#' @param ancestor_offsets Optional inherited-offset table. Accepted movement
#'   columns are `inherited_dx_m`/`inherited_dy_m`, `effective_dx_m`/
#'   `effective_dy_m`, or `dx_m`/`dy_m`.
#' @param key_col Optional identifier column used in external offset tables.
#'   Defaults to `region`, `feature_id`, or `parent_id`, in that order.
#'
#' @return A data frame with `region`, separate base/inherited/manual columns,
#'   and `effective_dx_m`/`effective_dy_m`.
#' @export
compose_offsets <- function(base = NULL,
                            state,
                            ancestor_offsets = NULL,
                            key_col = NULL) {
  state <- validate_dragmapr_state(state)
  base <- composition_table(
    base, key_col = key_col,
    x_candidates = c("base_dx_m", "dx_m"),
    y_candidates = c("base_dy_m", "dy_m"),
    prefix = "base"
  )
  inherited <- composition_table(
    ancestor_offsets, key_col = key_col,
    x_candidates = c("inherited_dx_m", "effective_dx_m", "dx_m"),
    y_candidates = c("inherited_dy_m", "effective_dy_m", "dy_m"),
    prefix = "inherited"
  )
  manual <- state$region_offsets
  names(manual)[names(manual) == "dx_m"] <- "manual_dx_m"
  names(manual)[names(manual) == "dy_m"] <- "manual_dy_m"
  keys <- natural_sort(unique(c(base$region, inherited$region, manual$region)))
  out <- data.frame(region = keys, stringsAsFactors = FALSE)
  out <- merge(out, base, by = "region", all.x = TRUE, sort = FALSE)
  out <- merge(out, inherited, by = "region", all.x = TRUE, sort = FALSE)
  out <- merge(out, manual, by = "region", all.x = TRUE, sort = FALSE)
  movement <- c(
    "base_dx_m", "base_dy_m", "inherited_dx_m", "inherited_dy_m",
    "manual_dx_m", "manual_dy_m"
  )
  for (name in movement) {
    if (!name %in% names(out)) out[[name]] <- 0
    out[[name]][is.na(out[[name]])] <- 0
  }
  out$effective_dx_m <- out$base_dx_m + out$inherited_dx_m + out$manual_dx_m
  out$effective_dy_m <- out$base_dy_m + out$inherited_dy_m + out$manual_dy_m
  out <- out[match(keys, out$region), c("region", movement,
                                        "effective_dx_m", "effective_dy_m"),
             drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @rdname compose_offsets
#' @param base_offsets Alias for `base` in the state-first calling form.
#' @export
effective_offsets <- function(state,
                              base_offsets = NULL,
                              ancestor_offsets = NULL,
                              key_col = NULL) {
  compose_offsets(
    base = base_offsets,
    state = state,
    ancestor_offsets = ancestor_offsets,
    key_col = key_col
  )
}

composition_table <- function(x, key_col, x_candidates, y_candidates, prefix) {
  empty <- data.frame(region = character(), stringsAsFactors = FALSE)
  empty[[paste0(prefix, "_dx_m")]] <- numeric()
  empty[[paste0(prefix, "_dy_m")]] <- numeric()
  if (is.null(x)) return(empty)
  if (is.list(x) && !is.data.frame(x) && is.data.frame(x$offsets)) x <- x$offsets
  if (!is.data.frame(x)) {
    stop("Offset composition inputs must be data frames or layout objects with `$offsets`.",
         call. = FALSE)
  }
  resolved_key <- key_col
  if (is.null(resolved_key)) {
    resolved_key <- c("region", "feature_id", "parent_id")
    resolved_key <- resolved_key[resolved_key %in% names(x)][1L]
  }
  if (length(resolved_key) != 1L || is.na(resolved_key) ||
      !resolved_key %in% names(x)) {
    stop("Could not find an offset identifier column (`region`, `feature_id`, or `parent_id`).",
         call. = FALSE)
  }
  x_col <- x_candidates[x_candidates %in% names(x)][1L]
  y_col <- y_candidates[y_candidates %in% names(x)][1L]
  if (is.na(x_col) || is.na(y_col)) {
    stop("Offset input is missing compatible horizontal/vertical movement columns.",
         call. = FALSE)
  }
  out <- data.frame(
    region = as.character(x[[resolved_key]]),
    dx = suppressWarnings(as.numeric(x[[x_col]])),
    dy = suppressWarnings(as.numeric(x[[y_col]])),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$region) || any(!nzchar(out$region)) || anyDuplicated(out$region) ||
      any(!is.finite(out$dx)) || any(!is.finite(out$dy))) {
    stop("Offset inputs require unique IDs and finite movement values.", call. = FALSE)
  }
  names(out)[2:3] <- paste0(prefix, c("_dx_m", "_dy_m"))
  out
}
