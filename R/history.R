#' Undo/redo history for dragmapr states
#'
#' `d_history()` creates an undo/redo stack for `dragmapr_state` objects. Push
#' a checkpoint with [d_history_push()] after each meaningful edit, then step
#' backwards with [d_undo()] and forwards with [d_redo()]. Pushing a new
#' checkpoint clears the redo stack, matching familiar editor behaviour.
#'
#' The history object has reference semantics: `d_history_push()`, `d_undo()`,
#' and `d_redo()` modify it in place, so reassignment is optional.
#'
#' @param history A `dragmapr_history` object created by [d_history()].
#' @param state A `dragmapr_state` checkpoint to remember.
#'
#' @return `d_history()` returns a `dragmapr_history` object;
#'   `d_history_push()` returns the history invisibly; `d_undo()` and
#'   `d_redo()` return the restored `dragmapr_state`.
#' @export
#' @examples
#' h <- d_history()
#' s0 <- d_state(
#'   region_offsets = data.frame(region = "A", dx_m = 0, dy_m = 0)
#' )
#' d_history_push(h, s0)
#' s1 <- d_nudge(s0, "A", dx_m = 50)
#' d_history_push(h, s1)
#' back <- d_undo(h) # s0 offsets again
#' fwd <- d_redo(h)  # s1 offsets again
#' d_state_equal(back, s0)
#' d_state_equal(fwd, s1)
d_history <- function() {
  history <- new.env(parent = emptyenv())
  history$past <- list()
  history$future <- list()
  class(history) <- "dragmapr_history"
  history
}

validate_dragmapr_history <- function(history) {
  if (!inherits(history, "dragmapr_history") || !is.environment(history)) {
    stop("`history` must be created by d_history().", call. = FALSE)
  }
  history
}

#' @rdname d_history
#' @export
d_history_push <- function(history, state) {
  history <- validate_dragmapr_history(history)
  state <- validate_dragmapr_state(state)
  history$past <- c(history$past, list(state))
  history$future <- list()
  invisible(history)
}

#' @rdname d_history
#' @export
d_undo <- function(history) {
  history <- validate_dragmapr_history(history)
  if (length(history$past) < 2L) {
    stop("Nothing to undo: push at least two states first.", call. = FALSE)
  }
  current <- history$past[[length(history$past)]]
  history$future <- c(list(current), history$future)
  history$past <- history$past[-length(history$past)]
  history$past[[length(history$past)]]
}

#' @rdname d_history
#' @export
d_redo <- function(history) {
  history <- validate_dragmapr_history(history)
  if (!length(history$future)) {
    stop("Nothing to redo.", call. = FALSE)
  }
  state <- history$future[[1L]]
  history$future <- history$future[-1L]
  history$past <- c(history$past, list(state))
  state
}

#' @export
print.dragmapr_history <- function(x, ...) {
  cat("dragmapr history\n")
  cat("Checkpoints: ", length(x$past), "\n", sep = "")
  cat("Redoable: ", length(x$future), "\n", sep = "")
  invisible(x)
}
