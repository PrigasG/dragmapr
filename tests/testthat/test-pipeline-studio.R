test_that("Pipeline Studio deferred work is reactive-safe and single-flight", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("later")
  skip_if_not_installed("bslib")
  skip_if_not_installed("explodemap", minimum_version = "0.4.0")

  app_env <- new.env(parent = globalenv())
  app_file <- system.file("shiny", "pipeline-studio", "app.R", package = "dragmapr")
  if (!nzchar(app_file)) {
    app_file <- test_path("..", "..", "inst", "shiny", "pipeline-studio", "app.R")
  }
  suppressWarnings(source(app_file, local = app_env))
  # This test exercises Pipeline Studio's scheduler, not the optional
  # explodemap-to-state bridge. Keep it independent of whichever released
  # explodemap version happens to be installed in the check library.
  app_env$as_dragmapr_state <- function(...) d_state()

  small <- data.frame(region = rep(letters[1:5], each = 10))
  large <- data.frame(region = rep("A", app_env$OPTIMIZE_MAX_FEATURES + 1L))
  many_groups <- data.frame(
    region = seq_len(app_env$OPTIMIZE_MAX_GROUPS + 1L)
  )
  expect_null(app_env$optimization_limit_message(small))
  expect_match(app_env$optimization_limit_message(large), "1,001 features", fixed = TRUE)
  expect_match(app_env$optimization_limit_message(many_groups), "51 groups", fixed = TRUE)

  shiny::testServer(app_env$server, {
    completed <- new.env(parent = emptyenv())
    completed$sources <- character()

    first <- run_process(
      "Reactive-context regression check",
      function() completed$sources <- c(completed$sources, rv$source),
      delay = 0
    )
    duplicate <- run_process(
      "Duplicate work that must not be queued",
      function() completed$sources <- c(completed$sources, "duplicate"),
      delay = 0
    )

    expect_true(first)
    expect_false(duplicate)
    later::run_now(timeoutSecs = 1)

    expect_identical(completed$sources, isolate(rv$source))
    expect_identical(isolate(process_depth()), 0L)
  })
})
