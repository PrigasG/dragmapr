test_that("d_validate_state returns TRUE for valid states", {
  expect_true(d_validate_state(d_state()))
  expect_true(d_validate_state(d_state(
    region_offsets = data.frame(region = "A", dx_m = 1, dy_m = 2)
  )))
  expect_error(d_validate_state(list()), "must be created by d_state")
})

test_that("d_nudge increments region offsets", {
  s <- d_state(
    region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0)
  )
  v0 <- s$version
  s1 <- d_nudge(s, "A", dx_m = 5, dy_m = -2)

  expect_equal(s1$region_offsets$dx_m, 15)
  expect_equal(s1$region_offsets$dy_m, -2)
  expect_gt(s1$version, v0)
  # The input state is unchanged.
  expect_equal(s$region_offsets$dx_m, 10)
})

test_that("d_nudge increments label offsets", {
  s <- d_state(
    label_offsets = data.frame(label_id = "l1", region = "A", dx_m = 1, dy_m = 2)
  )
  s1 <- d_nudge(s, "l1", dx_m = 1, target = "label")

  expect_equal(s1$label_offsets$dx_m, 2)
  expect_equal(s1$label_offsets$dy_m, 2)
})

test_that("d_nudge validates its inputs", {
  s <- d_state()
  expect_error(d_nudge(s, "A", dx_m = "x"), "single finite number")
  expect_error(d_nudge(s, "A", dy_m = Inf), "single finite number")
  expect_error(d_nudge(s, NA, dx_m = 1), "single non-empty string")
  expect_error(d_nudge(s, "A", target = "bogus"), "should be one of")
})

test_that("d_nudge on an unknown id adds a row like update_region_offset()", {
  s <- d_state()
  s1 <- d_nudge(s, "Z", dx_m = 4)
  expect_equal(s1$region_offsets$dx_m[s1$region_offsets$region == "Z"], 4)
})

test_that("d_snap_offsets rounds region and label offsets to the grid", {
  s <- d_state(
    region_offsets = data.frame(region = "A", dx_m = 1234, dy_m = -567),
    label_offsets = data.frame(label_id = "l1", region = "A", dx_m = 149, dy_m = 251)
  )
  v0 <- s$version
  s1 <- d_snap_offsets(s, grid = 1000)

  expect_equal(s1$region_offsets$dx_m, 1000)
  expect_equal(s1$region_offsets$dy_m, -1000)
  expect_equal(s1$label_offsets$dx_m, 0)
  expect_equal(s1$label_offsets$dy_m, 0)
  expect_gt(s1$version, v0)
  expect_true(d_validate_state(s1))
})

test_that("d_snap_offsets validates the grid", {
  s <- d_state()
  expect_error(d_snap_offsets(s, grid = 0), "single positive number")
  expect_error(d_snap_offsets(s, grid = -5), "single positive number")
  expect_error(d_snap_offsets(s, grid = c(1, 2)), "single positive number")
  expect_error(d_snap_offsets(s, grid = NA_real_), "single positive number")
})

test_that("d_history supports push, undo, and redo", {
  h <- d_history()
  s0 <- d_state(region_offsets = data.frame(region = "A", dx_m = 0, dy_m = 0))
  s1 <- d_nudge(s0, "A", dx_m = 50)

  d_history_push(h, s0)
  d_history_push(h, s1)

  back <- d_undo(h)
  expect_equal(back$region_offsets$dx_m, 0)
  expect_true(d_state_equal(back, s0))

  fwd <- d_redo(h)
  expect_equal(fwd$region_offsets$dx_m, 50)
  expect_true(d_state_equal(fwd, s1))

  expect_error(d_redo(h), "Nothing to redo")
})

test_that("d_history undo needs two checkpoints and push clears redo", {
  h <- d_history()
  expect_error(d_undo(h), "Nothing to undo")

  s0 <- d_state()
  d_history_push(h, s0)
  expect_error(d_undo(h), "Nothing to undo")

  s1 <- d_nudge(s0, "A", dx_m = 5)
  d_history_push(h, s1)
  d_undo(h)

  s2 <- d_nudge(s0, "A", dx_m = 10)
  d_history_push(h, s2)
  expect_error(d_redo(h), "Nothing to redo")

  back <- d_undo(h)
  expect_equal(back$region_offsets$dx_m, 0)
})

test_that("d_history validates its inputs", {
  h <- d_history()
  expect_error(d_history_push(h, list()), "must be created by d_state")
  expect_error(d_history_push(list(), d_state()), "must be created by d_history")
  expect_error(d_undo(list()), "must be created by d_history")
  expect_error(d_redo("nope"), "must be created by d_history")
})

test_that("print.dragmapr_history summarizes the stack", {
  h <- d_history()
  d_history_push(h, d_state())
  d_history_push(h, d_nudge(d_state(), "A", dx_m = 1))
  out <- capture.output(print(h))
  expect_true(any(grepl("Checkpoints: 2", out, fixed = TRUE)))
})
