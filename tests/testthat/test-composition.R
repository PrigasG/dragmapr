test_that("offset composition keeps base inherited and manual movement separate", {
  base <- data.frame(
    feature_id = c("A", "B"), base_dx_m = c(10, 20), base_dy_m = c(1, 2)
  )
  inherited <- data.frame(region = c("A", "B"), dx_m = 100, dy_m = -10)
  state <- d_state(
    region_col = "id",
    region_offsets = data.frame(region = "B", dx_m = 3, dy_m = 4)
  )

  out <- compose_offsets(base, state, inherited)

  expect_named(out, c(
    "region", "base_dx_m", "base_dy_m", "inherited_dx_m",
    "inherited_dy_m", "manual_dx_m", "manual_dy_m",
    "effective_dx_m", "effective_dy_m"
  ))
  expect_equal(out$effective_dx_m, c(110, 123))
  expect_equal(out$effective_dy_m, c(-9, -4))
  expect_identical(effective_offsets(state, base, inherited), out)
})

test_that("apply_dragmapr_state materializes the full composed movement", {
  x <- sf::st_sf(
    id = c("A", "B"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(10, 0)), crs = 3857)
  )
  state <- d_state(
    region_col = "id",
    region_offsets = data.frame(region = "B", dx_m = 3, dy_m = 4)
  )
  base <- data.frame(region = c("A", "B"), base_dx_m = c(1, 2), base_dy_m = 0)

  out <- apply_dragmapr_state(x, state, base_offsets = base)
  xy <- sf::st_coordinates(out)

  expect_equal(xy[, 1], c(1, 15))
  expect_equal(xy[, 2], c(0, 4))
})
