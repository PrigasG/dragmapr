test_that("safe state mutation updates and resets region offsets", {
  state <- d_state(version = 2)

  state <- update_region_offset(state, "A", dx_m = 10, dy_m = -2)
  expect_equal(state$version, 3L)
  expect_equal(state$region_offsets$dx_m[state$region_offsets$region == "A"], 10)

  state <- update_region_offset(state, "A", dx_m = 5, mode = "increment")
  expect_equal(state$region_offsets$dx_m[state$region_offsets$region == "A"], 15)
  expect_equal(state$region_offsets$dy_m[state$region_offsets$region == "A"], -2)

  state <- reset_region(state, "A")
  expect_equal(nrow(state$region_offsets), 0L)
  expect_gt(state$version, 3L)
})

test_that("safe state mutation updates and resets label offsets", {
  state <- d_state(version = 1)

  state <- update_label_offset(state, "A-label", region = "A", dx_m = 3, dy_m = 4)
  expect_equal(state$label_offsets$region, "A")
  expect_equal(state$label_offsets$dx_m, 3)

  state <- update_label_offset(state, "A-label", dx_m = 2, dy_m = -1, mode = "increment")
  expect_equal(state$label_offsets$dx_m, 5)
  expect_equal(state$label_offsets$dy_m, 3)

  state <- reset_label(state, "A-label")
  expect_equal(nrow(state$label_offsets), 0L)
})

test_that("state diff reports added and removed keys", {
  before <- d_state(
    region_offsets = data.frame(region = c("A", "B"), dx_m = c(0, 1), dy_m = 0),
    label_offsets = data.frame(label_id = "la", region = "A", dx_m = 0, dy_m = 0)
  )
  after <- d_state(
    region_offsets = data.frame(region = c("A", "C"), dx_m = c(0, 2), dy_m = 0),
    label_offsets = data.frame(label_id = "lc", region = "C", dx_m = 1, dy_m = 0)
  )

  diff <- d_state_diff(after, before)

  expect_equal(diff$added_regions, "C")
  expect_equal(diff$removed_regions, "B")
  expect_equal(diff$added_labels, "lc")
  expect_equal(diff$removed_labels, "la")
  expect_equal(diff$summary$added_regions, 1L)
})

test_that("compatibility validation checks ids and CRS", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 3857
    )
  )
  ok_state <- d_state(
    region_offsets = data.frame(region = "A", dx_m = 1, dy_m = 0),
    crs = 3857,
    geometry_id = "fixture"
  )

  ok <- validate_state_compatibility(x, ok_state, region_col = "region",
                                     geometry_id = "fixture", strict = FALSE)
  expect_s3_class(ok, "dragmapr_compatibility")
  expect_true(ok$valid)
  expect_length(ok$warnings, 1L)

  bad_state <- update_region_offset(ok_state, "Z", dx_m = 1, dy_m = 1)
  bad <- validate_state_compatibility(x, bad_state, region_col = "region",
                                      geometry_id = "other")
  expect_false(bad$valid)
  expect_true(any(grepl("not present", bad$errors)))
  expect_true(any(grepl("geometry_id", bad$errors)))
})

test_that("region and label connector helpers return tables and sf lines", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
      sf::st_polygon(list(rbind(c(4, 0), c(6, 0), c(6, 2), c(4, 2), c(4, 0)))),
      crs = 3857
    )
  )
  state <- d_state(
    region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0),
    label_offsets = data.frame(label_id = "A", region = "A", dx_m = 5, dy_m = 0),
    crs = 3857
  )
  labels <- as_drag_labels(data.frame(
    label_id = "A", region = "A", label = "A",
    x = 1, y = 1, connector = TRUE
  ))

  region_rows <- region_connectors(x, state, region_col = "region")
  label_rows <- label_connectors(labels, state)
  region_sf <- region_connectors(x, state, region_col = "region", as_sf = TRUE)
  label_sf <- label_connectors(labels, state, as_sf = TRUE, crs = 3857)

  expect_equal(region_rows$region, "A")
  expect_equal(label_rows$label_id, "A")
  expect_s3_class(region_sf, "sf")
  expect_s3_class(label_sf, "sf")
  expect_true(all(as.character(sf::st_geometry_type(region_sf)) == "LINESTRING"))
})
