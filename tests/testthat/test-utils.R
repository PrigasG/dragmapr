test_that("%||% returns fallback only for NULL", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(FALSE %||% "fallback", FALSE)
})

test_that("validate_dragmapr_sf flags non-sf and malformed geometry columns", {
  expect_error(validate_dragmapr_sf(data.frame(a = 1)), "must be an sf object")

  x <- sf::st_sf(
    region = "A",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  )
  expect_silent(validate_dragmapr_sf(x))

  wrong_type <- structure(
    data.frame(region = "A", geometry = 1),
    class = c("sf", "data.frame"),
    sf_column = "geometry"
  )
  expect_error(validate_dragmapr_sf(wrong_type), "must contain an sfc vector")

  # Rename the geometry column out from under the active sf_column attribute.
  broken <- x
  names(broken)[names(broken) == "geometry"] <- "geom"
  expect_error(validate_dragmapr_sf(broken), "geometry column")
})

test_that("public sf entry points share the malformed-sf message", {
  broken <- sf::st_sf(
    region = "A",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  )
  names(broken)[names(broken) == "geometry"] <- "geom"

  expect_error(
    apply_offsets(broken, data.frame(region = "A", dx_m = 1, dy_m = 1),
                  region_col = "region"),
    "geometry column"
  )
})
