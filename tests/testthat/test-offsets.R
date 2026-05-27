test_that("read_offsets normalizes CSV columns", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("Region,DX_M,DY_M", " North ,10.5,-2"), path)

  offsets <- read_offsets(path)

  expect_equal(offsets$region, "North")
  expect_equal(offsets$dx_m, 10.5)
  expect_equal(offsets$dy_m, -2)
})

test_that("read_offsets rejects invalid offset data", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("region,dx_m", "North,10"), path)
  expect_error(read_offsets(path), "missing column")

  path <- tempfile(fileext = ".csv")
  writeLines(c("region,dx_m,dy_m", "North,10,0", "North,20,0"), path)
  expect_error(read_offsets(path), "duplicate region")

  path <- tempfile(fileext = ".csv")
  writeLines(c("region,dx_m,dy_m", "North,nope,0"), path)
  expect_error(read_offsets(path), "non-numeric")
})

test_that("apply_offsets translates matching sf groups", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(10, 10)), crs = 3857)
  )
  offsets <- data.frame(region = "North", dx_m = 5, dy_m = -2)

  out <- apply_offsets(x, offsets, region_col = "region")

  expect_equal(unclass(sf::st_geometry(out)[[1]]), c(5, -2))
  expect_equal(unclass(sf::st_geometry(out)[[2]]), c(10, 10))
})

test_that("apply_offsets translates polygon groups", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(10, 10), c(14, 10), c(14, 14), c(10, 14), c(10, 10)))),
      crs = 3857
    )
  )
  offsets <- data.frame(region = "North", dx_m = 5, dy_m = -2)

  out <- apply_offsets(x, offsets, region_col = "region")

  expect_equal(sf::st_coordinates(out[1, ])[1, c("X", "Y")], c(X = 5, Y = -2))
  expect_equal(sf::st_coordinates(out[2, ])[1, c("X", "Y")], c(X = 10, Y = 10))
})

test_that("apply_offsets validates offset data frames", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  )

  expect_error(
    apply_offsets(x, data.frame(region = "North", dx_m = NA, dy_m = 0), "region"),
    "non-numeric or missing"
  )
})
