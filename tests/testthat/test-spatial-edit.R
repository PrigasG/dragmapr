test_that("spatial_feature_table summarizes editable features", {
  x <- sf::st_sf(
    id = c("A", "B"),
    name = c("Alpha", "Beta"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
      crs = 3857
    )
  )

  tbl <- spatial_feature_table(x, key_col = "id")

  expect_equal(tbl$.feature_id, c("A", "B"))
  expect_equal(tbl$.row, 1:2)
  expect_equal(tbl$.geometry_type, c("POLYGON", "POLYGON"))
  expect_true(all(c(".xmin", ".ymin", ".xmax", ".ymax", "name") %in% names(tbl)))
  expect_false(inherits(tbl, "sf"))

  with_geom <- spatial_feature_table(x, key_col = "id", include_geometry = TRUE)
  expect_true(inherits(with_geom$geometry, "sfc"))

  colliding <- x
  colliding$.feature_id <- c("original-a", "original-b")
  colliding$.row <- c(10L, 20L)
  collision_tbl <- spatial_feature_table(colliding, key_col = "id")
  expect_identical(anyDuplicated(names(collision_tbl)), 0L)
  expect_equal(collision_tbl$.feature_id.1, colliding$.feature_id)
  expect_equal(collision_tbl$.row.1, colliding$.row)
})

test_that("remove and keep spatial features work by key or row id", {
  x <- sf::st_sf(
    id = c("A", "B", "C"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 3857
    )
  )

  out <- remove_spatial_features(x, "B", key_col = "id")
  expect_equal(out$id, c("A", "C"))
  expect_s3_class(out, "sf")

  kept <- keep_spatial_features(x, c("1", "3"))
  expect_equal(kept$id, c("A", "C"))

  expect_equal(nrow(remove_spatial_features(x, character(), key_col = "id")), 3L)
  expect_equal(nrow(keep_spatial_features(x, character(), key_col = "id")), 0L)
  expect_s3_class(keep_spatial_features(x, character(), key_col = "id"), "sf")
})

test_that("add and replace spatial features preserve target schema", {
  x <- sf::st_sf(
    id = c("A", "B"),
    value = c(1L, 2L),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 3857
    )
  )
  feature <- sf::st_sf(
    id = "C",
    extra = "drop me",
    geometry = sf::st_sfc(sf::st_point(c(2, 2)), crs = 3857)
  )

  expect_warning(added <- add_spatial_features(x, feature), "Dropping")
  expect_equal(added$id, c("A", "B", "C"))
  expect_true("value" %in% names(added))
  expect_false("extra" %in% names(added))
  expect_true(is.na(added$value[added$id == "C"]))

  replacement <- sf::st_sf(
    id = "B2",
    value = 20L,
    geometry = sf::st_sfc(sf::st_point(c(4, 4)), crs = 3857)
  )
  replaced <- replace_spatial_features(x, "B", replacement, key_col = "id")
  expect_equal(replaced$id, c("A", "B2"))
  expect_equal(replaced$value[replaced$id == "B2"], 20L)
})

test_that("missing feature attributes retain their vector type", {
  geometry <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 3857
  )
  x <- sf::st_sf(
    id = "A",
    metadata = I(list(list(source = "original"))),
    geometry = geometry
  )
  x$area <- sf::st_area(x)
  feature <- sf::st_sf(
    id = "B",
    geometry = geometry + c(2, 0)
  )

  added <- add_spatial_features(x, feature)

  expect_true(inherits(added$area, "units"))
  expect_true(is.na(added$area[[2L]]))
  expect_true(is.list(added$metadata))
  expect_null(added$metadata[[2L]])
})
