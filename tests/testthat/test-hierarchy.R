rect <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
}

hierarchy_sf <- function() {
  sf::st_sf(
    county = c("A", "A", "B", "B"),
    town = c("One", "Two", "One", "Three"),
    geometry = sf::st_sfc(
      rect(0, 1, 0, 1), rect(1, 2, 0, 1),
      rect(3, 4, 0, 1), rect(4, 5, 0, 1),
      crs = 3857
    )
  )
}

test_that("detect_hierarchy_columns ranks parent-child pairs", {
  pairs <- detect_hierarchy_columns(hierarchy_sf())

  expect_true(nrow(pairs) >= 1L)
  expect_equal(pairs$parent[[1]], "county")
  expect_equal(pairs$child[[1]], "town")
  expect_true(pairs$recommended[[1]])
  expect_true(pairs$child_repeats_across_parents[[1]])
})

test_that("recommend_dragmapr_hierarchy returns a plain recommendation", {
  rec <- recommend_dragmapr_hierarchy(hierarchy_sf())

  expect_equal(rec$parent, "county")
  expect_equal(rec$child, "town")
  expect_gt(rec$confidence, 0)
  expect_true(is.data.frame(rec$pairs))
})

test_that("validate_bloom_hierarchy returns keys and helpful failures", {
  ok <- validate_bloom_hierarchy(hierarchy_sf(), "county", "town")

  expect_true(ok$valid)
  expect_equal(length(ok$parent_key), nrow(hierarchy_sf()))
  expect_true(any(grepl("county=", ok$child_key, fixed = TRUE)))

  bad <- validate_bloom_hierarchy(hierarchy_sf(), "county", "county")
  expect_false(bad$valid)
  expect_match(bad$message, "different")
})

test_that("build_branch_transition_data prepares helper fields", {
  built <- build_branch_transition_data(
    hierarchy_sf(),
    "county",
    "town",
    animation = "leaf_flip",
    duration_ms = 350,
    leaf_child_scale = 0.86
  )

  expect_s3_class(built$sf, "sf")
  expect_true(built$parent_key_col %in% names(built$sf))
  expect_true(built$child_key_col %in% names(built$sf))
  expect_true(built$shell_col %in% names(built$sf))
  expect_equal(built$transition$child_region_col, built$child_key_col)
  expect_equal(built$transition$shell_col, built$shell_col)
  expect_equal(built$transition$animation, "leaf_flip")
  expect_equal(built$transition$duration_ms, 350)
  expect_equal(built$transition$leaf_child_scale, 0.86)
  expect_gt(nrow(built$sf), nrow(hierarchy_sf()))
})

test_that("make_branch_bloom_labels prepares parent labels by default", {
  x <- hierarchy_sf()
  x$county_label <- paste("County", x$county)
  x$town_label <- paste("Town", x$town)
  built <- build_branch_transition_data(x, "county", "town")

  parent_only <- make_branch_bloom_labels(
    built$sf,
    parent_key_col = built$parent_key_col,
    child_key_col = built$child_key_col,
    shell_col = built$shell_col,
    parent_label_col = "county_label",
    child_label_col = "town_label"
  )
  expect_true(nrow(parent_only) > 0L)
  expect_true(all(parent_only$label_level == "parent"))
  expect_false(any(parent_only$connector))

  both <- make_branch_bloom_labels(
    built$sf,
    parent_key_col = built$parent_key_col,
    child_key_col = built$child_key_col,
    shell_col = built$shell_col,
    parent_label_col = "county_label",
    child_label_col = "town_label",
    show_child_labels = TRUE
  )
  expect_true(any(both$label_level == "child"))
  expect_true(all(c("label_parent", "label_type") %in% names(both)))
})

test_that("summarise_spatial_crs and profile_spatial_upload are useful", {
  x <- hierarchy_sf()
  crs <- summarise_spatial_crs(x)
  profile <- profile_spatial_upload(x)

  expect_equal(crs$id, "EPSG:3857")
  expect_equal(crs$type, "Projected")
  expect_true(is.list(profile$hierarchy))
  expect_true(profile$animation$can_leaf_flip)
  expect_equal(profile$animation$parent_col, "county")
  expect_equal(profile$animation$child_col, "town")
})
