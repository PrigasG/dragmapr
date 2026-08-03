test_that("dragmapr_state validates and round-trips JSON", {
  state <- dragmapr_state(
    level = "division",
    region_offsets = data.frame(region = "North", dx_m = 10, dy_m = -5),
    label_offsets = data.frame(label_id = "lbl", region = "North", dx_m = 1, dy_m = 2),
    expanded_groups = c("A", "A", "B"),
    view = list(scale = 2),
    version = 12
  )

  expect_s3_class(state, "dragmapr_state")
  expect_equal(state$expanded_groups, c("A", "B"))
  expect_equal(snapshot_dragmapr_state(state)$version, 12L)
  expect_equal(state$region_col, "division")
  expect_equal(state$label_id_col, "label_id")

  path <- tempfile(fileext = ".json")
  write_dragmapr_state(state, path)
  restored <- read_dragmapr_state(path)

  expect_s3_class(restored, "dragmapr_state")
  expect_equal(restored$level, "division")
  expect_equal(restored$region_col, "division")
  expect_equal(restored$region_offsets$dx_m, 10)
  expect_equal(restored$label_offsets$label_id, "lbl")
})

test_that("dragmapr_state records crs, geometry_id and selected_feature", {
  state <- dragmapr_state(
    region_offsets = data.frame(region = "North", dx_m = 10, dy_m = -5),
    crs = 3857,
    geometry_id = "hhs-2026",
    selected_feature = "North"
  )

  expect_equal(state$crs, 3857L)
  expect_equal(state$geometry_id, "hhs-2026")
  expect_equal(state$selected_feature, "North")

  path <- tempfile(fileext = ".json")
  write_dragmapr_state(state, path)
  restored <- read_dragmapr_state(path)

  expect_equal(restored$crs, 3857L)
  expect_equal(restored$geometry_id, "hhs-2026")
  expect_equal(restored$selected_feature, "North")
  expect_equal(names(restored$label_offsets), c("label_id", "region", "dx_m", "dy_m"))
  expect_equal(nrow(restored$label_offsets), 0L)

  expect_error(dragmapr_state(geometry_id = c("a", "b")), "geometry_id")
  expect_error(dragmapr_state(crs = "not-a-crs"))
})

test_that("dragmapr_state_diff reports composition changes with tolerance", {
  canonical <- dragmapr_state(
    region_offsets = data.frame(region = c("A", "B"), dx_m = c(0, 10), dy_m = c(0, 0)),
    label_offsets = data.frame(label_id = "l1", region = "A", dx_m = 0, dy_m = 0)
  )
  draft <- dragmapr_state(
    region_offsets = data.frame(region = c("A", "B"), dx_m = c(0.5, 14), dy_m = c(0, 0)),
    label_offsets = data.frame(label_id = "l1", region = "A", dx_m = 3, dy_m = 0)
  )

  diff <- dragmapr_state_diff(draft, canonical, tolerance = 1)

  expect_s3_class(diff, "dragmapr_state_diff")
  expect_true(diff$changed)
  expect_equal(diff$changed_regions, "B")
  expect_equal(diff$changed_labels, "l1")
  expect_true(dragmapr_state_equal(canonical, canonical))
  expect_false(dragmapr_state_equal(draft, canonical, tolerance = 1))
})

test_that("dragmapr_state_diff separates composition from interaction", {
  canonical <- dragmapr_state(selected_feature = "A", view = list(scale = 1))
  draft <- dragmapr_state(selected_feature = "B", view = list(scale = 2))

  expect_true(dragmapr_state_equal(draft, canonical, compare = "composition"))
  expect_false(dragmapr_state_equal(draft, canonical, compare = "interaction"))
  expect_false(dragmapr_state_equal(draft, canonical, compare = "all"))
})

test_that("summary.dragmapr_state reports moved regions and labels", {
  state <- dragmapr_state(
    region_offsets = data.frame(region = c("A", "B"), dx_m = c(0, 5), dy_m = c(0, 0)),
    label_offsets = data.frame(label_id = "l1", region = "A", dx_m = 1, dy_m = 0),
    geometry_id = "geom",
    selected_feature = "A",
    crs = 3857,
    version = 3
  )

  s <- summary(state)
  expect_s3_class(s, "summary.dragmapr_state")
  expect_equal(s$regions_moved, 1L)
  expect_equal(s$labels_moved, 1L)
  expect_equal(s$geometry_id, "geom")
})

test_that("older snapshots without new fields still restore", {
  snapshot <- list(
    level = "region",
    region_offsets = data.frame(region = "North", dx_m = 1, dy_m = 2),
    version = 3L
  )
  restored <- restore_dragmapr_state(snapshot)

  expect_s3_class(restored, "dragmapr_state")
  expect_null(restored$crs)
  expect_null(restored$geometry_id)
  expect_null(restored$selected_feature)
  expect_equal(restored$region_col, "region")
  expect_equal(restored$schema_version, "1.1.0")
})

test_that("state binding separates level labels from join columns", {
  state <- dragmapr_state(
    level = "ADM1",
    region_col = "unit_id",
    region_offsets = data.frame(region = "01", dx_m = 5, dy_m = 0)
  )
  x <- sf::st_sf(
    unit_id = "01",
    ADM1 = "Alabama",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  )

  out <- apply_dragmapr_state(x, state)
  expect_equal(unclass(sf::st_geometry(out)[[1]]), c(5, 0))
  expect_equal(snapshot_dragmapr_state(state)$binding$region_col, "unit_id")
})

test_that("merge carries crs and geometry_id; level change drops selection", {
  base <- dragmapr_state(
    region_offsets = data.frame(region = "A", dx_m = 1, dy_m = 0),
    crs = 3857,
    geometry_id = "hhs-2026",
    selected_feature = "A"
  )
  update <- dragmapr_state(
    region_offsets = data.frame(region = "A", dx_m = 9, dy_m = 0)
  )
  merged <- merge_dragmapr_state(base, update)
  expect_equal(merged$crs, 3857L)
  expect_equal(merged$geometry_id, "hhs-2026")
  expect_equal(merged$selected_feature, "A")

  relation <- data.frame(region_level = "A", child_level = c("A1", "A2"))
  child <- inherit_drag_offsets(base, from = "region_level", to = "child_level",
                                relation = relation)
  expect_equal(child$crs, 3857L)
  expect_equal(child$geometry_id, "hhs-2026")
  expect_null(child$selected_feature)
})

test_that("merge_dragmapr_state replaces rows by key", {
  base <- dragmapr_state(
    region_offsets = data.frame(region = c("A", "B"), dx_m = c(1, 2), dy_m = c(0, 0)),
    version = 1
  )
  update <- dragmapr_state(
    region_offsets = data.frame(region = "B", dx_m = 5, dy_m = 6),
    version = 2
  )

  merged <- merge_dragmapr_state(base, update)

  # Revision bumps past the higher input so the merge sorts after both.
  expect_equal(merged$version, 3L)
  expect_equal(merged$region_offsets$dx_m[merged$region_offsets$region == "A"], 1)
  expect_equal(merged$region_offsets$dx_m[merged$region_offsets$region == "B"], 5)
})

test_that("revision bumps monotonically across transforms", {
  expect_equal(bump_revision(0L), 1L)
  expect_equal(bump_revision(4L, 9L), 10L)
  expect_equal(bump_revision(), 1L)

  base <- dragmapr_state(
    region_offsets = data.frame(region = "A", dx_m = 1, dy_m = 0),
    version = 7
  )
  relation <- data.frame(parent = "A", child = c("A1", "A2"))
  child <- inherit_drag_offsets(base, from = "parent", to = "child", relation = relation)
  expect_equal(child$version, 8L)
  back <- collapse_drag_offsets(child, from = "child", to = "parent", relation = relation)
  expect_equal(back$version, 9L)
})

test_that("apply_dragmapr_state applies region offsets", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  )
  state <- dragmapr_state(
    region_offsets = data.frame(region = "North", dx_m = 5, dy_m = -2)
  )

  out <- apply_dragmapr_state(x, state, region_col = "region")

  expect_equal(unclass(sf::st_geometry(out)[[1]]), c(5, -2))
})

test_that("drag offsets inherit and collapse across hierarchy levels", {
  state <- dragmapr_state(
    level = "division",
    region_offsets = data.frame(region = c("East", "West"), dx_m = c(10, -5), dy_m = c(1, 2)),
    version = 4
  )
  relation <- data.frame(
    division = c("East", "East", "West"),
    state = c("A", "B", "C")
  )

  child <- inherit_drag_offsets(state, from = "division", to = "state", relation = relation)

  expect_equal(child$level, "state")
  expect_equal(child$version, 5L)
  expect_equal(child$region_offsets$dx_m[child$region_offsets$region == "A"], 10)
  expect_equal(child$region_offsets$dx_m[child$region_offsets$region == "C"], -5)

  child$region_offsets$dx_m[child$region_offsets$region == "A"] <- 2
  child$region_offsets$dx_m[child$region_offsets$region == "B"] <- 6
  collapsed <- collapse_drag_offsets(child, from = "state", to = "division", relation = relation)

  expect_equal(collapsed$level, "division")
  expect_equal(collapsed$region_offsets$dx_m[collapsed$region_offsets$region == "East"], 4)
})
