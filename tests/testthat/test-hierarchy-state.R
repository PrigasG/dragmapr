test_that("hierarchy state stores and removes child states", {
  root <- d_state(level = "division", region_col = "division_id")
  child <- d_state(
    level = "state",
    region_col = "state_id",
    region_offsets = data.frame(region = "AL", dx_m = 10, dy_m = 0)
  )
  hierarchy <- d_hierarchy_state(root = root, active_path = "division")

  hierarchy <- d_set_child_state(hierarchy, "division:1", child)
  expect_s3_class(hierarchy, "dragmapr_hierarchy_state")
  expect_equal(hierarchy$version, 1L)
  expect_equal(d_child_state(hierarchy, "division:1")$region_col, "state_id")

  hierarchy <- d_remove_child_state(hierarchy, "division:1")
  expect_equal(hierarchy$version, 2L)
  expect_null(d_child_state(hierarchy, "division:1"))
})

test_that("normalized relationships answer generic hierarchy queries", {
  county_edges <- d_relationships(
    data.frame(state = "34", county = c("001", "003")),
    parent_level = "state", parent_id = "state",
    child_level = "county", child_id = "county"
  )
  local_edges <- d_relationships(
    data.frame(county = c("001", "001"), local = c("m1", "m2")),
    parent_level = "county", parent_id = "county",
    child_level = "municipality", child_id = "local"
  )
  relationships <- d_relationships(rbind(county_edges, local_edges))

  expect_equal(children_of(relationships, "34", "state")$child_id, c("001", "003"))
  expect_equal(parent_of(relationships, "m1", "municipality")$parent_id, "001")
  expect_equal(descendants_of(relationships, "34", "state")$id,
               c("001", "003", "m1", "m2"))
  expect_equal(ancestors_of(relationships, "m1", "municipality")$id,
               c("001", "34"))
  expect_equal(hierarchy_path(relationships, "m1", "municipality"),
               c("34", "001", "m1"))
})

test_that("hierarchy operations compose and move complete branches", {
  root <- d_state(
    level = "county", region_col = "county_id",
    region_offsets = data.frame(region = "001", dx_m = 10, dy_m = -2)
  )
  child <- d_state(
    level = "municipality", region_col = "local_id",
    region_offsets = data.frame(region = "m1", dx_m = 2, dy_m = 3)
  )
  hierarchy <- d_hierarchy_state(
    root = root,
    children = list("county:001" = child),
    active_path = "county:001"
  )
  base <- data.frame(feature_id = "m1", base_dx_m = 100, base_dy_m = 5)

  expect_identical(d_active_state(hierarchy), child)
  expect_identical(d_parent_state(hierarchy), root)
  expect_equal(d_effective_offsets(hierarchy, base_offsets = base)$effective_dx_m,
               112)

  moved <- d_move_branch(hierarchy, dx_m = 5, dy_m = 1)
  expect_equal(moved$root$region_offsets$dx_m, 15)
  expect_equal(d_effective_offsets(moved, base_offsets = base)$effective_dx_m,
               117)
  expect_equal(d_branch_offsets(moved)$cumulative_dy_m, -1)

  snapshot <- snapshot_dragmapr_hierarchy_state(moved)
  restored <- restore_dragmapr_hierarchy_state(snapshot)
  expect_equal(restored$version, moved$version)
  expect_equal(d_active_state(restored)$region_offsets,
               d_active_state(moved)$region_offsets)
})
