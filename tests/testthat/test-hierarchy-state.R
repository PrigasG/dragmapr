test_that("hierarchy state stores and removes child states", {
  root <- dragmapr_state(level = "division", region_col = "division_id")
  child <- dragmapr_state(
    level = "state",
    region_col = "state_id",
    region_offsets = data.frame(region = "AL", dx_m = 10, dy_m = 0)
  )
  hierarchy <- dragmapr_hierarchy_state(root = root, active_path = "division")

  hierarchy <- dragmapr_set_child_state(hierarchy, "division:1", child)
  expect_s3_class(hierarchy, "dragmapr_hierarchy_state")
  expect_equal(hierarchy$version, 1L)
  expect_equal(dragmapr_child_state(hierarchy, "division:1")$region_col, "state_id")

  hierarchy <- dragmapr_remove_child_state(hierarchy, "division:1")
  expect_equal(hierarchy$version, 2L)
  expect_null(dragmapr_child_state(hierarchy, "division:1"))
})

