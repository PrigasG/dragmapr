rect2 <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y1, y1, y0, y0))))
}

fix_sf <- function(region) {
  n <- length(region)
  sf::st_sf(
    region = region,
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) rect2(i, i + 0.9, 0, 1)),
      crs = 3857
    )
  )
}

test_that("schema versions require whole-number major.minor.patch", {
  expect_error(d_state(schema_version = "1.2"), "major.minor.patch")
  expect_error(d_state(schema_version = "1.2.x"), "major.minor.patch")
  expect_error(d_state(schema_version = "1.2.5.1"), "major.minor.patch")
  expect_error(d_state(schema_version = "1.5"), "major.minor.patch")
  expect_error(d_state(schema_version = "01.2.0"), "major.minor.patch")
  expect_true(d_validate_state(d_state(schema_version = "2.0.0")))

  snap <- snapshot_dragmapr_state(d_state())
  expect_error(
    migrate_dragmapr_state(snap, target_schema_version = "2"),
    "major.minor.patch"
  )
})

test_that("build_transition_plan rejects fractional animation orders", {
  movement <- data.frame(
    feature_id = c("a", "b"),
    original_anchor_x = c(0, 0), original_anchor_y = c(0, 0),
    final_anchor_x = c(10, 20), final_anchor_y = c(0, 0),
    animation_order = c(1.5, 2)
  )
  expect_error(build_transition_plan(movement), "positive whole numbers")

  movement$animation_order <- c(1L, 2L)
  plan <- build_transition_plan(movement)
  expect_equal(plan$animation_order, c(1L, 2L))

  movement$animation_order <- c("1", "2")
  plan2 <- build_transition_plan(movement)
  expect_equal(plan2$animation_order, c(1L, 2L))
})

test_that("make_squiggle_data returns a typed empty frame for degenerate input", {
  connectors <- data.frame(
    connector_id = c("a", "b"),
    x = c(0, 1), y = c(0, 1),
    xend = c(0, 1), yend = c(0, 1),
    stringsAsFactors = FALSE
  )
  out <- make_squiggle_data(connectors, amplitude = 10, waves = 2)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("connector_id", "x", "y"))
  expect_equal(nrow(out), 0L)
  expect_true(is.character(out$connector_id))
  expect_true(is.numeric(out$x))
})

test_that("make_group_boundaries ignores missing group ids", {
  x <- fix_sf(c("A", NA, "A", "B"))
  bounds <- make_group_boundaries(x, group_col = "region")

  expect_equal(sort(as.character(bounds$group_id)), c("A", "B"))

  x_na <- fix_sf(c(NA, NA))
  expect_error(
    make_group_boundaries(x_na, group_col = "region"),
    "No valid group values"
  )
})

test_that("select_label_ids prefers label_prefer and warns on prefer", {
  labels <- as_drag_labels(data.frame(
    label_id = c("A", "B", "C"), region = c("A", "B", "C"),
    label = c("A", "B", "C"), x = 1:3, y = 1:3
  ))

  expect_equal(
    select_label_ids(labels, max_labels = 2, label_prefer = "C"),
    c("C", "A")
  )
  expect_warning(
    out <- select_label_ids(labels, max_labels = 2, prefer = "C"),
    "deprecated"
  )
  expect_equal(out, c("C", "A"))
})

test_that("make_region_labels drops missing region values", {
  labels <- make_region_labels(fix_sf(c("A", NA, "B")), region_col = "region")
  expect_equal(sort(as.character(labels$label_id)), c("A", "B"))
})

test_that("effective_offsets() agrees with compose_offsets()", {
  s <- d_state(region_offsets = data.frame(region = "A", dx_m = 3, dy_m = 4))
  base <- data.frame(region = "A", dx_m = 1, dy_m = 1)
  expect_equal(
    compose_offsets(base = base, state = s),
    effective_offsets(s, base_offsets = base)
  )
})

test_that("CRS metre detection only matches exact metre/meter spellings", {
  expect_true(crs_units_are_metres("metre"))
  expect_true(crs_units_are_metres("meter"))
  expect_true(crs_units_are_metres("metres"))
  expect_true(crs_units_are_metres("METRE"))
  expect_false(crs_units_are_metres("kilometre"))
  expect_false(crs_units_are_metres("decimetre"))
  expect_false(crs_units_are_metres("m"))
  expect_false(crs_units_are_metres("US survey foot"))
})

test_that("merge_state_rows cannot inject NA keys", {
  base <- data.frame(
    region = c("A", "B"), dx_m = c(1, 2), dy_m = c(0, 0),
    stringsAsFactors = FALSE
  )
  update <- data.frame(
    region = c("B", NA), dx_m = c(5, 9), dy_m = c(0, 0),
    stringsAsFactors = FALSE
  )
  out <- merge_state_rows(base, update, key = "region")

  expect_false(anyNA(out$region))
  expect_equal(sort(out$region), c("A", "B"))
  expect_equal(out$dx_m[out$region == "B"], 5)
})

test_that("incremental updates treat missing offsets as zero", {
  s <- d_state()
  s1 <- update_region_offset(s, region = "Z", dx_m = 4, mode = "increment")
  expect_equal(s1$region_offsets$dx_m[s1$region_offsets$region == "Z"], 4)

  s2 <- d_state(region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0))
  s3 <- update_region_offset(s2, region = "A", dx_m = 5, mode = "increment")
  expect_equal(s3$region_offsets$dx_m, 15)
})

test_that("region_connectors() skips missing region values", {
  x <- fix_sf(c("A", NA))
  s <- d_state(region_offsets = data.frame(region = "A", dx_m = 10, dy_m = 0))
  out <- region_connectors(x, s, region_col = "region", include_unmoved = TRUE)

  expect_false(anyNA(out$region))
  expect_equal(out$region, "A")
  expect_equal(out$dx_m, 10)
})

test_that("write/read_dragmapr_state round-trips through a tempfile", {
  s <- d_state(region_offsets = data.frame(region = "A", dx_m = 10, dy_m = -5))
  path <- tempfile(fileext = ".json")
  write_dragmapr_state(s, path)
  restored <- read_dragmapr_state(path)
  expect_true(d_state_equal(s, restored))
})
