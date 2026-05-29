test_that("prepare_dragmapr_sf transforms longlat polygon data", {
  x <- sf::st_sf(
    region = "A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(-1, 0), c(1, 0), c(1, 1), c(-1, 1), c(-1, 0)))),
      crs = 4326
    )
  )

  out <- prepare_dragmapr_sf(x)

  expect_s3_class(out, "sf")
  expect_false(sf::st_is_longlat(out))
})

test_that("prepare_dragmapr_sf drops non-polygon features", {
  x <- sf::st_sf(
    region = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_point(c(2, 2)),
      crs = 3857
    )
  )

  out <- suppressMessages(prepare_dragmapr_sf(x))

  expect_equal(nrow(out), 1)
  expect_equal(out$region, "A")
})

test_that("read_dragmapr_sf_upload returns NULL for no upload", {
  expect_null(read_dragmapr_sf_upload(NULL))
  expect_null(read_dragmapr_sf_upload(data.frame()))
})

test_that("read_dragmapr_sf_upload validates Shiny upload shape", {
  upload <- data.frame(name = "regions.geojson")

  expect_error(
    read_dragmapr_sf_upload(upload),
    "Shiny fileInput"
  )
})

test_that("spatial file detection errors guide users", {
  dir <- tempfile("dragmapr_empty_upload_")
  dir.create(dir)
  writeLines("not spatial", file.path(dir, "notes.txt"))

  expect_error(
    dragmapr:::.detect_spatial_file(dir),
    "No supported spatial file found"
  )
})

test_that("invalid spatial files produce actionable read errors", {
  file <- tempfile(fileext = ".geojson")
  writeLines("not geojson", file)

  expect_error(
    dragmapr:::.read_spatial_file(file, source = "test file"),
    "Could not read the test file as spatial data"
  )
})

test_that("dragmapr_iframe_bridge names configured Shiny inputs", {
  js <- dragmapr_iframe_bridge(region_input = "regions", label_input = "labels")

  expect_match(js, '"regions"', fixed = TRUE)
  expect_match(js, '"labels"', fixed = TRUE)
  expect_match(js, "dragmapr-request-state", fixed = TRUE)
  expect_match(js, "_dragmaprOriginAllowed", fixed = TRUE)
})

test_that("upload file names are sanitized", {
  expect_equal(
    dragmapr:::sanitize_upload_names(c("../regions.geojson", "C:\\fake\\map.shp")),
    c("regions.geojson", "map.shp")
  )
  expect_error(dragmapr:::sanitize_upload_names(".."), "non-empty base names")
})

test_that("zip archives reject unsafe paths", {
  skip_if_not(nzchar(Sys.which("zip")), "zip command not available")
  parent <- tempfile("dragmapr_zip_parent_")
  child <- file.path(parent, "child")
  dir.create(child, recursive = TRUE)
  writeLines("not spatial", file.path(parent, "outside.geojson"))
  old <- setwd(child)
  on.exit(setwd(old), add = TRUE)
  system2("zip", c("-q", "unsafe.zip", "../outside.geojson"))

  expect_error(
    dragmapr:::.unzip_spatial_archive("unsafe.zip", tempfile("dragmapr_unzip_"), "test zip"),
    "unsafe paths"
  )
})

test_that("spatial studio falls back from stale demo columns", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  expect_equal(env$choose_column("hhs_region", c("county", "name")), "county")
  expect_equal(env$choose_column("name", c("county", "name")), "name")
  expect_equal(env$choose_column(NULL, c("county", "name"), fallback = "name"), "name")
})

test_that("spatial studio creates valid empty label offsets", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  labels <- data.frame(
    label_id = character(),
    region = character(),
    label = character(),
    stringsAsFactors = FALSE
  )

  out <- env$empty_label_offsets(labels)

  expect_equal(nrow(out), 0)
  expect_named(out, c("label_id", "region", "dx_m", "dy_m"))
})

test_that("spatial studio uses natural stable display factors", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  out <- env$factor_for_display(c("B", "A", "B", "10", "2", "1", "101", "100"))

  expect_true(is.ordered(out))
  expect_equal(levels(out), c("1", "2", "10", "100", "101", "A", "B"))
  expect_equal(env$stable_unique(c("Region 10", "Region 2", "Region 1")), c("Region 1", "Region 2", "Region 10"))
})

test_that("spatial studio keys label color inputs by label id", {
  env <- new.env(parent = globalenv())
  suppressWarnings(
    source(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"), local = env)
  )

  expect_equal(env$label_color_input_id("Region 10"), "label_color_Region_10")
  expect_equal(env$label_color_input_id("1/2"), "label_color_1_2")
})
