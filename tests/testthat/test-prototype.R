test_that("drag_map_prototype writes configurable label options", {
  x <- sf::st_sf(
    region = "North",
    name = "North label",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(
    x,
    region_col = "region",
    label_col = "name",
    draggable_labels = FALSE,
    label_marker = FALSE,
    label_marker_shape = "circle",
    label_radius = 16,
    label_text_size = 14,
    label_width = 80,
    label_height = 34,
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"draggableLabels":false', fixed = TRUE)
  expect_match(html, '"labelMarker":false', fixed = TRUE)
  expect_match(html, '"labelMarkerShape":"none"', fixed = TRUE)
  expect_match(html, '"labelRadius":16', fixed = TRUE)
  expect_match(html, '"labelTextSize":14', fixed = TRUE)
  expect_match(html, '"labelWidth":80', fixed = TRUE)
  expect_match(html, '"labelHeight":34', fixed = TRUE)
  expect_match(html, 'class", "drag-hit"', fixed = TRUE)
  expect_match(html, "North label", fixed = TRUE)
  expect_false(grepl("cdn.jsdelivr.net", html, fixed = TRUE))
  expect_match(html, "https://d3js.org v7", fixed = TRUE)
})

test_that("drag_map_prototype escapes script-sensitive label text", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "</script><script>alert(1)</script>",
    x = 2,
    y = 2
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = labels, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_false(grepl("</script><script>alert(1)</script>", html, fixed = TRUE))
  expect_match(html, "\\u003c\\/script\\u003e", fixed = TRUE)
})

test_that("drag_map_prototype writes legend and marker shape options", {
  x <- sf::st_sf(
    region = c("North", "South"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      sf::st_polygon(list(rbind(c(5, 0), c(9, 0), c(9, 4), c(5, 4), c(5, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(
    x,
    region_col = "region",
    label_marker_shape = "circle",
    show_legend = TRUE,
    legend_position = "right",
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"labelMarkerShape":"circle"', fixed = TRUE)
  expect_match(html, '"showLegend":true', fixed = TRUE)
  expect_match(html, '"legendPosition":"right"', fixed = TRUE)
  expect_match(html, "function syncLegend", fixed = TRUE)
})

test_that("drag_map_prototype can hide the built-in side panel", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", side_panel = FALSE, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"sidePanel":false', fixed = TRUE)
  expect_match(html, '<html lang="en" class="no-side-panel">', fixed = TRUE)
  expect_false(grepl("__HTML_CLASS__", html, fixed = TRUE))
  expect_match(html, "html.no-side-panel aside", fixed = TRUE)
})

test_that("drag_map_prototype identifies active regions while dragging", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = FALSE, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, 'id="identity-badge"', fixed = TRUE)
  expect_match(html, "setActiveRegion(region, \"Dragging\")", fixed = TRUE)
  expect_match(html, ".region.is-active path", fixed = TRUE)
})

test_that("drag_map_prototype writes named region palette", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(
    x,
    region_col = "region",
    region_palette = c(North = "#123456"),
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"regionPalette":{"North":"#123456"}', fixed = TRUE)
})

test_that("drag_map_prototype writes initial region and label offsets", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  labels <- data.frame(
    label_id = "north-label",
    region = "North",
    label = "North",
    x = 2,
    y = 2
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(
    x,
    region_col = "region",
    labels = labels,
    region_offsets = data.frame(region = "North", dx_m = 10, dy_m = -5),
    label_offsets = data.frame(label_id = "north-label", region = "North", dx_m = 2, dy_m = 3),
    file = file
  )

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, 'const initialRegionOffsets = [{"region":"North","dx_m":10,"dy_m":-5}]', fixed = TRUE)
  expect_match(html, 'const initialLabelOffsets = [{"label_id":"north-label","region":"North","dx_m":2,"dy_m":3}]', fixed = TRUE)
})

test_that("drag_map_prototype can omit labels", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = FALSE, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"labels":false', fixed = TRUE)
  expect_match(html, "let labelRows = [];", fixed = TRUE)
})

test_that("drag_map_prototype accepts annotation boxes", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  boxes <- as_drag_annotations(data.frame(
    label_id = "note-1",
    region = "North",
    label = "A longer annotation",
    x = 2,
    y = 2
  ), connector = TRUE, connector_type = "squiggle")
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = boxes, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"label_type":"box"', fixed = TRUE)
  expect_match(html, '"connector":true', fixed = TRUE)
  expect_match(html, '"connector_type":"squiggle"', fixed = TRUE)
  expect_match(html, '"labelBoxWidth":150', fixed = TRUE)
  # connectorLayer.raise() was intentionally removed in 0.1.0 — it inverted the
  # stacking order so connector lines rendered above label text.  Correct order
  # (regions → connectors → labels) is achieved by appending in that sequence.
  expect_false(grepl("connectorLayer.raise()", html, fixed = TRUE))
  expect_match(html, "root/regions", fixed = TRUE)
  expect_match(html, "Math.abs(dx) <= halfWidth", fixed = TRUE)
})

test_that("drag_map_prototype accepts user-supplied labels", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  custom_labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "Custom note",
    x = 2,
    y = 2,
    tooltip = "Preserved metadata"
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = custom_labels, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Custom note", fixed = TRUE)
  expect_match(html, "Preserved metadata", fixed = TRUE)
})

test_that("drag_map_prototype writes label colors", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  custom_labels <- data.frame(
    label_id = "note-1",
    region = "North",
    label = "Custom note",
    x = 2,
    y = 2,
    label_color = "#1D4ED8"
  )
  file <- tempfile(fileext = ".html")

  drag_map_prototype(x, region_col = "region", labels = custom_labels, file = file)

  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, '"label_color":"#1D4ED8"', fixed = TRUE)
  expect_match(html, "function labelColor", fixed = TRUE)
})
