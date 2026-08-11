test_that("dragmapr option constructors validate boundaries", {
  display <- d_display_options(
    region_palette = c(North = "#123456"),
    show_origin_outlines = TRUE,
    map_background = "dark"
  )
  interaction <- d_interaction_options(draggable_regions = FALSE)
  geometry <- d_geometry_options(width = 1000, height = 800)

  expect_equal(display$regionPalette$North, "#123456")
  expect_true(display$showOriginOutlines)
  expect_equal(display$mapBackground, "dark")
  expect_false(interaction$draggableRegions)
  expect_equal(geometry$width, 1000)
  expect_error(d_display_options(region_palette = c("#123456")), "named vector")
  expect_error(d_interaction_options(draggable_regions = NA), "TRUE or FALSE")
})

test_that("interaction options default to full state on drag end", {
  options <- d_interaction_options()
  throttled <- d_interaction_options(
    state_emit = "throttled", live_drag = FALSE, throttle_ms = 120
  )

  expect_equal(options$stateEmit, "end")
  expect_true(options$liveDrag)
  expect_equal(throttled$stateEmit, "throttled")
  expect_false(throttled$liveDrag)
  expect_equal(throttled$throttleMs, 120)
  expect_error(d_interaction_options(throttle_ms = -1), "non-negative")
})

test_that("widget preserves hierarchy state and emits compact live drags", {
  js <- paste(readLines(
    system.file("htmlwidgets", "dragmapr.js", package = "dragmapr"), warn = FALSE
  ), collapse = "\n")

  expect_match(js, "expanded_groups: state.expandedGroups.slice()", fixed = TRUE)
  expect_match(js, "stateEmit: \"end\"", fixed = TRUE)
  expect_match(js, "emitLiveDrag", fixed = TRUE)
  expect_false(grepl('sendInput("drag", snapshot("drag"', js, fixed = TRUE))
})

test_that("dragmapr_widget builds row-oriented payload", {
  x <- sf::st_sf(
    region = "North",
    name = "North label",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  state <- d_state(
    level = "division",
    region_offsets = data.frame(region = "North", dx_m = 10, dy_m = -5),
    expanded_groups = "North",
    styles = d_styles(data.frame(region = "North", fill = "#112233")),
    version = 3
  )

  widget <- d_widget(x, region_col = "region", label_col = "name", state = state)

  expect_s3_class(widget, "htmlwidget")
  expect_equal(widget$x$state$level, "division")
  expect_equal(widget$x$revision, 3L)
  expect_equal(widget$x$state$region_offsets[[1]]$region, "North")
  expect_equal(widget$x$state$expanded_groups, "North")
  expect_equal(widget$x$state$styles[[1]]$fill, "#112233")
  expect_equal(widget$x$labels[[1]]$label, "North label")
  expect_equal(widget$x$geojson$type, "FeatureCollection")
})

test_that("dragmapr_widget accepts direct region_palette argument", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )

  widget <- d_widget(
    x,
    region_col = "region",
    labels = FALSE,
    region_palette = c(North = "#123456")
  )

  expect_equal(widget$x$display$regionPalette$North, "#123456")
  expect_error(
    d_widget(x, region_col = "region", labels = FALSE, region_palette = c("#123456")),
    "named vector"
  )
})

test_that("widget payload hoists crs and selected_feature", {
  x <- sf::st_sf(
    region = "North",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = 3857
    )
  )
  state <- d_state(
    region_offsets = data.frame(region = "North", dx_m = 1, dy_m = 0),
    crs = 3857,
    geometry_id = "hhs-2026",
    selected_feature = "North"
  )

  widget <- d_widget(x, region_col = "region", labels = FALSE, state = state)

  expect_equal(widget$x$crs, 3857L)
  expect_equal(widget$x$selectedFeature, "North")
  expect_equal(widget$x$state$geometry_id, "hhs-2026")

  # No selection -> empty string sentinel that the browser reads as "none".
  bare <- d_widget(x, region_col = "region", labels = FALSE)
  expect_equal(bare$x$selectedFeature, "")
})

test_that("updateDragmapr routes selected_feature as a composition update", {
  sent <- NULL
  session <- list(
    ns = function(id) paste0("ns-", id),
    sendCustomMessage = function(type, message) {
      sent <<- list(type = type, message = message)
    }
  )

  out <- updateDragmapr(session, "map", selected_feature = "North", show_drag_trail = TRUE)
  expect_equal(out$selectedFeature, "North")
  expect_true(out$display$showDragTrail)
  expect_equal(sent$message$selectedFeature, "North")

  cleared <- updateDragmapr(session, "map", selected_feature = NULL)
  expect_equal(cleared$selectedFeature, "")

  removed <- updateDragmapr(session, "map", remove_features = c("North", "South"))
  expect_equal(removed$removeFeatures, c("North", "South"))

  deleted <- updateDragmapr(session, "map", delete_selected = TRUE)
  expect_true(deleted$deleteSelected)

  targeted <- updateDragmapr(session, "map", show_drag_trail = FALSE,
                             generation = 42, revision = 11)
  expect_equal(targeted$generation, 42)
  expect_equal(targeted$serverRevision, 11)
  expect_equal(sent$message$generation, 42)
  expect_equal(sent$message$serverRevision, 11)

  expect_error(updateDragmapr(session, "map", delete_selected = NA), "TRUE or FALSE")
  expect_error(updateDragmapr(session, "map", generation = NA), "generation")
})

test_that("dragmapr_edit accepts sf and layout-shaped inputs", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
    crs = 3857
  )
  x <- sf::st_sf(region = "North", geometry = poly)

  # Raw sf needs region_col.
  expect_error(d_edit(x), "region_col")
  w1 <- d_edit(x, region_col = "region", labels = FALSE)
  expect_s3_class(w1, "htmlwidget")

  # Duck-typed dragmapr_layout (as explodemap::as_dragmapr() returns).
  layout <- structure(
    list(sf = x, region_col = "region", region_offsets = NULL, label_offsets = NULL),
    class = c("dragmapr_layout", "list")
  )
  w2 <- d_edit(layout, labels = FALSE)
  expect_s3_class(w2, "htmlwidget")
  expect_equal(w2$x$state$level, "region")

  # Duck-typed grouped_exploded_map shape.
  grouped <- list(sf_grouped = x, diagnostics = list(region_col = "region"))
  w3 <- d_edit(grouped, labels = FALSE, state = d_state(
    region_offsets = data.frame(region = "North", dx_m = 5, dy_m = 0)
  ))
  expect_s3_class(w3, "htmlwidget")

  expect_error(d_edit(42), "must be a projected sf")
})

test_that("dragmapr_widget_state ingests a browser state event", {
  # Shape mirrors the payload the widget sends to input[[id_state]]: rows arrive
  # as a list of per-row lists, revision is the client's counter, and "" means
  # no selection.
  value <- list(
    event = "region_click",
    schema_version = "1.0.0",
    package_version = "0.2.0",
    level = "division",
    revision = 17,
    crs = 3857,
    geometry_id = "hhs-2026",
    selected_feature = "North",
    region_offsets = list(
      list(region = "North", dx_m = 10, dy_m = -5),
      list(region = "South", dx_m = 0, dy_m = 0)
    ),
    label_offsets = list(),
    expanded_groups = list(),
    view = list(scale = 2)
  )

  state <- d_widget_state(value)

  expect_s3_class(state, "dragmapr_state")
  expect_equal(state$level, "division")
  expect_equal(state$version, 17L)
  expect_equal(state$crs, 3857L)
  expect_equal(state$geometry_id, "hhs-2026")
  expect_equal(state$schema_version, "1.0.0")
  expect_equal(state$package_version, "0.2.0")
  expect_equal(state$selected_feature, "North")
  expect_equal(state$region_offsets$dx_m[state$region_offsets$region == "North"], 10)
  expect_equal(nrow(state$label_offsets), 0L)

  expect_null(d_widget_state(NULL))

  value$selected_feature <- ""
  expect_null(d_widget_state(value)$selected_feature)
})

test_that("updateDragmapr sends display-only message", {
  sent <- NULL
  session <- list(
    ns = function(id) paste0("ns-", id),
    sendCustomMessage = function(type, message) {
      sent <<- list(type = type, message = message)
    }
  )

  out <- updateDragmapr(
    session,
    "map",
    show_origin_outlines = TRUE,
    map_background = "white"
  )

  expect_equal(sent$type, "dragmapr-update")
  expect_equal(sent$message$id, "ns-map")
  expect_true(sent$message$display$showOriginOutlines)
  expect_equal(out$display$mapBackground, "white")
  expect_error(updateDragmapr(session, "map", region_col = "region"), "Unsupported")
})

test_that("updateDragmapr sends hierarchy and style composition", {
  sent <- NULL
  session <- list(
    ns = identity,
    sendCustomMessage = function(type, message) sent <<- message
  )

  updateDragmapr(
    session, "map",
    expanded_groups = c("A", "B"),
    styles = d_styles(data.frame(region = "A", fill = "#112233"))
  )

  expect_equal(sent$expandedGroups, c("A", "B"))
  expect_equal(sent$styles[[1]]$region, "A")
  expect_equal(sent$styles[[1]]$fill, "#112233")
})
