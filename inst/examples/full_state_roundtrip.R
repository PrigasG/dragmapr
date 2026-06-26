# Demonstrates the dragmapr state contract end to end:
#
#   explodemap::as_dragmapr_state()   ->   edit in the browser
#        (compute)                          (compose)
#                                              |
#   write_dragmapr_state() / re-render   <-   dragmapr_widget_state()
#        (persist)                            (ingest)
#
# The same `dragmapr_state` object travels the whole loop: an initial layout is
# produced upstream, the user drags and selects in the native widget, every edit
# is rebuilt server-side into a `dragmapr_state`, and that state is persisted to
# JSON and re-rendered as a static map -- without ever recomputing the geometry.
#
# Run with:
#   shiny::runApp(system.file("examples/full_state_roundtrip.R", package = "dragmapr"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

library(dragmapr)
library(shiny)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# Geometry must be in a projected CRS.
make_square <- function(x0, y0, size = 100000) {
  sf::st_polygon(list(rbind(
    c(x0, y0),
    c(x0 + size, y0),
    c(x0 + size, y0 + size),
    c(x0, y0 + size),
    c(x0, y0)
  )))
}

regions <- sf::st_sf(
  region = c("North", "South", "East", "West"),
  geometry = sf::st_sfc(
    make_square(0, 140000),
    make_square(0, 0),
    make_square(140000, 70000),
    make_square(-140000, 70000),
    crs = 3857
  )
)

# In a full pipeline the starting state comes from explodemap, which computes a
# mathematically valid exploded layout and hands it over as a dragmapr_state:
#
#   layout <- explodemap::explode_grouped(sf_obj, region_col = "region")
#   state0 <- explodemap::as_dragmapr_state(layout)
#
# as_dragmapr_state() converts the exploded anchors into dx_m/dy_m deltas and
# records the projected CRS plus a geometry_id. We build the equivalent shape
# directly here so the example runs with only dragmapr installed.
state0 <- dragmapr_state(
  region_offsets = data.frame(
    region = c("North", "South", "East", "West"),
    dx_m   = c(0, 0, 60000, -60000),
    dy_m   = c(50000, -50000, 0, 0)
  ),
  crs = 3857,
  geometry_id = "synthetic-quad-v1"
)

SAVE_PATH <- file.path(tempdir(), "dragmapr_composition.json")

ui <- fluidPage(
  tags$h2("dragmapr state round-trip"),
  tags$p(
    "Drag a region, or click one to select it. Every edit flows back to R, ",
    "is rebuilt into a dragmapr_state, and drives the saved JSON and the ",
    "static re-render on the right."
  ),
  fluidRow(
    column(
      width = 8,
      dragmaprOutput("map", height = "560px")
    ),
    column(
      width = 4,
      tags$h4("Live state (from the browser)"),
      verbatimTextOutput("state_summary"),
      actionButton("reselect", "Select 'East' from R"),
      actionButton("save", "Save composition to JSON"),
      verbatimTextOutput("saved_path"),
      tags$h4("Static re-render"),
      plotOutput("static", height = "260px")
    )
  )
)

server <- function(input, output, session) {
  # The current composition, as a dragmapr_state. Seeded from the upstream
  # layout and replaced by each browser edit.
  composition <- reactiveVal(state0)

  output$map <- renderDragmapr({
    dragmapr_widget(regions, region_col = "region", state = isolate(composition()))
  })

  # Inbound bridge: rebuild a dragmapr_state from each browser state event
  # (the `<id>_state` input the widget emits on drag / click / selection).
  observeEvent(input$map_state, {
    edit <- dragmapr_widget_state(input$map_state)
    if (!is.null(edit)) {
      composition(edit)
    }
  })

  output$state_summary <- renderText({
    s <- composition()
    moved <- s$region_offsets[s$region_offsets$dx_m != 0 | s$region_offsets$dy_m != 0, , drop = FALSE]
    paste0(
      "revision:     ", s$version, "\n",
      "crs:          ", format(s$crs), "\n",
      "geometry_id:  ", s$geometry_id %||% "(none)", "\n",
      "selected:     ", s$selected_feature %||% "(none)", "\n",
      "moved:        ", if (nrow(moved)) paste(moved$region, collapse = ", ") else "(none)"
    )
  })

  # Outbound bridge: push a selection from R into the browser. The widget echoes
  # the change back through input$map_state, so `composition` stays in sync.
  observeEvent(input$reselect, {
    updateDragmapr(session, "map", selected_feature = "East")
  })

  # Persist: the state is plain, reproducible data.
  observeEvent(input$save, {
    write_dragmapr_state(composition(), SAVE_PATH)
  })
  output$saved_path <- renderText({
    input$save
    if (file.exists(SAVE_PATH)) paste("Saved to:", SAVE_PATH) else "(not saved yet)"
  })

  # Re-render the composed state as a static map from geometry + offsets, with
  # no recomputation of the layout.
  output$static <- renderPlot({
    s <- composition()
    render_dragged_map(
      regions,
      region_offsets = s$region_offsets,
      region_col = "region",
      title = "Composed layout"
    )
  })
}

shinyApp(ui, server)
