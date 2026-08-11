# dragmapr <img src="man/figures/logo.png" alt="dragmapr logo" align="right" height="150"/>

[![R-CMD-check](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrigasG/dragmapr/actions/workflows/R-CMD-check.yaml)
[![Spatial Studio](https://img.shields.io/badge/Hugging%20Face-Spatial%20Studio-ffcc4d)](https://Prigas89-dragmapr-spatial-studio.hf.space)
[![Connect Cloud](https://img.shields.io/badge/Posit%20Connect-Spatial%20Studio-447099)](https://prigas89-dragmapr.share.connect.posit.cloud)
[![Pipeline Studio](https://img.shields.io/badge/Hugging%20Face-Pipeline%20Studio-2b7fff)](https://huggingface.co/spaces/Prigas89/spatial-pipeline-studio)

`dragmapr` is a small editing layer for maps that need a human hand. It lets
you drag regions, labels, and callouts in the browser, then save those edits as
plain R data so the same layout can be rebuilt later.

It is useful when automatic placement gets close but not quite close enough:
service areas that need breathing room, labels that need manual nudging, small
geographies that should be removed from a presentation map, or an
`explodemap` layout that needs final editorial polish.

## Install

```r
install.packages("dragmapr")

# Development version
# install.packages("pak")
# pak::pak("PrigasG/dragmapr")
```

## A First Draggable Map

This example builds four projected polygons, opens the editor, and saves the
browser helper as a standalone HTML file.

```r
library(dragmapr)

make_square <- function(x0, y0, size = 100000) {
  sf::st_polygon(list(rbind(
    c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size),
    c(x0, y0 + size), c(x0, y0)
  )))
}

regions <- sf::st_sf(
  region = c("North", "South", "East", "West"),
  label = c("North", "South", "East", "West"),
  geometry = sf::st_sfc(
    make_square(0, 140000),
    make_square(0, 0),
    make_square(140000, 70000),
    make_square(-140000, 70000),
    crs = 3857
  )
)

drag_map_prototype(
  regions,
  region_col = "region",
  label_col = "label",
  show_origin_outlines = TRUE,
  file = "drag-map.html",
  open = interactive()
)
```

After dragging, the helper can download region and label offset CSVs. Those
offsets are enough to recreate a static map from the original geometry:

```r
region_offsets <- data.frame(
  region = c("North", "South", "East", "West"),
  dx_m = c(0, 0, 60000, -60000),
  dy_m = c(50000, -50000, 0, 0)
)

label_offsets <- data.frame(
  label_id = c("North", "South", "East", "West"),
  region = c("North", "South", "East", "West"),
  dx_m = c(0, 0, 25000, -25000),
  dy_m = c(30000, -30000, 0, 0)
)

render_dragged_map(
  regions,
  region_col = "region",
  label_col = "label",
  region_offsets = region_offsets,
  label_offsets = label_offsets,
  title = "Edited layout",
  file = "edited-map.png"
)
```

## Work With State

For apps and repeatable pipelines, keep the edits in a `dragmapr_state`. The
state stores region offsets, label offsets, the selected feature, the viewport,
and a version number while leaving the source geometry untouched.

```r
state <- d_state(
  level = "state",
  region_col = "geoid",
  region_offsets = region_offsets,
  label_offsets = label_offsets,
  crs = 3857,
  geometry_id = "toy-regions-v1",
  selected_feature = "East"
)

validate_dragmapr_state(state)
write_dragmapr_state(state, "composition.json")

state <- read_dragmapr_state("composition.json")

render_dragged_map(
  regions,
  region_col = "region",
  state = state,
  file = "composition.png"
)
```

`level` names the active geography. `region_col` is the source column used to
join offsets back to geometry, so saved state can use stable IDs without tying
the display label to the data key.

The same object is the handoff point for `explodemap`:

```r
library(explodemap)
library(dragmapr)

layout <- explode_grouped(my_sf, region_col = "region", plot = FALSE)
state <- as_dragmapr_state(layout)

d_edit(layout, state = state)
```

For drill-down apps, keep each level's edits together:

```r
hierarchy <- d_hierarchy_state(root = state, active_path = "state")
hierarchy <- d_set_child_state(hierarchy, "state:06", county_state)
county_state <- d_child_state(hierarchy, "state:06")
```

## Use It In Shiny

The native widget reports structured state to Shiny inputs. The server can keep
that state, render previews, enable Undo, or push display updates without
rebuilding the geometry.

```r
library(shiny)
library(dragmapr)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("selected", "Selected region", choices = regions$region),
      checkboxInput("origin", "Show origin outlines", TRUE),
      actionButton("remove", "Remove selected geography")
    ),
    mainPanel(
      dragmaprOutput("map", height = "650px"),
      verbatimTextOutput("state_summary")
    )
  )
)

server <- function(input, output, session) {
  current_state <- reactiveVal(d_state(crs = 3857))

  output$map <- renderDragmapr({
    d_widget(
      regions,
      region_col = "region",
      state = current_state(),
      display_options = d_display_options(
        show_origin_outlines = isTRUE(input$origin)
      )
    )
  })

  observeEvent(input$map_state, {
    current_state(d_widget_state(input$map_state))
  })

  observeEvent(input$selected, {
    updateDragmapr(session, "map", selected_feature = input$selected)
  })

  observeEvent(input$origin, {
    updateDragmapr(session, "map", show_origin_outlines = input$origin)
  })

  observeEvent(input$remove, {
    updateDragmapr(session, "map", remove_features = input$selected)
  })

  output$state_summary <- renderPrint(summary(current_state()))
}

shinyApp(ui, server)
```

Useful Shiny inputs emitted by the widget include `input$map_state`,
`input$map_region_click`, `input$map_drag_start`, `input$map_drag_end`,
`input$map_feature_delete`, and `input$map_ready`.

## Remove Or Replace Geography

Presentation maps often carry more geography than the story needs. `dragmapr`
keeps deletion as an explicit edit: inspect the features, remove or keep stable
ids, and still let Undo restore the previous state in an app.

```r
features <- spatial_feature_table(regions, key_col = "region")
features[, c(".feature_id", ".row", ".geometry_type", ".bbox_xmin", ".bbox_ymin")]

without_north <- remove_spatial_features(
  regions,
  ids = "North",
  key_col = "region"
)

south_and_west <- keep_spatial_features(
  regions,
  ids = c("South", "West"),
  key_col = "region"
)
```

New or corrected geography can be appended or swapped in without changing the
rest of the workflow:

```r
new_region <- sf::st_sf(
  region = "Central",
  label = "Central",
  geometry = sf::st_sfc(make_square(280000, 70000), crs = 3857)
)

regions2 <- add_spatial_features(regions, new_region)
regions3 <- replace_spatial_features(
  regions2,
  ids = "East",
  features = new_region,
  key_col = "region"
)
```

Pipeline Studio exposes this as a left-panel editing flow: select a geography,
review its source rows, remove it from the draft source, and Undo if the edit
was wrong.

## Labels, Notes, And Connectors

Labels can be hidden, dragged independently, or replaced with callout boxes.

```r
labels <- make_region_labels(regions, region_col = "region", label_col = "label")

notes <- as_drag_annotations(
  data.frame(
    label_id = "east-note",
    region = "East",
    label = "A longer note can move independently.",
    x = 210000,
    y = 120000
  ),
  width_px = 180,
  height_px = 80,
  connector = TRUE,
  connector_type = "curve"
)

drag_map_prototype(
  regions,
  region_col = "region",
  labels = notes,
  connector_smart = TRUE,
  connector_endpoint = "arrow",
  open = interactive()
)
```

## Project Bundles

Spatial Studio exports a project ZIP containing the source geometry, offsets,
labels, palette, metadata, and a small R script. A bundle can be rendered later
without reopening the browser editor.

```r
render_dragmapr_project(
  "dragmapr-project.zip",
  file = "final-map.png",
  width = 10,
  height = 8,
  dpi = 300
)
```

## Live Apps

- Spatial Studio on Hugging Face: <https://Prigas89-dragmapr-spatial-studio.hf.space>
- Spatial Studio on Posit Connect Cloud: <https://prigas89-dragmapr.share.connect.posit.cloud>
- Pipeline Studio for `explodemap` and `dragmapr`: <https://huggingface.co/spaces/Prigas89/spatial-pipeline-studio>
- Package site: <https://prigasg.github.io/dragmapr/>

Run Pipeline Studio locally:

```r
shiny::runApp(system.file("shiny/pipeline-studio", package = "dragmapr"))
```

## Deployment Notes

The Hugging Face Space uses the repository `Dockerfile`. Posit Connect Cloud
uses the Git-backed wrapper in `connect-cloud/spatial-studio/`; publish
`master` and choose `connect-cloud/spatial-studio/app.R` as the primary file.

Regenerate the Connect manifest after dependency changes:

```r
rsconnect::writeManifest(
  appDir = "connect-cloud/spatial-studio",
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)
```

## What To Expect

`dragmapr` edits display geometry. The original spatial data remains the source
of truth, while offsets and state describe how the map should appear. Use a
projected CRS for editing; `prepare_dragmapr_sf()` can repair common geometry
issues and transform longitude/latitude data before the browser sees it. For
large files, simplify or group features first so dragging stays responsive.

The package supports both paths: quick standalone HTML for one-off layout work,
and native Shiny/htmlwidget state for applications that need Undo, live display
updates, geography removal, and reproducible export. The common thread is that
every edit is visible as data: an offset table, a state JSON file, or an edited
`sf` object you can inspect before you publish the final map.
