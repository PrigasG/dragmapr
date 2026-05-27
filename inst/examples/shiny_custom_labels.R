# Shiny app showing custom user-supplied labels in a draggable plot.
#
# Run with:
# shiny::runApp(system.file("examples", "shiny_custom_labels.R", package = "dragmapr"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

library(dragmapr)
library(shiny)

panels <- example_panel_layout()
custom_labels <- as_drag_labels(data.frame(
  label_id = c("start-note", "review-note"),
  region = c("A", "C"),
  label = c("Start here", "Drag for review"),
  x = c(55000, 420000),
  y = c(115000, 90000),
  tooltip = c("Custom label metadata", "Useful for app-specific annotation")
))

ui <- fluidPage(
  tags$h2("Custom draggable labels"),
  tags$p("The labels are user-supplied data, not automatically derived region labels."),
  uiOutput("helper")
)

server <- function(input, output, session) {
  static_dir <- tempfile("dragmapr_labels_www")
  dir.create(static_dir, recursive = TRUE)
  shiny::addResourcePath("dragmapr_label_static", static_dir)

  helper_file <- file.path(static_dir, "panel_custom_labels.html")
  drag_map_prototype(
    panels$panels,
    region_col = "group",
    labels = custom_labels,
    label_marker = FALSE,
    label_text_size = 14,
    file = helper_file
  )

  output$helper <- renderUI({
    tags$iframe(
      src = "dragmapr_label_static/panel_custom_labels.html",
      style = "width: 100%; height: 720px; border: 1px solid #d8dee8;"
    )
  })
}

shinyApp(ui, server)
