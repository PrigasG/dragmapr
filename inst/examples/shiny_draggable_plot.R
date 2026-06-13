if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

library(dragmapr)
library(shiny)

hhs <- example_hhs_layout()

ui <- fluidPage(
  tags$h2("Draggable HHS plot"),
  tags$p("Drag regions and labels in the embedded helper, then copy or download the offset CSVs."),
  uiOutput("helper")
)

server <- function(input, output, session) {
  static_dir <- tempfile("dragmapr_www")
  dir.create(static_dir, recursive = TRUE)
  shiny::addResourcePath("dragmapr_static", static_dir)

  helper_file <- file.path(static_dir, "hhs_drag_helper.html")
  drag_map_prototype(
    hhs$states,
    region_col = "hhs_region",
    label_col = "hhs_region",
    file = helper_file
  )

  output$helper <- renderUI({
    tags$iframe(
      src = "dragmapr_static/hhs_drag_helper.html",
      style = "width: 100%; height: 760px; border: 1px solid #d8dee8;"
    )
  })
}

shinyApp(ui, server)
