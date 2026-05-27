# Shiny app showing static export after draggable offsets are available.
#
# Run with:
# shiny::runApp(system.file("examples", "shiny_static_export.R", package = "dragmapr"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

library(dragmapr)
library(shiny)

hhs <- example_hhs_layout()

ui <- fluidPage(
  tags$h2("Static export from draggable offsets"),
  tags$p("This example uses bundled offsets. Replace them with CSVs downloaded from the draggable helper."),
  plotOutput("plot", height = 560),
  downloadButton("download_png", "Download PNG")
)

server <- function(input, output, session) {
  current_plot <- reactive({
    render_dragged_map(
      hhs$states,
      region_offsets = hhs$region_offsets,
      region_col = "hhs_region",
      labels = hhs$labels,
      label_offsets = hhs$label_offsets,
      region_palette = hhs$region_colors,
      region_labels = hhs$region_names,
      title = "US Map by HHS Regions"
    )
  })

  output$plot <- renderPlot({
    current_plot()
  })

  output$download_png <- downloadHandler(
    filename = function() "hhs_dragged_static.png",
    content = function(file) {
      ggplot2::ggsave(file, current_plot(), width = 9, height = 6, dpi = 300)
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)
