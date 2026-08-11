if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

library(dragmapr)
library(shiny)

hhs <- example_hhs_layout()
`%||%` <- function(x, y) if (is.null(x)) y else x

read_csv_text <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }
  utils::read.csv(text = text, stringsAsFactors = FALSE, check.names = FALSE)
}

ui <- fluidPage(
  tags$head(
    # Iframe bridge: relays drag state from the helper iframe to Shiny inputs.
    tags$script(HTML(d_iframe_bridge(
      iframe_selector = "#dragmapr-export-helper"
    ))),
    # Custom message handlers: push presentation changes from Shiny into the iframe.
    tags$script(HTML("
      function getHelperFrame() {
        return document.getElementById('dragmapr-export-helper');
      }
      function helperOrigin() {
        var iframe = getHelperFrame();
        if (!iframe || !iframe.src) return window.location.origin;
        try { return new URL(iframe.src, window.location.href).origin; }
        catch (e) { return window.location.origin; }
      }
      Shiny.addCustomMessageHandler('dragmapr-labels', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow)
          iframe.contentWindow.postMessage({type: 'dragmapr-set-labels', labels: !!message.labels}, helperOrigin());
      });
      Shiny.addCustomMessageHandler('dragmapr-label-data', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow)
          iframe.contentWindow.postMessage({type: 'dragmapr-set-label-data', labels: message.labels || []}, helperOrigin());
      });
      Shiny.addCustomMessageHandler('dragmapr-label-options', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow)
          iframe.contentWindow.postMessage({type: 'dragmapr-set-label-options', options: message.options || {}}, helperOrigin());
      });
    "))
  ),
  tags$h2("Draggable plot with report export"),
  tags$p("Drag regions or labels. Shiny captures the offset state and can export a report-ready PNG."),
  fluidRow(
    column(
      width = 7,
      uiOutput("helper")
    ),
    column(
      width = 5,
      tags$h3("Static preview"),
      checkboxInput("show_labels", "Show labels", value = TRUE),
      radioButtons(
        "annotation_mode",
        "Annotation style",
        choices = c("Short labels" = "labels", "Info boxes" = "boxes"),
        selected = "labels",
        inline = TRUE
      ),
      radioButtons(
        "label_marker_shape",
        "Text label marker",
        choices = c("Circle" = "circle", "Rounded box" = "rect", "Text only" = "none"),
        selected = "circle",
        inline = TRUE
      ),
      sliderInput("label_radius", "Circle label radius", min = 8, max = 48, value = 14, step = 1),
      sliderInput("box_width", "Info box width", min = 110, max = 260, value = 165, step = 5),
      sliderInput("box_height", "Info box height", min = 48, max = 140, value = 68, step = 4),
      checkboxInput("show_connectors", "Show connector lines", value = FALSE),
      radioButtons(
        "connector_type",
        "Connector style",
        choices = c("Straight" = "straight", "Elbow" = "elbow", "Curve" = "curve", "Squiggle" = "squiggle"),
        selected = "straight",
        inline = TRUE
      ),
      sliderInput("connector_linewidth", "Connector thickness", min = 0.25, max = 2.5, value = 0.45, step = 0.05),
      checkboxInput("show_legend", "Show legend", value = TRUE),
      plotOutput("preview", height = 520),
      downloadButton("download_png", "Download PNG"),
      tags$h4("Captured region state"),
      verbatimTextOutput("region_state", placeholder = TRUE),
      tags$h4("Captured label state"),
      verbatimTextOutput("label_state", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session) {
  static_dir <- tempfile("dragmapr_www")
  dir.create(static_dir, recursive = TRUE)
  shiny::addResourcePath("dragmapr_export_static", static_dir)

  helper_file <- file.path(static_dir, "hhs_drag_helper.html")
  hhs_notes <- hhs$labels
  hhs_notes$label <- unname(hhs$region_names[hhs_notes$region])
  hhs_notes$label <- paste(
    "Region", hhs_notes$region,
    "covers the", sub("^[0-9]+ - ", "", hhs_notes$label), "service area."
  )
  hhs_notes <- as_drag_annotations(hhs_notes, width_px = 165, height_px = 68)

  drag_map_prototype(
    hhs$states,
    region_col = "hhs_region",
    labels = hhs$labels,
    label_col = "hhs_region",
      region_palette = hhs$region_colors,
      show_legend = TRUE,
      file = helper_file
    )

  region_csv <- debounce(reactive(input$region_csv), 250)
  label_csv <- debounce(reactive(input$label_csv), 250)
  region_state <- reactive(read_csv_text(region_csv()) %||% hhs$region_offsets)
  label_state <- reactive(read_csv_text(label_csv()) %||% hhs$label_offsets)
  annotation_labels <- reactive({
    labels <- if (identical(input$annotation_mode, "boxes")) {
      hhs_notes
    } else {
      hhs$labels
    }
    if (identical(input$annotation_mode, "boxes")) {
      labels$width_px <- input$box_width %||% 165
      labels$height_px <- input$box_height %||% 68
    }
    labels$connector <- isTRUE(input$show_connectors)
    labels$connector_type <- input$connector_type %||% "straight"
    as_drag_labels(labels)
  })
  current_plot <- reactive({
    render_dragged_map(
      hhs$states,
      region_offsets = region_state(),
      region_col = "hhs_region",
      labels = if (isTRUE(input$show_labels)) annotation_labels() else FALSE,
      label_offsets = label_state(),
      region_palette = hhs$region_colors,
      region_labels = hhs$region_names,
      show_legend = isTRUE(input$show_legend),
      show_label_marker = !identical(input$label_marker_shape %||% "circle", "none"),
      label_marker_shape = input$label_marker_shape %||% "circle",
      marker_size = (input$label_radius %||% 14) / 3,
      connector_linewidth = input$connector_linewidth %||% 0.45,
      label_padding = 0.12,
      title = "US Map by HHS Regions"
    )
  })

  output$helper <- renderUI({
    tags$iframe(
      id    = "dragmapr-export-helper",
      src   = "dragmapr_export_static/hhs_drag_helper.html",
      style = "width: 100%; height: 760px; border: 1px solid #d8dee8;"
    )
  })
  output$preview <- renderPlot(current_plot())
  observeEvent(input$show_labels, {
    session$sendCustomMessage("dragmapr-labels", list(labels = isTRUE(input$show_labels)))
  }, ignoreInit = FALSE)
  observeEvent(annotation_labels(), {
    session$sendCustomMessage(
      "dragmapr-label-data",
      list(labels = jsonlite::fromJSON(
        jsonlite::toJSON(annotation_labels(), dataframe = "rows", auto_unbox = TRUE),
        simplifyVector = FALSE
      ))
    )
  }, ignoreInit = FALSE)
  observe({
    session$sendCustomMessage(
      "dragmapr-label-options",
      list(options = list(
        labelMarker = !identical(input$label_marker_shape %||% "circle", "none"),
        labelMarkerShape = input$label_marker_shape %||% "circle",
        labelRadius = input$label_radius %||% 14,
        labelBoxWidth = input$box_width %||% 165,
        labelBoxHeight = input$box_height %||% 68,
        connectorLinewidth = (input$connector_linewidth %||% 0.45) * 3,
        showLegend = isTRUE(input$show_legend)
      ))
    )
  })
  output$region_state <- renderText(region_csv() %||% "Waiting for draggable plot state...")
  output$label_state <- renderText({
    if (!isTRUE(input$show_labels)) {
      return("Labels hidden.")
    }
    label_csv() %||% "Waiting for draggable label state..."
  })
  output$download_png <- downloadHandler(
    filename = function() "dragmapr-report-plot.png",
    content = function(file) {
      ggplot2::ggsave(file, current_plot(), width = 9, height = 8, dpi = 300)
    },
    contentType = "image/png"
  )
}

shinyApp(ui, server)
