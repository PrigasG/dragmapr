#' RStudio addin for the interactive drag-map prototype
#'
#' Launches a compact Shiny gadget that lets you pick a projected `sf` object
#' from an environment, choose the region and label columns, adjust label,
#' connector, legend, label-filter, movement-context, colour, and static-output
#' settings, and interact with the D3 drag-map prototype inside the IDE viewer
#' pane. When you click **Done**, the current region and label offsets are
#' assigned as `region_offsets` and `label_offsets` in the same environment,
#' ready to pass to [render_dragged_map()].
#'
#' The addin appears under **Addins > Launch dragmapr** in RStudio once the
#' package is installed.
#'
#' @param env Environment to scan for `sf` objects and receive exported offset
#'   tables. Defaults to `.GlobalEnv`, which is what RStudio uses for addins.
#'
#' @return Invisibly returns a list with elements `region_offsets`,
#'   `label_offsets`, and `static_options`. The offset data frames are also
#'   assigned into `env` as `region_offsets` and `label_offsets`; static render
#'   settings are assigned as `dragmapr_static_options`.
#' @seealso [drag_map_prototype()] for the underlying HTML generator;
#'   [render_dragged_map()] to reconstruct a static ggplot2 image from the
#'   returned offsets.
#' @export
dragmapr_addin <- function(env = dragmapr_global_env()) {
  .check_addin_deps()
  if (!is.environment(env)) {
    stop("`env` must be an environment.", call. = FALSE)
  }

  sf_objects_in_env <- function() {
    nms <- ls(envir = env)
    keep <- vapply(nms, function(nm) {
      tryCatch(
        inherits(get(nm, envir = env, inherits = FALSE), "sf"),
        error = function(e) FALSE
      )
    }, logical(1))
    nms[keep]
  }

  default_palette <- c(
    "#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756",
    "#72B7B2", "#EECA3B", "#B74F6F", "#8CD17D", "#79706E"
  )

  valid_hex_color <- function(x) {
    is.character(x) && length(x) == 1L &&
      grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
  }

  palette_from_text <- function(groups, text = NULL) {
    groups <- as.character(groups)
    if (length(groups) == 0L) {
      return(NULL)
    }
    colors <- if (is.null(text) || !nzchar(trimws(text))) {
      default_palette
    } else {
      trimws(strsplit(text, ",", fixed = TRUE)[[1]])
    }
    colors <- colors[nzchar(colors)]
    colors <- colors[vapply(colors, valid_hex_color, logical(1))]
    if (length(colors) == 0L) {
      colors <- default_palette
    }
    stats::setNames(rep(colors, length.out = length(groups)), groups)
  }

  label_data_for <- function(x, region_col, label_col, show_labels,
                             label_edits = list(),
                             annotation_mode = "labels", show_connectors = FALSE,
                             connector_type = "straight", label_width = 64,
                             label_height = 30, box_width = 170,
                             box_height = 76) {
    if (!isTRUE(show_labels)) {
      return(FALSE)
    }
    labels <- make_region_labels(x, region_col = region_col, label_col = label_col)
    if (length(label_edits) > 0L && nrow(labels) > 0L) {
      ids <- as.character(labels$label_id)
      for (id in intersect(names(label_edits), ids)) {
        value <- label_edits[[id]]
        if (!is.null(value)) {
          labels$label[ids == id] <- value
        }
      }
    }
    if (identical(annotation_mode, "boxes")) {
      as_drag_annotations(
        labels,
        width_px = box_width,
        height_px = box_height,
        connector = show_connectors,
        connector_type = connector_type
      )
    } else {
      labels$connector <- isTRUE(show_connectors)
      labels$connector_type <- connector_type
      labels$width_px <- label_width
      labels$height_px <- label_height
      as_drag_labels(labels)
    }
  }

  serve_prototype <- function(x, region_col, label_col, show_labels,
                              label_edits, annotation_mode,
                              label_marker_shape, label_text_size,
                              label_radius, label_width, label_height,
                              box_width, box_height, show_connectors,
                              connector_type, connector_linewidth,
                              connector_color, connector_linetype,
                              connector_endpoint,
                              region_palette, show_legend,
                              max_legend_keys, legend_position,
                              legend_title, legend_values, label_values,
                              show_origin_outlines, show_movement_connectors,
                              movement_connector_color,
                              movement_connector_opacity,
                              movement_connector_linewidth,
                              movement_connector_linetype,
                              movement_connector_endpoint,
                              show_drag_trail) {
    tmp_dir <- tempfile("dragmapr_addin_")
    dir.create(tmp_dir, recursive = TRUE)
    tmp_file <- file.path(tmp_dir, "index.html")
    label_data <- label_data_for(
      x = x,
      region_col = region_col,
      label_col = label_col,
      show_labels = show_labels,
      label_edits = label_edits,
      annotation_mode = annotation_mode,
      show_connectors = show_connectors,
      connector_type = connector_type,
      label_width = label_width,
      label_height = label_height,
      box_width = box_width,
      box_height = box_height
    )
    drag_map_prototype(
      x = x,
      region_col = region_col,
      label_col = label_col,
      labels = label_data,
      label_marker_shape = label_marker_shape,
      label_radius = label_radius,
      label_text_size = label_text_size,
      label_width = label_width,
      label_height = label_height,
      label_box_width = box_width,
      label_box_height = box_height,
      connector_color = connector_color,
      connector_linewidth = connector_linewidth,
      connector_linetype = connector_linetype,
      connector_endpoint = connector_endpoint,
      region_palette = region_palette,
      show_legend = show_legend,
      max_legend_keys = max_legend_keys,
      legend_position = legend_position,
      legend_title = legend_title,
      legend_values = legend_values,
      label_values = label_values,
      show_origin_outlines = show_origin_outlines,
      show_movement_connectors = show_movement_connectors,
      movement_connector_color = movement_connector_color,
      movement_connector_opacity = movement_connector_opacity,
      movement_connector_linewidth = movement_connector_linewidth,
      movement_connector_linetype = movement_connector_linetype,
      movement_connector_endpoint = movement_connector_endpoint,
      show_drag_trail = show_drag_trail,
      file = tmp_file,
      open = FALSE,
      side_panel = FALSE
    )
    list(dir = tmp_dir, file = tmp_file)
  }

  parse_offset_csv <- function(csv_text) {
    if (is.null(csv_text) || !nzchar(trimws(csv_text))) {
      return(NULL)
    }
    tryCatch(
      utils::read.csv(text = csv_text, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }

  rows_for_message <- function(x) {
    jsonlite::fromJSON(
      jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }

  selected_values <- function(values, all_values) {
    all_values <- as.character(all_values)
    values <- as.character(values %||% character())
    values <- intersect(values, all_values)
    if (length(values) == length(all_values) && setequal(values, all_values)) {
      NULL
    } else {
      values
    }
  }

  valid_color_or_default <- function(value, default) {
    if (valid_hex_color(value)) value else default
  }

  sf_choices <- function() {
    sf_nms <- sf_objects_in_env()
    if (length(sf_nms) == 0L) {
      c("(no sf objects found)" = "")
    } else {
      sf_nms
    }
  }

  initial_sf_choices <- sf_choices()
  initial_sf <- unname(initial_sf_choices[[1]])

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "dragmapr",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        dragmapr_iframe_bridge(
          region_input = "region_csv",
          label_input = "label_csv",
          iframe_selector = "iframe.dragmapr-helper-frame",
          allowed_origin = "same-origin"
        )
      )),
      shiny::tags$script(shiny::HTML("
        function resizeDragmaprFrame(iframe) {
          if (!iframe || !iframe.contentWindow) return;
          [150, 600].forEach(function(delay) {
            window.setTimeout(function() {
              try { iframe.contentWindow.dispatchEvent(new Event('resize')); } catch (e) {}
            }, delay);
          });
        }
        function dragmaprHelperFrame() {
          return document.querySelector('iframe.dragmapr-helper-frame');
        }
        function dragmaprHelperOrigin() {
          var iframe = dragmaprHelperFrame();
          if (!iframe || !iframe.src) return window.location.origin;
          try {
            return new URL(iframe.src, window.location.href).origin;
          } catch (e) {
            return window.location.origin;
          }
        }
        Shiny.addCustomMessageHandler('dragmapr-labels', function(message) {
          var iframe = dragmaprHelperFrame();
          if (iframe && iframe.contentWindow) {
            iframe.contentWindow.postMessage(
              {type: 'dragmapr-set-labels', labels: !!message.labels},
              dragmaprHelperOrigin()
            );
          }
        });
        Shiny.addCustomMessageHandler('dragmapr-label-data', function(message) {
          var iframe = dragmaprHelperFrame();
          if (iframe && iframe.contentWindow) {
            iframe.contentWindow.postMessage(
              {type: 'dragmapr-set-label-data', labels: message.labels || []},
              dragmaprHelperOrigin()
            );
          }
        });
        Shiny.addCustomMessageHandler('dragmapr-label-options', function(message) {
          var iframe = dragmaprHelperFrame();
          if (iframe && iframe.contentWindow) {
            iframe.contentWindow.postMessage(
              {type: 'dragmapr-set-label-options', options: message.options || {}},
              dragmaprHelperOrigin()
            );
          }
        });
      ")),
      shiny::tags$style(shiny::HTML("
        body {
          background: #f8fafc;
          color: #172033;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        .gadget-container {
          padding: 0;
        }
        .dragmapr-layout {
          display: flex;
          gap: 14px;
          height: calc(100vh - 42px);
          padding: 12px;
          box-sizing: border-box;
        }
        .dragmapr-sidebar {
          width: 300px;
          min-width: 280px;
          max-width: 340px;
          display: flex;
          flex-direction: column;
          gap: 12px;
          overflow-y: auto;
        }
        .dragmapr-card {
          background: #ffffff;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
          box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
          padding: 12px;
        }
        .dragmapr-card .checkbox,
        .dragmapr-card .radio {
          margin-top: 4px;
          margin-bottom: 8px;
        }
        .dragmapr-section {
          font-size: 0.7rem;
          font-weight: 700;
          letter-spacing: 0.07em;
          text-transform: uppercase;
          color: #4b5563;
          margin: 0 0 10px;
          padding-bottom: 5px;
          border-bottom: 1px solid #e5e7eb;
        }
        .dragmapr-sidebar label {
          font-size: 12px;
          font-weight: 650;
          color: #334155;
        }
        .dragmapr-sidebar .form-group {
          margin-bottom: 10px;
        }
        .dragmapr-sidebar .btn {
          margin-bottom: 0;
        }
        .dragmapr-action-row {
          display: flex;
          gap: 8px;
          flex-wrap: wrap;
        }
        .dragmapr-action-row .btn,
        .dragmapr-primary-action {
          width: 100%;
          font-size: 12px;
        }
        .dragmapr-slider .form-group {
          margin-bottom: 4px;
        }
        .dragmapr-slider .irs {
          margin-top: -6px;
        }
        .dragmapr-number-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          gap: 8px;
        }
        .dragmapr-number-grid .form-group {
          margin-bottom: 4px;
        }
        .dragmapr-help {
          color: #64748b;
          font-size: 12px;
          line-height: 1.35;
          margin: 4px 0 0;
        }
        .dragmapr-card textarea {
          min-height: 68px;
          resize: vertical;
        }
        .dragmapr-main {
          flex: 1;
          min-width: 0;
          display: flex;
          flex-direction: column;
          gap: 8px;
        }
        .dragmapr-toolbar {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 10px;
          min-height: 42px;
          padding: 8px 10px;
          border: 1px solid #d9e0ea;
          border-radius: 8px;
          background: #ffffff;
          box-shadow: 0 1px 3px rgba(15, 23, 42, 0.05);
        }
        .dragmapr-toolbar-title {
          font-size: 0.76rem;
          font-weight: 700;
          color: #475569;
          letter-spacing: 0.05em;
          text-transform: uppercase;
          white-space: nowrap;
        }
        .dragmapr-iframe-wrap {
          flex: 1;
          min-height: 0;
          overflow: hidden;
          border: 1px solid #d9e0ea;
          border-radius: 8px;
          background: #ffffff;
          box-shadow: 0 1px 3px rgba(15, 23, 42, 0.05);
          display: flex;
          flex-direction: column;
        }
        /* shiny::uiOutput inserts an extra div; give it full height so
           the iframe's height:100% resolves correctly. Without this the
           D3 projection reads a tiny clientHeight and draws a tiny map. */
        .dragmapr-iframe-wrap .shiny-html-output {
          flex: 1;
          min-height: 0;
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        .dragmapr-iframe-wrap iframe {
          width: 100%;
          flex: 1;
          border: none;
        }
        .dragmapr-empty {
          display: flex;
          align-items: center;
          justify-content: center;
          height: 100%;
          color: #64748b;
          font-size: 13px;
          text-align: center;
          padding: 24px;
        }
        .dragmapr-status {
          display: flex;
          align-items: center;
          gap: 8px;
          min-height: 18px;
          font-size: 12px;
          line-height: 1.35;
          color: #475569;
        }
        .dragmapr-status.ok  { color: #16a34a; }
        .dragmapr-status.err { color: #dc2626; }
        @media (max-width: 760px) {
          .dragmapr-layout {
            flex-direction: column;
            height: auto;
          }
          .dragmapr-sidebar {
            width: auto;
            min-width: 0;
            max-width: none;
            overflow-y: visible;
          }
          .dragmapr-iframe-wrap {
            min-height: 560px;
          }
        }
      "))
    ),
    shiny::div(
      class = "dragmapr-layout",
      shiny::div(
        class = "dragmapr-sidebar",
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Data source", class = "dragmapr-section"),
          shiny::selectInput(
            "sf_name", "sf object",
            choices = initial_sf_choices,
            selected = initial_sf,
            width = "100%"
          ),
          shiny::div(
            class = "dragmapr-action-row",
            shiny::actionButton(
              "refresh_objects", "Refresh objects",
              class = "btn-sm btn-default dragmapr-primary-action"
            )
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Columns & colors", class = "dragmapr-section"),
          shiny::selectInput("region_col", "Region column", choices = character(0), width = "100%"),
          shiny::selectInput("label_col", "Label column", choices = character(0), width = "100%"),
          shiny::textInput(
            "region_palette_text", "Region colors",
            value = paste(default_palette, collapse = ", "),
            placeholder = "#4C78A8, #F58518, #54A24B"
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Legend", class = "dragmapr-section"),
          shiny::checkboxInput("show_legend", "Show legend", value = FALSE),
          shiny::checkboxInput("legend_show_all", "Show all legend keys", value = FALSE),
          shiny::textInput("legend_title", "Legend title", value = "Region"),
          shiny::uiOutput("legend_filter_ui"),
          shiny::selectInput(
            "legend_position", "Legend position",
            choices = c("Bottom" = "bottom", "Top" = "top",
                        "Left" = "left", "Right" = "right", "None" = "none"),
            selected = "bottom",
            width = "100%"
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Labels", class = "dragmapr-section"),
          shiny::checkboxInput("show_labels", "Show labels", value = TRUE),
          shiny::uiOutput("label_filter_ui"),
          shiny::uiOutput("label_editor_ui"),
          shiny::radioButtons(
            "annotation_mode", "Annotation style",
            choices = c("Short labels" = "labels", "Info boxes" = "boxes"),
            selected = "labels"
          ),
          shiny::selectInput(
            "label_marker_shape", "Text label marker",
            choices = c("Rounded box" = "rect", "Circle" = "circle", "Text only" = "none"),
            selected = "rect",
            width = "100%"
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("label_text_size", "Text size", min = 7, max = 22, value = 11, step = 1)
          ),
          shiny::uiOutput("label_size_controls")
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Connectors", class = "dragmapr-section"),
          shiny::checkboxInput("show_connectors", "Show connector lines", value = FALSE),
          shiny::textInput("connector_color", "Connector color", value = "#334155"),
          shiny::selectInput(
            "connector_type", "Connector style",
            choices = c("Straight" = "straight", "Elbow" = "elbow",
                        "Curve" = "curve", "Squiggle" = "squiggle"),
            selected = "straight",
            width = "100%"
          ),
          shiny::selectInput(
            "connector_linetype", "Line style",
            choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"),
            selected = "solid",
            width = "100%"
          ),
          shiny::selectInput(
            "connector_endpoint", "Arrow",
            choices = c("None" = "none", "Arrow" = "arrow"),
            selected = "none",
            width = "100%"
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("connector_linewidth", "Connector thickness",
                               min = 0.25, max = 2.5, value = 1.3, step = 0.05)
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Movement context", class = "dragmapr-section"),
          shiny::checkboxInput("show_origin_outlines", "Show origin outlines", value = FALSE),
          shiny::checkboxInput("show_movement_connectors", "Show movement connectors", value = FALSE),
          shiny::checkboxInput("show_drag_trail", "Show drag preview trail", value = FALSE),
          shiny::textInput("movement_connector_color", "Movement connector color", value = "#64748b"),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("movement_connector_opacity", "Movement connector opacity",
                               min = 0, max = 1, value = 0.72, step = 0.02)
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("movement_connector_linewidth", "Movement connector thickness",
                               min = 0.25, max = 3, value = 1.4, step = 0.05)
          ),
          shiny::selectInput(
            "movement_connector_linetype", "Movement connector line style",
            choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"),
            selected = "solid",
            width = "100%"
          ),
          shiny::selectInput(
            "movement_connector_endpoint", "Movement connector arrow",
            choices = c("Closed arrow" = "closed", "Open arrow" = "open", "None" = "none"),
            selected = "closed",
            width = "100%"
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Static output", class = "dragmapr-section"),
          shiny::textInput("static_title", "Static map title", value = "dragmapr map"),
          shiny::div(
            class = "dragmapr-number-grid",
            shiny::numericInput("static_width", "Width", value = 10, min = 2, max = 30, step = 0.5),
            shiny::numericInput("static_height", "Height", value = 8, min = 2, max = 30, step = 0.5),
            shiny::numericInput("static_dpi", "DPI", value = 300, min = 72, max = 600, step = 24)
          ),
          shiny::tags$p(
            "These settings are kept with the returned offsets for your static render script.",
            class = "dragmapr-help"
          )
        ),
        shiny::div(
          class = "dragmapr-card",
          shiny::div("Render", class = "dragmapr-section"),
          shiny::actionButton(
            "render_btn", "Render prototype",
            class = "btn-primary dragmapr-primary-action"
          )
        )
      ),
      shiny::div(
        class = "dragmapr-main",
        shiny::div(
          class = "dragmapr-toolbar",
          shiny::span("Map workspace", class = "dragmapr-toolbar-title"),
          shiny::uiOutput("status_msg")
        ),
        shiny::div(
          class = "dragmapr-iframe-wrap",
          shiny::uiOutput("prototype_frame")
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      resource_name = NULL,
      region_offsets = data.frame(
        region = character(), dx_m = numeric(), dy_m = numeric(),
        stringsAsFactors = FALSE
      ),
      label_offsets = data.frame(
        label_id = character(), region = character(),
        dx_m = numeric(), dy_m = numeric(),
        stringsAsFactors = FALSE
      ),
      frame_src = NULL,
      label_edits = list(),
      status = "",
      status_class = "info"
    )

    current_sf <- function() {
      nm <- input$sf_name
      if (is.null(nm) || !nzchar(nm)) {
        return(NULL)
      }
      x <- tryCatch(get(nm, envir = env, inherits = FALSE), error = function(e) NULL)
      if (is.null(x) || !inherits(x, "sf")) {
        return(NULL)
      }
      x
    }

    current_label_rows <- function() {
      x <- current_sf()
      region_col <- input$region_col
      label_col <- input$label_col
      if (is.null(x) || is.null(region_col) || is.null(label_col) ||
          !nzchar(region_col) || !nzchar(label_col)) {
        return(data.frame())
      }
      tryCatch(
        label_data_for(
          x = x,
          region_col = region_col,
          label_col = label_col,
          show_labels = isTRUE(input$show_labels),
          label_edits = rv$label_edits,
          annotation_mode = input$annotation_mode %||% "labels",
          show_connectors = isTRUE(input$show_connectors),
          connector_type = input$connector_type %||% "straight",
          label_width = input$label_width %||% 64,
          label_height = input$label_height %||% 30,
          box_width = input$box_width %||% 170,
          box_height = input$box_height %||% 76
        ),
        error = function(e) {
          rv$status <- paste("Could not derive label rows:", conditionMessage(e))
          rv$status_class <- "warn"
          data.frame()
        }
      )
    }

    region_groups <- function() {
      x <- current_sf()
      region_col <- input$region_col
      if (is.null(x) || is.null(region_col) || !nzchar(region_col)) {
        return(character())
      }
      sort(unique(as.character(x[[region_col]])))
    }

    current_palette <- function() {
      palette_from_text(region_groups(), input$region_palette_text)
    }

    visible_region_values <- function(input_id) {
      selected_values(input[[input_id]], region_groups())
    }

    base_label_text <- function(label_id) {
      x <- current_sf()
      region_col <- input$region_col
      label_col <- input$label_col
      if (is.null(x) || is.null(region_col) || is.null(label_col) ||
          !nzchar(region_col) || !nzchar(label_col)) {
        return("")
      }
      labels <- make_region_labels(x, region_col = region_col, label_col = label_col)
      idx <- match(label_id, as.character(labels$label_id))
      if (is.na(idx)) "" else as.character(labels$label[[idx]])
    }

    send_style_controls <- function() {
      if (is.null(rv$frame_src)) {
        return()
      }
      session$sendCustomMessage("dragmapr-labels", list(labels = isTRUE(input$show_labels)))
      label_rows <- current_label_rows()
      label_values <- visible_region_values("visible_labels")
      legend_values <- visible_region_values("visible_legend_values")
      session$sendCustomMessage(
        "dragmapr-label-data",
        list(labels = if (is.data.frame(label_rows)) rows_for_message(label_rows) else list())
      )
      session$sendCustomMessage("dragmapr-set-label-values", list(values = label_values))
      session$sendCustomMessage("dragmapr-set-legend-values", list(values = legend_values))
      session$sendCustomMessage("dragmapr-set-region-palette", list(palette = as.list(current_palette())))
      session$sendCustomMessage("dragmapr-label-options", list(options = list(
        labelMarker = !identical(input$label_marker_shape %||% "rect", "none"),
        labelMarkerShape = input$label_marker_shape %||% "rect",
        labelTextSize = input$label_text_size %||% 11,
        labelRadius = input$label_radius %||% 14,
        labelWidth = input$label_width %||% 64,
        labelHeight = input$label_height %||% 30,
        labelBoxWidth = input$box_width %||% 170,
        labelBoxHeight = input$box_height %||% 76,
        connectorColor = valid_color_or_default(input$connector_color, "#334155"),
        connectorLinewidth = input$connector_linewidth %||% 1.3,
        connectorLinetype = input$connector_linetype %||% "solid",
        connectorEndpoint = input$connector_endpoint %||% "none",
        regionPalette = as.list(current_palette()),
        showLegend = isTRUE(input$show_legend),
        maxLegendKeys = if (isTRUE(input$legend_show_all)) 1000000 else 25,
        legendPosition = input$legend_position %||% "bottom",
        legendTitle = input$legend_title %||% "Region",
        showOriginOutlines = isTRUE(input$show_origin_outlines),
        showMovementConnectors = isTRUE(input$show_movement_connectors),
        movementConnectorColor = valid_color_or_default(input$movement_connector_color, "#64748b"),
        movementConnectorOpacity = input$movement_connector_opacity %||% 0.72,
        movementConnectorLinewidth = input$movement_connector_linewidth %||% 1.4,
        movementConnectorLinetype = input$movement_connector_linetype %||% "solid",
        movementConnectorEndpoint = input$movement_connector_endpoint %||% "closed",
        showDragTrail = isTRUE(input$show_drag_trail)
      )))
      rv$status <- "Updated map controls."
      rv$status_class <- "ok"
    }

    shiny::observeEvent(input$refresh_objects, {
      choices <- sf_choices()
      selected <- input$sf_name
      if (!selected %in% unname(choices)) {
        selected <- unname(choices[[1]])
      }
      shiny::updateSelectInput(session, "sf_name", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$sf_name, {
      nm <- input$sf_name
      if (!nzchar(nm)) {
        return()
      }
      x <- tryCatch(get(nm, envir = env, inherits = FALSE), error = function(e) NULL)
      if (is.null(x) || !inherits(x, "sf")) {
        return()
      }
      cols <- setdiff(names(x), attr(x, "sf_column"))
      if (length(cols) == 0L) {
        rv$status <- paste0("Object '", nm, "' has no non-geometry columns.")
        rv$status_class <- "err"
        return()
      }
      shiny::updateSelectInput(session, "region_col", choices = cols, selected = cols[[1]])
      shiny::updateSelectInput(session, "label_col", choices = cols, selected = cols[[1]])
    }, ignoreInit = FALSE)

    shiny::observeEvent(list(input$sf_name, input$region_col, input$label_col), {
      rv$label_edits <- list()
    }, ignoreInit = TRUE)

    output$label_filter_ui <- shiny::renderUI({
      groups <- region_groups()
      shiny::selectInput(
        "visible_labels",
        "Visible labels",
        choices = stats::setNames(groups, groups),
        selected = groups,
        multiple = TRUE,
        width = "100%"
      )
    })

    output$legend_filter_ui <- shiny::renderUI({
      groups <- region_groups()
      shiny::selectInput(
        "visible_legend_values",
        "Legend values",
        choices = stats::setNames(groups, groups),
        selected = groups,
        multiple = TRUE,
        width = "100%"
      )
    })

    output$label_editor_ui <- shiny::renderUI({
      rows <- current_label_rows()
      if (!is.data.frame(rows) || nrow(rows) == 0L) {
        return(NULL)
      }
      ids <- as.character(rows$label_id)
      labels <- paste0(rows$label, " (", rows$region, ")")
      choices <- stats::setNames(ids, labels)
      selected <- input$edit_label_id
      if (is.null(selected) || !selected %in% ids) {
        selected <- ids[[1]]
      }
      shiny::tagList(
        shiny::selectInput("edit_label_id", "Edit label text", choices = choices, selected = selected, width = "100%"),
        shiny::textAreaInput("edit_label_text", NULL, value = rv$label_edits[[selected]] %||% base_label_text(selected), width = "100%"),
        shiny::actionButton("reset_label_text", "Reset selected label", class = "btn-sm btn-default dragmapr-primary-action")
      )
    })

    output$label_size_controls <- shiny::renderUI({
      mode <- input$annotation_mode %||% "labels"
      if (identical(mode, "boxes")) {
        shiny::tagList(
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("box_width", "Box width", min = 110, max = 280, value = 170, step = 5)
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("box_height", "Box height", min = 48, max = 150, value = 76, step = 4)
          )
        )
      } else {
        shiny::tagList(
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("label_radius", "Circle radius", min = 0, max = 30, value = 14, step = 1)
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("label_width", "Marker width", min = 32, max = 180, value = 64, step = 4)
          ),
          shiny::div(
            class = "dragmapr-slider",
            shiny::sliderInput("label_height", "Marker height", min = 22, max = 90, value = 30, step = 2)
          )
        )
      }
    })

    shiny::observeEvent(input$edit_label_id, {
      id <- input$edit_label_id
      if (is.null(id) || !nzchar(id)) {
        return()
      }
      shiny::updateTextAreaInput(session, "edit_label_text", value = rv$label_edits[[id]] %||% base_label_text(id))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$edit_label_text, {
      id <- input$edit_label_id
      if (is.null(id) || !nzchar(id)) {
        return()
      }
      edits <- rv$label_edits
      value <- input$edit_label_text %||% ""
      if (identical(value, base_label_text(id))) {
        edits[[id]] <- NULL
      } else {
        edits[[id]] <- value
      }
      rv$label_edits <- edits
      send_style_controls()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset_label_text, {
      id <- input$edit_label_id
      if (is.null(id) || !nzchar(id)) {
        return()
      }
      edits <- rv$label_edits
      edits[[id]] <- NULL
      rv$label_edits <- edits
      shiny::updateTextAreaInput(session, "edit_label_text", value = base_label_text(id))
      send_style_controls()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$render_btn, {
      nm <- input$sf_name
      region_col <- input$region_col
      label_col <- input$label_col
      if (!nzchar(nm) || !nzchar(region_col)) {
        rv$status <- "Select an sf object and region column first."
        rv$status_class <- "err"
        return()
      }
      x <- tryCatch(get(nm, envir = env, inherits = FALSE), error = function(e) NULL)
      if (is.null(x)) {
        rv$status <- paste0("Object '", nm, "' not found.")
        rv$status_class <- "err"
        return()
      }

      if (!is.null(rv$resource_name)) {
        tryCatch(shiny::removeResourcePath(rv$resource_name), error = function(e) NULL)
      }

      result <- tryCatch(
        serve_prototype(
          x = x,
          region_col = region_col,
          label_col = label_col,
          show_labels = isTRUE(input$show_labels),
          label_edits = rv$label_edits,
          annotation_mode = input$annotation_mode %||% "labels",
          label_marker_shape = input$label_marker_shape %||% "rect",
          label_text_size = input$label_text_size %||% 11,
          label_radius = input$label_radius %||% 14,
          label_width = input$label_width %||% 64,
          label_height = input$label_height %||% 30,
          box_width = input$box_width %||% 170,
          box_height = input$box_height %||% 76,
          show_connectors = isTRUE(input$show_connectors),
          connector_type = input$connector_type %||% "straight",
          connector_linewidth = input$connector_linewidth %||% 1.3,
          connector_color = valid_color_or_default(input$connector_color, "#334155"),
          connector_linetype = input$connector_linetype %||% "solid",
          connector_endpoint = input$connector_endpoint %||% "none",
          region_palette = current_palette(),
          show_legend = isTRUE(input$show_legend),
          max_legend_keys = if (isTRUE(input$legend_show_all)) 1000000 else 25,
          legend_position = input$legend_position %||% "bottom",
          legend_title = input$legend_title %||% "Region",
          legend_values = visible_region_values("visible_legend_values"),
          label_values = visible_region_values("visible_labels"),
          show_origin_outlines = isTRUE(input$show_origin_outlines),
          show_movement_connectors = isTRUE(input$show_movement_connectors),
          movement_connector_color = valid_color_or_default(input$movement_connector_color, "#64748b"),
          movement_connector_opacity = input$movement_connector_opacity %||% 0.72,
          movement_connector_linewidth = input$movement_connector_linewidth %||% 1.4,
          movement_connector_linetype = input$movement_connector_linetype %||% "solid",
          movement_connector_endpoint = input$movement_connector_endpoint %||% "closed",
          show_drag_trail = isTRUE(input$show_drag_trail)
        ),
        error = function(e) {
          rv$status <- conditionMessage(e)
          rv$status_class <- "err"
          NULL
        }
      )
      if (is.null(result)) {
        return()
      }

      res_name <- paste0("dragmapr_", as.integer(proc.time()[[3]] * 1000))
      shiny::addResourcePath(res_name, result$dir)
      rv$resource_name <- res_name
      rv$frame_src <- paste0("/", res_name, "/index.html")
      rv$status <- paste0("Prototype ready - sf: ", nm)
      rv$status_class <- "ok"
    })

    shiny::observeEvent(input$region_csv, {
      df <- parse_offset_csv(input$region_csv)
      if (!is.null(df)) {
        rv$region_offsets <- df
      }
    })

    shiny::observeEvent(input$label_csv, {
      df <- parse_offset_csv(input$label_csv)
      if (!is.null(df)) {
        rv$label_offsets <- df
      }
    })

    shiny::observeEvent(
      list(input$show_labels, input$visible_labels, input$annotation_mode,
           input$label_marker_shape, input$label_text_size, input$label_radius,
           input$label_width, input$label_height, input$box_width, input$box_height,
           input$show_connectors, input$connector_type, input$connector_linewidth,
           input$connector_color, input$connector_linetype, input$connector_endpoint,
           input$region_palette_text, input$show_legend, input$legend_show_all,
           input$legend_position, input$legend_title, input$visible_legend_values,
           input$show_origin_outlines, input$show_movement_connectors,
           input$movement_connector_color, input$movement_connector_opacity,
           input$movement_connector_linewidth, input$movement_connector_linetype,
           input$movement_connector_endpoint, input$show_drag_trail),
      send_style_controls(),
      ignoreInit = TRUE
    )

    output$prototype_frame <- shiny::renderUI({
      src <- rv$frame_src
      if (is.null(src)) {
        return(shiny::div(
          class = "dragmapr-empty",
          "Select an sf object, choose columns, and render the prototype."
        ))
      }
      shiny::tags$iframe(
        class = "dragmapr-helper-frame",
        src = src,
        onload = "resizeDragmaprFrame(this);",
        style = "width:100%;height:100%;border:none;"
      )
    })

    output$status_msg <- shiny::renderUI({
      shiny::div(class = paste("dragmapr-status", rv$status_class), rv$status)
    })

    shiny::observeEvent(input$done, {
      region_offsets <- rv$region_offsets
      label_offsets <- rv$label_offsets
      static_options <- list(
        title = input$static_title %||% "dragmapr map",
        width = input$static_width %||% 10,
        height = input$static_height %||% 8,
        dpi = input$static_dpi %||% 300,
        region_col = input$region_col %||% NULL,
        label_col = input$label_col %||% NULL,
        region_palette = current_palette(),
        show_legend = isTRUE(input$show_legend),
        max_legend_keys = if (isTRUE(input$legend_show_all)) Inf else 25,
        legend_position = input$legend_position %||% "bottom",
        legend_title = input$legend_title %||% "Region",
        legend_values = visible_region_values("visible_legend_values"),
        label_values = visible_region_values("visible_labels"),
        show_label_marker = !identical(input$label_marker_shape %||% "rect", "none"),
        label_marker_shape = input$label_marker_shape %||% "rect",
        label_size = (input$label_text_size %||% 11) / 3,
        marker_size = (input$label_radius %||% 14) / 3,
        connector_linewidth = input$connector_linewidth %||% 1.3,
        connector_color = valid_color_or_default(input$connector_color, "#334155"),
        connector_linetype = input$connector_linetype %||% "solid",
        connector_endpoint = input$connector_endpoint %||% "none",
        show_origin_outlines = isTRUE(input$show_origin_outlines),
        show_movement_connectors = isTRUE(input$show_movement_connectors),
        movement_connector_color = valid_color_or_default(input$movement_connector_color, "#64748b"),
        movement_connector_opacity = input$movement_connector_opacity %||% 0.72,
        movement_connector_linewidth = input$movement_connector_linewidth %||% 1.4,
        movement_connector_linetype = input$movement_connector_linetype %||% "solid",
        movement_connector_endpoint = input$movement_connector_endpoint %||% "closed"
      )
      assign("region_offsets", region_offsets, envir = env)
      assign("label_offsets", label_offsets, envir = env)
      assign("dragmapr_static_options", static_options, envir = env)
      message(
        "dragmapr: assigned `region_offsets` (",
        nrow(region_offsets), " row(s)) and `label_offsets` (",
        nrow(label_offsets), " row(s)) to the target environment."
      )
      if (!is.null(rv$resource_name)) {
        tryCatch(shiny::removeResourcePath(rv$resource_name), error = function(e) NULL)
      }
      shiny::stopApp(invisible(list(
        region_offsets = region_offsets,
        label_offsets = label_offsets,
        static_options = static_options
      )))
    })

    shiny::observeEvent(input$cancel, {
      if (!is.null(rv$resource_name)) {
        tryCatch(shiny::removeResourcePath(rv$resource_name), error = function(e) NULL)
      }
      shiny::stopApp(invisible(NULL))
    })
  }

  result <- shiny::runGadget(
    ui, server,
    viewer = shiny::paneViewer(minHeight = 500)
  )
  invisible(result)
}

.check_addin_deps <- function() {
  missing_pkgs <- character(0)
  if (!requireNamespace("shiny", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "shiny")
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "miniUI")
  }
  if (length(missing_pkgs) > 0L) {
    stop(
      "The dragmapr addin requires the following package(s) to be installed: ",
      paste(missing_pkgs, collapse = ", "),
      ".\nInstall with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))",
      call. = FALSE
    )
  }
}

dragmapr_global_env <- function() {
  get(".GlobalEnv", envir = baseenv())
}
