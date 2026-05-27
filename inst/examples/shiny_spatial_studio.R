# Shiny spatial studio: upload or link sf data, drag grouped geometry, annotate, and export.
#
# Run with:
# shiny::runApp(system.file("examples", "shiny_spatial_studio.R", package = "dragmapr"))

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 5 * 1024^2), 100 * 1024^2))

library(dragmapr)
library(shiny)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

read_csv_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  utils::read.csv(text = text, stringsAsFactors = FALSE, check.names = FALSE)
}

is_blank <- function(x) is.null(x) || length(x) == 0L || !nzchar(trimws(x[1]))

valid_hex_color <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
}

# Show individual colorPickr per region up to this many groups; above it switch
# to a compact cycle-palette editor so the sidebar stays usable.
CPICKER_THRESHOLD <- 20L

# Legend is suppressed automatically in render_dragged_map() above this count.
LEGEND_THRESHOLD  <- 25L

default_palette <- c(
  "#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756",
  "#72B7B2", "#EECA3B", "#B74F6F", "#8CD17D", "#79706E"
)

default_label_colors <- c("#111827", "#1D4ED8", "#047857", "#B91C1C", "#7C3AED", "#B45309")

palette_for <- function(groups, text = NULL) {
  groups <- stable_unique(groups)
  colors <- if (is_blank(text)) {
    rep(default_palette, length.out = length(groups))
  } else {
    trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  }
  colors <- colors[nzchar(colors)]
  if (length(colors) == 0L) colors <- default_palette
  stats::setNames(rep(colors, length.out = length(groups)), groups)
}

safe_names <- function(x) {
  names(x)[vapply(x, function(col) {
    is.atomic(col) && !inherits(col, "sfc") && length(unique(stats::na.omit(col))) > 0L
  }, logical(1))]
}

stable_unique <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  values <- unique(x)
  values[order(natural_sort_key(values), values, method = "radix")]
}

natural_sort_key <- function(x) {
  vapply(as.character(x), function(value) {
    matches <- gregexpr("[0-9]+", value, perl = TRUE)[[1]]
    if (matches[1] < 0L) {
      return(value)
    }
    lengths <- attr(matches, "match.length")
    pieces <- regmatches(value, list(matches))[[1]]
    padded <- sprintf("%020.0f", as.numeric(pieces))
    regmatches(value, list(matches)) <- list(padded)
    value
  }, character(1), USE.NAMES = FALSE)
}

factor_for_display <- function(x) {
  factor(as.character(x), levels = stable_unique(x), ordered = TRUE)
}

label_color_input_id <- function(label_id) {
  paste0("label_color_", gsub("[^A-Za-z0-9_]", "_", as.character(label_id)))
}

choose_column <- function(candidate, cols, fallback = NULL) {
  if (length(cols) == 0L) return(NULL)
  fallback <- fallback %||% cols[1]
  if (!is.null(candidate) && candidate %in% cols) {
    candidate
  } else if (fallback %in% cols) {
    fallback
  } else {
    cols[1]
  }
}

make_studio_labels <- function(x, region_col, label_col, mode, width_px, height_px,
                               connector, connector_type) {
  labels <- make_region_labels(x, region_col = region_col, label_col = label_col)
  if (identical(mode, "boxes")) {
    labels$label <- paste0(labels$label, "\n", "Drag this note or edit the label column.")
    labels <- as_drag_annotations(labels, width_px = width_px, height_px = height_px,
                                  connector = connector, connector_type = connector_type)
  } else {
    labels$connector <- isTRUE(connector)
    labels$connector_type <- connector_type
    labels <- as_drag_labels(labels)
  }
  labels
}

empty_label_offsets <- function(labels) {
  n <- nrow(labels)
  data.frame(
    label_id = labels$label_id,
    region   = labels$region,
    dx_m = rep(0, n),
    dy_m = rep(0, n),
    stringsAsFactors = FALSE
  )
}

# ---- Inline CSS ---------------------------------------------------------------

studio_css <- "
/* ---- Layout ---- */
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
h2.studio-title {
  font-size: 1.35rem;
  font-weight: 700;
  margin: 0 0 2px;
  color: #1a1f36;
}
p.studio-subtitle {
  font-size: 0.85rem;
  color: #6b7280;
  margin: 0 0 16px;
}

/* ---- Sidebar sections ---- */
.studio-card {
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  background: #ffffff;
  padding: 10px 12px 12px;
  margin-bottom: 12px;
  box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
}
.studio-section {
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: #4b5563;
  margin: 0 0 10px;
  padding-bottom: 4px;
  border-bottom: 1px solid #e5e7eb;
}
.studio-card .form-group { margin-bottom: 10px; }
.studio-card .checkbox { margin-top: 4px; margin-bottom: 8px; }
.studio-card .radio { margin-top: 4px; margin-bottom: 4px; }
.studio-card .btn { margin-bottom: 4px; }

/* ---- Status bar ---- */
.studio-status {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 8px 12px;
  border-radius: 6px;
  font-size: 0.82rem;
  line-height: 1.4;
  margin-bottom: 12px;
}
.studio-status.info  { background: #eff6ff; color: #1d4ed8; border: 1px solid #bfdbfe; }
.studio-status.ok    { background: #f0fdf4; color: #166534; border: 1px solid #bbf7d0; }
.studio-status.error { background: #fef2f2; color: #991b1b; border: 1px solid #fecaca; }
.studio-status .status-icon { font-size: 1rem; flex-shrink: 0; }

/* ---- Color picker rows ---- */
.cpicker-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4px 12px;
  margin-bottom: 4px;
}
.cpicker-row {
  display: flex;
  align-items: center;
  gap: 6px;
}
.cpicker-label {
  font-size: 0.78rem;
  color: #374151;
  flex: 1;
  min-width: 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
/* Make the nano colorPickr button compact */
.cpicker-row .pcr-button { width: 24px !important; height: 24px !important; border-radius: 4px !important; }

/* ---- Download grid ---- */
.download-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
  margin-top: 8px;
}
.download-grid .btn { width: 100%; font-size: 0.78rem; padding: 5px 8px; }

/* ---- Top progress bar (fires on any server-side work) ---- */
.studio-progress-bar {
  position: fixed;
  top: 0; left: 0; right: 0;
  height: 3px;
  background: linear-gradient(90deg, #4C78A8 0%, #54A24B 50%, #F58518 100%);
  background-size: 200% 100%;
  z-index: 9999;
  opacity: 0;
  transition: opacity 0.15s;
  animation: progress-sweep 1.4s linear infinite;
}
body.shiny-busy .studio-progress-bar { opacity: 1; }
@keyframes progress-sweep {
  0%   { background-position: 100% 0; }
  100% { background-position: -100% 0; }
}

/* ---- Iframe loading overlay ---- */
.helper-wrap { position: relative; }
.helper-overlay {
  position: absolute;
  inset: 0;
  background: rgba(255, 255, 255, 0.85);
  backdrop-filter: blur(2px);
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 14px;
  z-index: 10;
  border-radius: 6px;
}
.helper-overlay p {
  font-size: 0.85rem;
  color: #6b7280;
  margin: 0;
}
.studio-spinner {
  width: 36px; height: 36px;
  border: 3px solid #e5e7eb;
  border-top-color: #4C78A8;
  border-radius: 50%;
  animation: spin 0.75s linear infinite;
}
@keyframes spin { to { transform: rotate(360deg); } }

/* ---- Preview recalculating veil ---- */
.shiny-output-container.recalculating { opacity: 0.45; transition: opacity 0.2s; }
"

# ---- UI -----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    # Iframe bridge: relays drag state from helper to Shiny inputs
    tags$script(HTML(dragmapr_iframe_bridge())),
    # Label-toggle message handler (app-specific, stays inline)
    tags$script(HTML("
      Shiny.addCustomMessageHandler('dragmapr-labels', function(message) {
        var iframe = document.querySelector('iframe');
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-labels', labels: !!message.labels}, '*'
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-reset-state', function(message) {
        var iframe = document.querySelector('iframe');
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage({type: 'dragmapr-reset-state'}, '*');
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-label-data', function(message) {
        var iframe = document.querySelector('iframe');
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-label-data', labels: message.labels || []}, '*'
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-label-options', function(message) {
        var iframe = document.querySelector('iframe');
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-label-options', options: message.options || {}}, '*'
          );
        }
      });
    ")),
    tags$style(HTML(studio_css)),
    # Progress bar — shown automatically whenever Shiny is busy
    tags$div(class = "studio-progress-bar")
  ),

  tags$h2("dragmapr spatial studio", class = "studio-title"),
  tags$p("Upload polygon data or enter a URL, drag the layout, then export.",
         class = "studio-subtitle"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$div(
        class = "studio-card",
        tags$div("Data source", class = "studio-section"),
        fileInput(
          "spatial_upload", "Upload spatial file",
          multiple = TRUE,
          accept = c(".zip", ".shp", ".dbf", ".shx", ".prj", ".cpg", ".gpkg", ".geojson", ".json")
        ),
        actionButton("load_demo", "Use bundled HHS demo", class = "btn-sm btn-default"),
        tags$br(), tags$br(),
        textInput("spatial_url", "Or load from URL",
                  placeholder = "https://example.com/regions.geojson"),
        actionButton("load_url", "Load URL", class = "btn-sm btn-primary"),
        actionButton("reset_layout", "Reset drag layout", class = "btn-sm btn-warning")
      ),

      tags$div(
        class = "studio-card",
        tags$div("Columns & colors", class = "studio-section"),
        uiOutput("column_controls"),
        uiOutput("color_pickers"),
        checkboxInput("show_legend", "Show legend in preview", value = TRUE),
        checkboxInput("legend_show_all", "Show all legend keys", value = FALSE),
        selectInput(
          "legend_position",
          "Legend position",
          choices = c("Bottom" = "bottom", "Top" = "top", "Left" = "left", "Right" = "right", "None" = "none"),
          selected = "bottom"
        )
      ),

      tags$div(
        class = "studio-card",
        tags$div("Labels", class = "studio-section"),
        checkboxInput("show_labels", "Show labels", value = TRUE),
        uiOutput("label_filter_ui"),
        uiOutput("label_color_ui"),
        radioButtons(
          "annotation_mode", "Annotation style",
          choices  = c("Short labels" = "labels", "Info boxes" = "boxes"),
          selected = "labels"
        ),
        checkboxInput("show_label_marker", "Circle behind text labels", value = TRUE),
        sliderInput("box_width",  "Info box width",  min = 110, max = 280, value = 170, step = 5),
        sliderInput("box_height", "Info box height", min = 48,  max = 150, value = 76,  step = 4)
      ),

      tags$div(
        class = "studio-card",
        tags$div("Connectors", class = "studio-section"),
        checkboxInput("show_connectors", "Show connector lines", value = FALSE),
        radioButtons(
          "connector_type", "Connector style",
          choices  = c("Straight" = "straight", "Elbow" = "elbow",
                       "Curve" = "curve", "Squiggle" = "squiggle"),
          selected = "straight"
        ),
        sliderInput("connector_linewidth", "Connector thickness",
                    min = 0.25, max = 2.5, value = 0.45, step = 0.05)
      ),

      tags$div(
        class = "studio-card",
        tags$div("Export", class = "studio-section"),
        tags$div(
          class = "download-grid",
          downloadButton("download_png",        "PNG"),
          downloadButton("download_region_csv", "Region CSV"),
          downloadButton("download_label_csv",  "Label CSV"),
          downloadButton("download_geojson",    "GeoJSON"),
          downloadButton("download_gpkg",       "GPKG"),
          downloadButton("download_html",       "HTML helper"),
          downloadButton("download_bundle",     "Bundle ZIP")
        )
      )
    ),

    mainPanel(
      width = 9,
      uiOutput("status_bar"),
      uiOutput("studio_overlay_ui"),
      tabsetPanel(
        tabPanel(
          "Drag",
          tags$div(
            class = "helper-wrap",
            uiOutput("helper"),
            uiOutput("helper_overlay_ui")
          )
        ),
        tabPanel(
          "Preview",
          div(
            style = "display: flex; gap: 12px; align-items: center; margin: 10px 0;",
            actionButton("refresh_preview", "Refresh preview", class = "btn-sm btn-default"),
            checkboxInput("auto_preview", "Auto-refresh preview", value = FALSE)
          ),
          plotOutput("preview", height = 620)
        ),
        tabPanel(
          "State",
          tags$h4("Key reactives"),
          tags$pre(paste(
            "source_sf()       raw sf from upload / URL / demo",
            "projected_sf()    after prepare_dragmapr_sf()",
            "region_col()      chosen grouping column",
            "label_col()       chosen label column",
            "label_table()     make_region_labels() + style flags",
            "region_state()    current region offsets",
            "label_state()     current label offsets",
            "region_palette()  named color vector",
            "current_plot()    ggplot2 object from render_dragged_map()",
            sep = "\n"
          )),
          tags$h4("Region offsets"),
          verbatimTextOutput("region_state_text", placeholder = TRUE),
          tags$h4("Label offsets"),
          verbatimTextOutput("label_state_text", placeholder = TRUE)
        )
      )
    )
  )
)

# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  state <- reactiveValues(
    source          = example_hhs_layout()$states,
    status          = "Using bundled HHS demo. Upload a file or enter a URL to replace it.",
    status_level    = "info",    # "info" | "ok" | "error"
    helper_token    = 0L,
    helper_building = FALSE,     # TRUE while drag_map_prototype() is running
    ingesting       = FALSE,
    region_csv_cache = NULL,
    label_csv_cache  = NULL
  )

  helper_dir <- tempfile("dragmapr_spatial_studio_")
  dir.create(helper_dir, recursive = TRUE)
  shiny::addResourcePath("dragmapr_spatial_studio", helper_dir)
  helper_file <- file.path(helper_dir, "studio_helper.html")

  set_status <- function(msg, level = "info") {
    state$status       <- msg
    state$status_level <- level
  }

  rows_for_message <- function(x) {
    jsonlite::fromJSON(
      jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }

  # Called after any successful data load to warn about high-cardinality columns.
  post_load_hints <- function(x, source_label) {
    n_feat <- nrow(x)
    # Count unique values in each non-geometry column to flag obvious issues.
    col_cards <- vapply(safe_names(x), function(col) {
      length(unique(stats::na.omit(as.character(x[[col]]))))
    }, integer(1L))
    if (length(col_cards) == 0L) {
      set_status(paste0("Loaded ", n_feat, " features from ", source_label, "."), "ok")
      return()
    }
    best_col  <- names(which.min(col_cards))
    n_best    <- col_cards[[best_col]]

    if (n_best > LEGEND_THRESHOLD) {
      set_status(
        paste0(
          "Loaded ", n_feat, " features from ", source_label, ". ",
          "Lowest-cardinality column is ‘", best_col, "’ with ", n_best, " unique values. ",
          "Choose a column with fewer unique values for clearest results. ",
          "Legend is auto-hidden above ", LEGEND_THRESHOLD, " groups."
        ),
        "info"
      )
    } else {
      set_status(
        paste0("Loaded ", n_feat, " features from ", source_label, "."),
        "ok"
      )
    }
  }

  # ---- Data loading ----

  observeEvent(input$load_demo, {
    state$ingesting <- TRUE
    on.exit({ state$ingesting <- FALSE }, add = TRUE)
    x <- example_hhs_layout()$states
    state$source <- x
    state$region_csv_cache <- NULL
    state$label_csv_cache <- NULL
    post_load_hints(x, "bundled HHS demo")
  })

  observeEvent(input$spatial_upload, {
    state$ingesting <- TRUE
    on.exit({ state$ingesting <- FALSE }, add = TRUE)
    tryCatch({
      x <- read_dragmapr_sf_upload(input$spatial_upload)
      if (!is.null(x)) {
        state$source <- x
        state$region_csv_cache <- NULL
        state$label_csv_cache <- NULL
        post_load_hints(x, "upload")
      }
    }, error = function(e) {
      set_status(paste("Upload error:", conditionMessage(e)), "error")
    })
  })

  observeEvent(input$load_url, {
    url <- trimws(input$spatial_url %||% "")
    if (!nzchar(url)) {
      set_status("Enter a URL before clicking Load URL.", "error")
      return()
    }
    state$ingesting <- TRUE
    on.exit({ state$ingesting <- FALSE }, add = TRUE)
    set_status("Downloading…", "info")
    tryCatch({
      x <- read_dragmapr_sf_url(url)
      state$source <- x
      state$region_csv_cache <- NULL
      state$label_csv_cache <- NULL
      post_load_hints(x, "URL")
    }, error = function(e) {
      set_status(paste("URL error:", conditionMessage(e)), "error")
    })
  })

  # ---- Reactives ----

  source_sf <- reactive(state$source)

  projected_sf <- reactive({
    tryCatch(
      prepare_dragmapr_sf(source_sf()),
      error = function(e) {
        set_status(paste("Projection error:", conditionMessage(e)), "error")
        source_sf()
      }
    )
  })

  available_columns <- reactive(safe_names(projected_sf()))

  region_col <- reactive({
    cols <- available_columns()
    req(length(cols) > 0L)
    choose_column(input$region_col, cols)
  })

  label_col <- reactive({
    cols <- available_columns()
    req(length(cols) > 0L)
    choose_column(input$label_col, cols, fallback = region_col())
  })

  # Returns sorted unique groups for the current region column
  region_groups <- reactive({
    req(region_col())
    stable_unique(projected_sf()[[region_col()]])
  })

  region_palette <- reactive({
    req(region_col())
    groups <- region_groups()
    n      <- length(groups)

    if (n > CPICKER_THRESHOLD) {
      # Large mode: read cycle-palette swatches (or text input fallback)
      if (requireNamespace("shinyWidgets", quietly = TRUE)) {
        cycle <- vapply(seq_along(default_palette), function(i) {
          val <- input[[paste0("cycle_color_", i)]]
          if (!is.null(val) && nzchar(val)) val else default_palette[i]
        }, character(1L))
      } else {
        cycle_text <- input$palette_text %||% ""
        cycle <- if (nzchar(cycle_text)) {
          cols <- trimws(strsplit(cycle_text, ",", fixed = TRUE)[[1]])
          cols[nzchar(cols)]
        } else {
          default_palette
        }
        if (length(cycle) == 0L) cycle <- default_palette
      }
      return(stats::setNames(
        rep(cycle, length.out = n),
        groups
      ))
    }

    # Small mode: one picker per region (or text input fallback)
    pal <- stats::setNames(rep(default_palette, length.out = n), groups)
    if (requireNamespace("shinyWidgets", quietly = TRUE)) {
      for (i in seq_along(groups)) {
        val <- input[[paste0("cpicker_", i)]]
        if (!is.null(val) && nzchar(val)) pal[[groups[i]]] <- val
      }
    } else {
      pal <- palette_for(groups, input$palette_text)
    }
    pal
  })

  all_label_table <- reactive({
    req(region_col(), label_col())
    labels <- make_region_labels(projected_sf(), region_col = region_col(), label_col = label_col())
    label_levels <- stable_unique(labels$label)
    region_levels <- region_groups()
    labels$label <- factor(as.character(labels$label), levels = label_levels, ordered = TRUE)
    labels$region <- factor(as.character(labels$region), levels = region_levels, ordered = TRUE)
    labels[order(labels$label, labels$region, labels$label_id), , drop = FALSE]
  })

  visible_label_ids <- reactive({
    ids <- all_label_table()$label_id
    selected <- input$label_filter
    if (is.null(selected)) {
      ids
    } else {
      intersect(as.character(selected), ids)
    }
  })

  label_palette <- reactive({
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    pal <- stats::setNames(rep("#111827", length(ids)), ids)
    if (length(ids) == 0L) {
      return(pal)
    }

    if (requireNamespace("shinyWidgets", quietly = TRUE)) {
      if (length(ids) > CPICKER_THRESHOLD) {
        for (i in seq_along(ids)) {
          cycle_index <- ((i - 1L) %% length(default_label_colors)) + 1L
          val <- input[[paste0("label_cycle_color_", cycle_index)]]
          if (is.null(val)) {
            val <- default_label_colors[cycle_index]
          }
          if (valid_hex_color(val)) {
            pal[[ids[i]]] <- val
          }
        }
        return(pal)
      }
      for (i in seq_along(ids)) {
        id <- ids[i]
        val <- input[[label_color_input_id(id)]]
        if (is.null(val)) {
          val <- default_label_colors[(i - 1L) %% length(default_label_colors) + 1L]
        }
        if (valid_hex_color(val)) {
          pal[[id]] <- val
        }
      }
      return(pal)
    }

    text <- input$label_palette_text %||% ""
    if (nzchar(text)) {
      colors <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
      colors <- colors[vapply(colors, valid_hex_color, logical(1))]
      if (length(colors) > 0L) {
        pal[] <- rep(colors, length.out = length(ids))
      }
    }
    pal
  })

  label_table <- reactive({
    req(region_col(), label_col())
    labels <- make_studio_labels(
      projected_sf(),
      region_col     = region_col(),
      label_col      = label_col(),
      mode           = input$annotation_mode %||% "labels",
      width_px       = input$box_width       %||% 170,
      height_px      = input$box_height      %||% 76,
      connector      = isTRUE(input$show_connectors),
      connector_type = input$connector_type  %||% "straight"
    )
    labels <- labels[labels$label_id %in% visible_label_ids(), , drop = FALSE]
    labels$label_color <- unname(label_palette()[as.character(labels$label_id)])
    labels$label_color[is.na(labels$label_color)] <- "#111827"
    labels
  })

  region_csv_raw <- debounce(reactive(input$region_csv), 250)
  label_csv_raw  <- debounce(reactive(input$label_csv),  250)

  observeEvent(region_csv_raw(), {
    if (!is.null(region_csv_raw()) && nzchar(region_csv_raw())) {
      state$region_csv_cache <- region_csv_raw()
    }
  }, ignoreInit = TRUE)

  observeEvent(label_csv_raw(), {
    if (!is.null(label_csv_raw()) && nzchar(label_csv_raw())) {
      state$label_csv_cache <- label_csv_raw()
    }
  }, ignoreInit = TRUE)

  region_state <- reactive({
    read_csv_text(state$region_csv_cache) %||% read_csv_text(region_csv_raw()) %||% data.frame(
      region = region_groups(), dx_m = 0, dy_m = 0,
      stringsAsFactors = FALSE
    )
  })

  label_state <- reactive({
    read_csv_text(state$label_csv_cache) %||% read_csv_text(label_csv_raw()) %||% empty_label_offsets(label_table())
  })

  build_plot <- function(region_offsets, label_offsets) {
    # Suppress the legend when there are too many groups — the studio handles
    # this directly so it works regardless of which package version is installed.
    max_keys <- if (isTRUE(input$legend_show_all)) Inf else LEGEND_THRESHOLD
    render_dragged_map(
      projected_sf(),
      region_offsets      = region_offsets,
      region_col          = region_col(),
      labels              = if (isTRUE(input$show_labels)) label_table() else FALSE,
      label_offsets       = label_offsets,
      region_palette      = region_palette(),
      show_legend         = isTRUE(input$show_legend),
      max_legend_keys     = max_keys,
      legend_position     = input$legend_position %||% "bottom",
      show_label_marker   = isTRUE(input$show_label_marker),
      connector_linewidth = input$connector_linewidth %||% 0.45,
      label_padding       = 0.12,
      title               = "dragmapr spatial studio"
    )
  }

  current_plot <- reactive({
    build_plot(region_state(), label_state())
  })

  # ---- Preview refresh logic ----

  preview_token <- reactiveVal(0L)
  preview_snapshot <- reactiveVal(NULL)
  do_refresh <- function() {
    preview_snapshot(list(
      region_offsets = isolate(region_state()),
      label_offsets  = isolate(label_state())
    ))
    preview_token(isolate(preview_token()) + 1L)
  }

  observeEvent(input$refresh_preview, do_refresh(), ignoreInit = TRUE)

  observeEvent(
    list(projected_sf(), region_col(), label_col(), region_palette(), label_table(),
         input$show_labels, input$show_legend, input$show_label_marker,
         input$connector_linewidth, input$legend_show_all, input$legend_position),
    do_refresh(),
    ignoreInit = FALSE
  )

  observeEvent(list(region_csv_raw(), label_csv_raw()), {
    if (isTRUE(input$auto_preview)) do_refresh()
  }, ignoreInit = TRUE)

  preview_plot <- reactive({
    preview_token()
    snapshot <- preview_snapshot()
    if (is.null(snapshot)) {
      snapshot <- list(region_offsets = region_state(), label_offsets = label_state())
    }
    build_plot(snapshot$region_offsets, snapshot$label_offsets)
  })

  adjusted_sf <- reactive({
    apply_offsets(projected_sf(), region_state(), region_col = region_col())
  })

  # ---- UI outputs ----

  output$status_bar <- renderUI({
    level <- state$status_level %||% "info"
    icon  <- switch(level, ok = "✓", error = "⚠️", "ℹ️")
    tags$div(
      class = paste("studio-status", level),
      tags$span(icon, class = "status-icon"),
      tags$span(state$status)
    )
  })

  output$studio_overlay_ui <- renderUI({
    if (!isTRUE(state$ingesting)) return(NULL)
    tags$div(
      class = "helper-overlay",
      style = "position: fixed; z-index: 9998;",
      tags$div(class = "studio-spinner"),
      tags$p("Reading spatial data...")
    )
  })

  output$column_controls <- renderUI({
    cols <- available_columns()
    if (length(cols) == 0L) return(tags$p("No non-geometry columns found.", style = "color:#6b7280; font-size:0.83rem;"))
    default_col <- if ("hhs_region" %in% cols) "hhs_region" else cols[1]
    tagList(
      selectInput("region_col", "Group / region column", choices = cols, selected = default_col),
      selectInput("label_col",  "Label column",          choices = cols, selected = default_col)
    )
  })

  output$color_pickers <- renderUI({
    req(region_col())
    groups <- region_groups()
    n      <- length(groups)
    pal    <- rep(default_palette, length.out = n)

    # ── Large cardinality: compact cycle-palette editor ──────────────────────
    # With many groups individual pickers are unusable. Instead let the user
    # edit the base palette (≤10 colors) that cycles across all groups.
    if (n > CPICKER_THRESHOLD) {
      cycle_colors <- paste(default_palette, collapse = ", ")
      header <- tags$p(
        style = "font-size:0.78rem; color:#6b7280; margin:2px 0 4px;",
        paste0(n, " groups — colors cycle through the palette below.")
      )
      if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
        return(tagList(
          header,
          textInput("palette_text", "Cycle palette (hex, comma-separated)",
                    value = cycle_colors)
        ))
      }
      # Show 10 compact swatches for the cycle base
      swatch_pickers <- lapply(seq_along(default_palette), function(i) {
        tags$div(
          class = "cpicker-row",
          tags$span(paste0("Color ", i), class = "cpicker-label"),
          shinyWidgets::colorPickr(
            inputId  = paste0("cycle_color_", i),
            label    = NULL,
            selected = default_palette[i],
            theme    = "nano",
            update   = "save",
            inline   = FALSE,
            swatches = default_palette,
            width    = "28px"
          )
        )
      })
      return(tagList(
        header,
        tags$div(class = "cpicker-grid", swatch_pickers)
      ))
    }

    # ── Small cardinality: one picker per region ──────────────────────────────
    if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
      return(textInput(
        "palette_text", "Palette (hex codes, comma-separated)",
        value = paste(pal, collapse = ", ")
      ))
    }
    pickers <- lapply(seq_along(groups), function(i) {
      tags$div(
        class = "cpicker-row",
        tags$span(groups[i], class = "cpicker-label", title = groups[i]),
        shinyWidgets::colorPickr(
          inputId  = paste0("cpicker_", i),
          label    = NULL,
          selected = pal[i],
          theme    = "nano",
          update   = "save",
          inline   = FALSE,
          swatches = default_palette,
          width    = "28px"
        )
      )
    })
    tagList(
      tags$p("Region colors",
             style = "font-size:0.78rem; color:#6b7280; margin:2px 0 6px;"),
      tags$div(class = "cpicker-grid", pickers)
    )
  })

  output$label_filter_ui <- renderUI({
    labels <- all_label_table()
    choices <- stats::setNames(
      as.character(labels$label_id),
      paste0(as.character(labels$label), " (", as.character(labels$region), ")")
    )
    selected <- isolate(input$label_filter)
    if (is.null(selected)) {
      selected <- unname(choices)
    } else {
      selected <- intersect(as.character(selected), unname(choices))
    }

    if (requireNamespace("glasstabs", quietly = TRUE)) {
      return(tagList(
        glasstabs::useGlassTabs(),
        glasstabs::glassMultiSelect(
          inputId = "label_filter",
          choices = choices,
          selected = selected,
          label = "Visible labels",
          placeholder = "Choose labels to show",
          all_label = "All labels",
          check_style = "checkbox",
          show_style_switcher = FALSE,
          show_select_all = TRUE,
          show_clear_all = TRUE,
          theme = "light"
        )
      ))
    }

    selectizeInput(
      "label_filter",
      "Visible labels",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        placeholder = "Choose labels to show"
      )
    )
  })

  output$label_color_ui <- renderUI({
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    n <- length(ids)

    if (n == 0L) {
      return(NULL)
    }

    if (n > CPICKER_THRESHOLD) {
      header <- tags$p(
        style = "font-size:0.78rem; color:#6b7280; margin:2px 0 4px;",
        paste0(n, " labels - colors cycle through the palette below.")
      )
      if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
        return(tagList(
          header,
          textInput(
            "label_palette_text",
            "Label palette (hex, comma-separated)",
            value = paste(default_label_colors, collapse = ", ")
          )
        ))
      }
      swatch_pickers <- lapply(seq_along(default_label_colors), function(i) {
        tags$div(
          class = "cpicker-row",
          tags$span(paste0("Label ", i), class = "cpicker-label"),
          shinyWidgets::colorPickr(
            inputId  = paste0("label_cycle_color_", i),
            label    = NULL,
            selected = default_label_colors[i],
            theme    = "nano",
            update   = "save",
            inline   = FALSE,
            swatches = default_label_colors,
            width    = "28px"
          )
        )
      })
      return(tagList(
        header,
        tags$div(class = "cpicker-grid", swatch_pickers)
      ))
    }

    if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
      return(textInput(
        "label_palette_text",
        "Label palette (hex, comma-separated)",
        value = paste(rep(default_label_colors, length.out = n), collapse = ", ")
      ))
    }

    pickers <- lapply(seq_along(ids), function(i) {
      tags$div(
        class = "cpicker-row",
        tags$span(
          paste0(as.character(labels$label[i]), " (", as.character(labels$region[i]), ")"),
          class = "cpicker-label",
          title = paste0(as.character(labels$label[i]), " (", as.character(labels$region[i]), ")")
        ),
        shinyWidgets::colorPickr(
          inputId  = label_color_input_id(ids[i]),
          label    = NULL,
          selected = default_label_colors[(i - 1L) %% length(default_label_colors) + 1L],
          theme    = "nano",
          update   = "save",
          inline   = FALSE,
          swatches = default_label_colors,
          width    = "28px"
        )
      )
    })

    tagList(
      tags$p("Label colors",
             style = "font-size:0.78rem; color:#6b7280; margin:2px 0 6px;"),
      tags$div(class = "cpicker-grid", pickers)
    )
  })

  observeEvent(available_columns(), {
    cols <- available_columns()
    if (length(cols) == 0L) return()
    default_col <- if ("hhs_region" %in% cols) "hhs_region" else cols[1]
    if (is.null(input$region_col) || !input$region_col %in% cols) {
      updateSelectInput(session, "region_col", choices = cols, selected = default_col)
    }
    if (is.null(input$label_col) || !input$label_col %in% cols) {
      updateSelectInput(session, "label_col", choices = cols, selected = default_col)
    }
  }, ignoreInit = FALSE)

  # Rebuild helper HTML for data, grouping, label column, or palette changes.
  # Label-only style changes are sent to the existing iframe with postMessage
  # so changing connector style does not reset the drag state.
  observeEvent(
    list(projected_sf(), region_col(), label_col(), region_palette()),
    {
      state$helper_building <- TRUE
      tryCatch({
        drag_map_prototype(
          projected_sf(),
          region_col          = region_col(),
          label_col           = label_col(),
          labels              = label_table(),
          label_marker        = isTRUE(input$show_label_marker),
          label_box_width     = input$box_width  %||% 170,
          label_box_height    = input$box_height %||% 76,
          connector_linewidth = (input$connector_linewidth %||% 0.45) * 3,
          region_offsets      = isolate(region_state()),
          label_offsets       = isolate(label_state()),
          region_palette      = region_palette(),
          file                = helper_file
        )
        state$helper_token    <- state$helper_token + 1L
        state$helper_building <- FALSE
      }, error = function(e) {
        state$helper_building <- FALSE
        set_status(paste("Helper error:", conditionMessage(e)), "error")
      })
    },
    ignoreInit = FALSE
  )

  observeEvent(input$show_labels, {
    session$sendCustomMessage("dragmapr-labels", list(labels = isTRUE(input$show_labels)))
  }, ignoreInit = FALSE)

  observeEvent(label_table(), {
    session$sendCustomMessage("dragmapr-label-data", list(labels = rows_for_message(label_table())))
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$show_label_marker, input$box_width, input$box_height, input$connector_linewidth),
    {
      session$sendCustomMessage("dragmapr-label-options", list(options = list(
        labelMarker = isTRUE(input$show_label_marker),
        labelBoxWidth = input$box_width %||% 170,
        labelBoxHeight = input$box_height %||% 76,
        connectorLinewidth = (input$connector_linewidth %||% 0.45) * 3
      )))
    },
    ignoreInit = TRUE
  )

  observeEvent(input$reset_layout, {
    zero_regions <- data.frame(
      region = region_groups(), dx_m = 0, dy_m = 0,
      stringsAsFactors = FALSE
    )
    zero_labels <- empty_label_offsets(label_table())
    state$region_csv_cache <- paste(capture.output(utils::write.csv(zero_regions, row.names = FALSE)), collapse = "\n")
    state$label_csv_cache <- paste(capture.output(utils::write.csv(zero_labels, row.names = FALSE)), collapse = "\n")
    session$sendCustomMessage("dragmapr-reset-state", list())
    do_refresh()
    set_status("Reset region and label offsets.", "ok")
  }, ignoreInit = TRUE)

  output$helper <- renderUI({
    state$helper_token
    tags$iframe(
      src   = paste0("dragmapr_spatial_studio/studio_helper.html?v=", state$helper_token),
      style = "width: 100%; height: 780px; border: 1px solid #e5e7eb; border-radius: 6px;"
    )
  })

  output$helper_overlay_ui <- renderUI({
    if (!isTRUE(state$helper_building)) return(NULL)
    tags$div(
      class = "helper-overlay",
      tags$div(class = "studio-spinner"),
      tags$p("Building interactive map…")
    )
  })

  output$preview           <- renderPlot(preview_plot())
  output$region_state_text <- renderText(region_csv_raw() %||% "Waiting for drag state…")
  output$label_state_text  <- renderText(label_csv_raw()  %||% "Waiting for drag state…")

  # ---- Download handlers ----

  output$download_png <- downloadHandler(
    filename    = function() "dragmapr-spatial-studio.png",
    content     = function(file) ggplot2::ggsave(file, current_plot(), width = 10, height = 8, dpi = 300),
    contentType = "image/png"
  )
  output$download_region_csv <- downloadHandler(
    filename    = function() "drag_region_offsets.csv",
    content     = function(file) utils::write.csv(region_state(), file, row.names = FALSE),
    contentType = "text/csv"
  )
  output$download_label_csv <- downloadHandler(
    filename    = function() "drag_label_offsets.csv",
    content     = function(file) utils::write.csv(label_state(), file, row.names = FALSE),
    contentType = "text/csv"
  )
  output$download_geojson <- downloadHandler(
    filename    = function() "dragmapr-adjusted.geojson",
    content     = function(file) {
      sf::st_write(adjusted_sf(), file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    },
    contentType = "application/geo+json"
  )
  output$download_gpkg <- downloadHandler(
    filename    = function() "dragmapr-adjusted.gpkg",
    content     = function(file) {
      sf::st_write(adjusted_sf(), file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    },
    contentType = "application/octet-stream"
  )
  output$download_html <- downloadHandler(
    filename    = function() "dragmapr-helper.html",
    content     = function(file) file.copy(helper_file, file, overwrite = TRUE),
    contentType = "text/html"
  )
  output$download_bundle <- downloadHandler(
    filename    = "dragmapr-project.zip",
    contentType = "application/zip",
    content     = function(file) {
      bundle_dir <- tempfile("dragmapr_bundle_")
      dir.create(bundle_dir)
      sf::st_write(projected_sf(),
                   file.path(bundle_dir, "source.gpkg"),
                   driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      utils::write.csv(region_state(),
                       file.path(bundle_dir, "drag_region_offsets.csv"),
                       row.names = FALSE)
      utils::write.csv(label_state(),
                       file.path(bundle_dir, "drag_label_offsets.csv"),
                       row.names = FALSE)
      utils::write.csv(
        data.frame(region = names(region_palette()),
                   color  = unname(region_palette()),
                   stringsAsFactors = FALSE),
        file.path(bundle_dir, "palette.csv"),
        row.names = FALSE
      )
      writeLines(
        jsonlite::toJSON(list(
          region_col       = region_col(),
          label_col        = label_col(),
          crs_epsg         = sf::st_crs(projected_sf())$epsg,
          created          = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          dragmapr_version = as.character(utils::packageVersion("dragmapr"))
        ), auto_unbox = TRUE, pretty = TRUE),
        file.path(bundle_dir, "metadata.json")
      )
      old_wd <- setwd(bundle_dir)
      on.exit(setwd(old_wd), add = TRUE)
      utils::zip(file, list.files(".", recursive = TRUE))
    }
  )
}

shinyApp(ui, server)
