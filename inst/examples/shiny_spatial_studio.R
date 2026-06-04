if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install shiny to run this example.", call. = FALSE)
}

options(shiny.maxRequestSize = max(getOption("shiny.maxRequestSize", 5 * 1024^2), 100 * 1024^2))

library(dragmapr)
library(shiny)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

studio_select <- function(inputId, label, choices, selected = NULL,
                          placeholder = "Select an option", clearable = FALSE) {
  if (requireNamespace("glasstabs", quietly = TRUE)) {
    return(tagList(
      glasstabs::useGlassTabs(),
      glasstabs::glassSelect(
        inputId = inputId,
        choices = choices,
        selected = selected,
        label = label,
        placeholder = placeholder,
        searchable = TRUE,
        clearable = clearable,
        theme = "light"
      )
    ))
  }
  selectInput(inputId, label, choices = choices, selected = selected)
}

studio_multi_select <- function(inputId, label, choices, selected = NULL,
                                placeholder = "Choose values",
                                all_label = "All values") {
  if (requireNamespace("glasstabs", quietly = TRUE)) {
    return(tagList(
      glasstabs::useGlassTabs(),
      glasstabs::glassMultiSelect(
        inputId = inputId,
        choices = choices,
        selected = selected,
        label = label,
        placeholder = placeholder,
        all_label = all_label,
        check_style = "checkbox",
        show_style_switcher = FALSE,
        show_select_all = TRUE,
        show_clear_all = TRUE,
        theme = "light"
      )
    ))
  }
  selectizeInput(
    inputId,
    label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    options = list(
      plugins = list("remove_button"),
      placeholder = placeholder
    )
  )
}

read_csv_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  utils::read.csv(text = text, stringsAsFactors = FALSE, check.names = FALSE)
}

csv_text <- function(x) {
  paste(capture.output(utils::write.csv(x, row.names = FALSE)), collapse = "\n")
}

is_blank <- function(x) is.null(x) || length(x) == 0L || !nzchar(trimws(x[1]))

valid_hex_color <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
}

# Legend is suppressed automatically in render_dragged_map() above this count.
LEGEND_THRESHOLD  <- 25L

default_palette <- c(
  "#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756",
  "#72B7B2", "#EECA3B", "#B74F6F", "#8CD17D", "#79706E"
)

default_label_colors <- c("#111827", "#1D4ED8", "#047857", "#B91C1C", "#7C3AED", "#B45309")

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

apply_label_edits <- function(labels, edits) {
  if (is.null(edits) || length(edits) == 0L || nrow(labels) == 0L) {
    return(labels)
  }
  labels$label <- as.character(labels$label)
  ids <- as.character(labels$label_id)
  for (id in intersect(names(edits), ids)) {
    value <- edits[[id]]
    if (!is.null(value)) {
      labels$label[ids == id] <- value
    }
  }
  labels
}

read_dragmapr_project_upload <- function(upload) {
  if (is.null(upload) || nrow(upload) == 0L) return(NULL)
  file <- upload$datapath[1]
  exdir <- tempfile("dragmapr_project_")
  dir.create(exdir, recursive = TRUE)
  utils::unzip(file, exdir = exdir)

  source_file <- file.path(exdir, "source.gpkg")
  if (!file.exists(source_file)) {
    stop("Project ZIP is missing source.gpkg.", call. = FALSE)
  }

  read_optional_csv <- function(name) {
    path <- file.path(exdir, name)
    if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      NULL
    }
  }

  metadata_file <- file.path(exdir, "metadata.json")
  metadata <- if (file.exists(metadata_file)) {
    jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  } else {
    list()
  }

  list(
    source         = sf::st_read(source_file, quiet = TRUE),
    region_offsets = read_optional_csv("drag_region_offsets.csv"),
    label_offsets  = read_optional_csv("drag_label_offsets.csv"),
    palette        = read_optional_csv("palette.csv"),
    labels         = read_optional_csv("labels.csv"),
    metadata       = metadata
  )
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
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: #f8fafc;
  color: #172033;
}
.container-fluid {
  max-width: 1920px;
  padding: 12px 14px 24px;
}
.well {
  background: #ffffff;
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(15, 23, 42, 0.06);
  height: calc(86vh + 172px);
  min-height: 892px;
  max-height: 1132px;
  overflow-y: auto;
  padding: 12px;
}
@media (max-width: 767px) {
  .well {
    height: auto;
    min-height: 0;
    max-height: none;
  }
}
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
.studio-card .selectize-control { margin-bottom: 0; }
.studio-card .irs { margin-top: -4px; }

/* ---- Compact slider sub-group ---- */
.slider-group-label {
  font-size: 0.72rem;
  font-weight: 600;
  color: #6b7280;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  margin: 10px 0 0;
  padding-top: 8px;
  border-top: 1px dashed #e5e7eb;
}
.slider-pair {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0 10px;
}
.slider-pair .form-group { margin-bottom: 6px; }
.slider-pair .control-label { font-size: 0.75rem; }
.studio-action-row {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  margin-top: 6px;
}
.studio-action-row .btn { margin-bottom: 0; }

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

/* ---- Main workspace controls ---- */
.map-control-toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  padding: 8px 10px;
  margin-bottom: 8px;
  border: 1px solid #d9e0ea;
  border-radius: 8px;
  background: #ffffff;
  box-shadow: 0 1px 3px rgba(15, 23, 42, 0.05);
}
.map-control-title {
  font-size: 0.76rem;
  font-weight: 700;
  color: #475569;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  white-space: nowrap;
}
.map-control-actions {
  display: flex;
  flex-wrap: wrap;
  justify-content: flex-end;
  gap: 6px;
}
.map-control-actions .btn {
  margin-bottom: 0;
}

/* ---- Blocking load veil ---- */
.studio-load-veil {
  position: fixed;
  inset: 0;
  z-index: 9998;
  display: none;
  align-items: center;
  justify-content: center;
  background: rgba(248, 250, 252, 0.78);
  backdrop-filter: blur(3px);
}
body.dragmapr-loading .studio-load-veil { display: flex; }
.studio-load-panel {
  display: flex;
  align-items: center;
  gap: 12px;
  min-width: 260px;
  max-width: 360px;
  padding: 14px 16px;
  border: 1px solid #dbe3ee;
  border-radius: 8px;
  background: #ffffff;
  box-shadow: 0 12px 32px rgba(15, 23, 42, 0.16);
}
.studio-load-panel strong {
  display: block;
  color: #172033;
  font-size: 0.9rem;
}
.studio-load-panel span {
  display: block;
  color: #64748b;
  font-size: 0.78rem;
  margin-top: 2px;
}

/* ---- Color picker rows ---- */
.studio-help {
  font-size: 0.78rem;
  color: #64748b;
  line-height: 1.35;
  margin: 2px 0 8px;
}
.studio-divider {
  border-top: 1px dashed #e5e7eb;
  margin: 12px 0 10px;
}
.studio-field-gap {
  height: 8px;
}
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
.region-color-editor {
  display: grid;
  grid-template-columns: minmax(0, 1fr) auto;
  align-items: end;
  gap: 8px;
}
.region-color-editor .form-group { margin-bottom: 0; }
.region-color-editor .pcr-button {
  width: 34px !important;
  height: 34px !important;
  border-radius: 6px !important;
}

/* ---- Download grid ---- */
.download-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
  margin-top: 8px;
}
.download-grid .btn { width: 100%; font-size: 0.78rem; padding: 5px 8px; }

/* ---- Main workspace ---- */
.tabbable > .nav-tabs {
  border-bottom: 1px solid #d9e0ea;
  margin-bottom: 0;
}
.tabbable > .nav-tabs > li > a {
  border-radius: 6px 6px 0 0;
  color: #475569;
  font-weight: 600;
}
.tab-content {
  background: #ffffff;
  border: 1px solid #d9e0ea;
  border-top: 0;
  border-radius: 0 0 8px 8px;
  padding: 12px;
  box-shadow: 0 1px 3px rgba(15, 23, 42, 0.05);
}
.preview-toolbar {
  display: flex;
  flex-wrap: wrap;
  gap: 12px;
  align-items: center;
  margin: 0 0 10px;
}
.preview-toolbar .checkbox { margin: 0; }
.studio-helper-frame {
  width: 100%;
  height: min(86vh, 960px);
  min-height: 720px;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  background: #ffffff;
}

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
  background: rgba(255, 255, 255, 0.72);
  backdrop-filter: blur(1.5px);
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 14px;
  z-index: 10;
  border-radius: 6px;
  opacity: 0;
  pointer-events: none;
  transition: opacity 0.16s ease;
}
.helper-wrap.is-updating .helper-overlay {
  opacity: 1;
  pointer-events: auto;
}
body.dragmapr-helper-busy .well,
body.dragmapr-helper-busy .map-control-toolbar,
body.dragmapr-helper-busy .nav-tabs {
  pointer-events: none;
  opacity: 0.68;
  transition: opacity 0.16s ease;
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
    tags$script(HTML(dragmapr_iframe_bridge(iframe_selector = "iframe.studio-helper-frame"))),
    # Label-toggle message handler (app-specific, stays inline)
    tags$script(HTML("
      // Use a specific selector so Shiny's own internal iframes (download
      // handlers, etc.) never intercept messages meant for the map helper.
      function getHelperFrame() {
        return document.querySelector('iframe.studio-helper-frame');
      }
      function helperTargetOrigin() {
        var iframe = getHelperFrame();
        if (!iframe || !iframe.src) return window.location.origin;
        try {
          return new URL(iframe.src, window.location.href).origin;
        } catch (e) {
          return window.location.origin;
        }
      }
      var helperState = {
        fullLoading: false,
        activeGeneration: null,
        readyGeneration: null,
        fallbackTimer: null
      };
      function helperGeneration(value) {
        return value == null ? null : String(value);
      }
      function currentHelperGeneration() {
        var iframe = getHelperFrame();
        if (!iframe || !iframe.src) return null;
        try {
          return helperGeneration(
            new URL(iframe.src, window.location.href).searchParams.get('v')
          );
        } catch (e) {
          return null;
        }
      }
      function applyHelperBusy(active) {
        var wrap = document.querySelector('.helper-wrap');
        if (active) {
          document.body.classList.add('dragmapr-helper-busy');
          if (wrap) wrap.classList.add('is-updating');
          return;
        }
        document.body.classList.remove('dragmapr-helper-busy');
        if (wrap) wrap.classList.remove('is-updating');
      }
      function syncLoadingVisuals() {
        document.body.classList.toggle('dragmapr-loading', helperState.fullLoading);
        var helperBusy = helperState.activeGeneration !== null &&
          helperState.readyGeneration !== helperState.activeGeneration;
        applyHelperBusy(!helperState.fullLoading && helperBusy);
      }
      function setFullLoading(active) {
        helperState.fullLoading = !!active;
        syncLoadingVisuals();
      }
      function markHelperReady(generation) {
        generation = helperGeneration(generation);
        if (!generation || generation !== helperState.activeGeneration) {
          return;
        }
        window.clearTimeout(helperState.fallbackTimer);
        helperState.readyGeneration = generation;
        helperState.fullLoading = false;
        syncLoadingVisuals();
        Shiny.setInputValue(
          'helper_ready_token',
          {generation: generation, at: Date.now()},
          {priority: 'event'}
        );
      }
      function scheduleHelperFallback(generation) {
        generation = helperGeneration(generation);
        window.clearTimeout(helperState.fallbackTimer);
        if (!generation) return;
        helperState.fallbackTimer = window.setTimeout(function() {
          if (generation !== helperState.activeGeneration) return;
          if (currentHelperGeneration() === generation) {
            markHelperReady(generation);
          } else {
            scheduleHelperFallback(generation);
          }
        }, 1500);
      }
      Shiny.addCustomMessageHandler('dragmapr-labels', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-labels', labels: !!message.labels}, helperTargetOrigin()
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-reset-state', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage({type: 'dragmapr-reset-state'}, helperTargetOrigin());
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-state', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage({
            type: 'dragmapr-set-state',
            regionOffsets: message.regionOffsets || [],
            labelOffsets: message.labelOffsets || []
          }, helperTargetOrigin());
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-label-data', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-label-data', labels: message.labels || []}, helperTargetOrigin()
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-label-options', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-label-options', options: message.options || {}}, helperTargetOrigin()
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-region-palette', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-region-palette', palette: message.palette || {}}, helperTargetOrigin()
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-label-colors', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-label-colors', colors: message.colors || {}}, helperTargetOrigin()
          );
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-loading', function(message) {
        setFullLoading(!!(message && message.active));
      });
      Shiny.addCustomMessageHandler('dragmapr-helper-loading', function(message) {
        var active = !!(message && message.active);
        var generation = helperGeneration(message && message.generation);
        if (active) {
          helperState.activeGeneration = generation;
          helperState.readyGeneration = null;
          scheduleHelperFallback(generation);
        } else if (!generation || generation === helperState.activeGeneration) {
          window.clearTimeout(helperState.fallbackTimer);
          helperState.activeGeneration = null;
          helperState.readyGeneration = null;
          helperState.fullLoading = false;
        }
        syncLoadingVisuals();
      });
      document.addEventListener('change', function(event) {
        if (event.target && event.target.id === 'spatial_upload') {
          setFullLoading(true);
        }
      });
      document.addEventListener('click', function(event) {
        var loadButton = event.target && event.target.closest &&
          event.target.closest('#load_demo');
        if (loadButton) {
          setFullLoading(true);
        }
      }, true);

      window.addEventListener('message', function(event) {
        var iframe = getHelperFrame();
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-ready') {
          markHelperReady(event.data.generation);
        }
      });
      document.addEventListener('load', function(event) {
        if (event.target && event.target.matches &&
            event.target.matches('iframe.studio-helper-frame')) {
          scheduleHelperFallback(currentHelperGeneration());
        }
      }, true);
      document.addEventListener('click', function(event) {
        var scriptButton = event.target && event.target.closest &&
          event.target.closest('#download_script');
        if (scriptButton) {
          Shiny.setInputValue('script_download_requested', Date.now(), {priority: 'event'});
        }
      }, true);
    ")),
    tags$style(HTML(studio_css))
  ),

  # Progress bar shown automatically whenever Shiny is busy.
  tags$div(class = "studio-progress-bar"),
  tags$div(
    class = "studio-load-veil",
    tags$div(
      class = "studio-load-panel",
      tags$div(class = "studio-spinner"),
      tags$div(
        tags$strong("Loading spatial data"),
        tags$span("Reading geometry, columns, labels, and preview state...")
      )
    )
  ),

  tags$h2("dragmapr spatial studio", class = "studio-title"),
  tags$p("Upload polygon data, drag the layout, then export.",
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
        tags$div(
          class = "studio-action-row",
          actionButton("load_demo", "Use bundled HHS demo", class = "btn-sm btn-default")
        )
      ),

      tags$div(
        class = "studio-card",
        tags$div("Columns & colors", class = "studio-section"),
        uiOutput("column_controls"),
        uiOutput("color_pickers")
      ),

      tags$div(
        class = "studio-card",
        tags$div("Legend", class = "studio-section"),
        checkboxInput("show_legend", "Show legend in drag and preview", value = TRUE),
        checkboxInput("legend_show_all", "Show all legend keys", value = FALSE),
        textInput("legend_title", "Legend title", value = "Region"),
        studio_select(
          "legend_position",
          "Legend position",
          choices = c("Bottom" = "bottom", "Top" = "top", "Left" = "left", "Right" = "right", "None" = "none"),
          selected = "bottom",
          placeholder = "Choose legend position"
        )
      ),

      tags$div(
        class = "studio-card",
        tags$div("Labels", class = "studio-section"),
        checkboxInput("show_labels", "Show labels", value = TRUE),
        uiOutput("label_filter_ui"),
        uiOutput("label_editor_ui"),

        tags$div(class = "studio-divider"),

        uiOutput("label_color_ui"),
        tags$div(class = "studio-field-gap"),
        studio_select(
          "annotation_mode", "Annotation style",
          choices  = c("Short labels" = "labels", "Info boxes" = "boxes"),
          selected = "labels",
          placeholder = "Choose label style"
        ),
        tags$div(class = "studio-field-gap"),
        studio_select(
          "label_marker_shape", "Text label marker",
          choices = c("Circle" = "circle", "Rounded box" = "rect", "Text only" = "none"),
          selected = "circle",
          placeholder = "Choose marker style"
        ),
        tags$div(class = "studio-field-gap"),
        sliderInput("label_text_size", "Text size (px)",
                    min = 7, max = 22, value = 11, step = 1),
        uiOutput("label_size_controls")
      ),

      tags$div(
        class = "studio-card",
        tags$div("Connectors", class = "studio-section"),
        checkboxInput("show_connectors", "Show connector lines", value = FALSE),
        checkboxInput("connector_smart", "Smart connector style", value = FALSE),
        studio_select(
          "connector_type", "Connector style",
          choices  = c("Straight" = "straight", "Elbow" = "elbow",
                       "Curve" = "curve", "Squiggle" = "squiggle"),
          selected = "straight",
          placeholder = "Choose connector style"
        ),
        tags$div(class = "studio-field-gap"),
        studio_select(
          "connector_linetype", "Line style",
          choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"),
          selected = "solid",
          placeholder = "Choose line style"
        ),
        tags$div(class = "studio-field-gap"),
        studio_select(
          "connector_endpoint", "Endpoint",
          choices = c("None" = "none", "Arrow" = "arrow"),
          selected = "none",
          placeholder = "Choose endpoint"
        ),
        tags$div(class = "studio-field-gap"),
        sliderInput("connector_linewidth", "Connector thickness",
                    min = 0.25, max = 2.5, value = 0.45, step = 0.05)
      ),

      tags$div(
        class = "studio-card",
        tags$div("Export", class = "studio-section"),
        checkboxInput("show_helper_panel", "Show offset panel in drag view", value = TRUE),
        textInput("static_title", "Static map title", value = "dragmapr spatial studio"),
        tags$div(
          class = "studio-action-row",
          actionButton("apply_static_title", "Apply title", class = "btn-sm btn-primary")
        ),
        tags$div(class = "studio-field-gap"),
        studio_select(
          "map_background", "Map background",
          choices = c("White" = "white", "Transparent" = "transparent",
                      "Light grid" = "light_grid", "Dark" = "dark"),
          selected = "white",
          placeholder = "Choose map background"
        ),
        tags$div(class = "studio-field-gap"),
        fluidRow(
          column(4, numericInput("static_width", "Width", value = 10, min = 2, max = 30, step = 0.5)),
          column(4, numericInput("static_height", "Height", value = 8, min = 2, max = 30, step = 0.5)),
          column(4, numericInput("static_dpi", "DPI", value = 300, min = 72, max = 600, step = 24))
        ),
        tags$p(
          "The R script expects the Project ZIP in the same folder, or you can edit project_path.",
          class = "studio-help"
        ),
        tags$div(
          class = "download-grid",
          downloadButton("download_png",        "PNG"),
          downloadButton(
            "download_script", "R script",
            onclick = "Shiny.setInputValue('script_download_requested', Date.now(), {priority: 'event'});"
          ),
          downloadButton("download_region_csv", "Region CSV"),
          downloadButton("download_label_csv",  "Label CSV"),
          downloadButton("download_labels",     "Labels table"),
          downloadButton("download_geojson",    "GeoJSON"),
          downloadButton("download_gpkg",       "GPKG"),
          downloadButton("download_html",       "HTML helper"),
          downloadButton("download_bundle",     "Project ZIP"),
          downloadButton("download_static_bundle", "Static bundle")
        ),
        tags$div(class = "studio-divider"),
        tags$p(
          "Reopen a Project ZIP previously downloaded from Spatial Studio.",
          class = "studio-help"
        ),
        fileInput(
          "project_upload", "Open project ZIP",
          multiple = FALSE,
          accept = ".zip"
        )
      )
    ),

    mainPanel(
      width = 9,
      uiOutput("status_bar"),
      tags$div(
        class = "map-control-toolbar",
        tags$span("Map controls", class = "map-control-title"),
        uiOutput("history_controls")
      ),
      tabsetPanel(
        tabPanel(
          "Drag",
          tags$div(
            class = "helper-wrap",
            uiOutput("helper"),
            tags$div(
              id = "helper-overlay",
              class = "helper-overlay",
              tags$div(class = "studio-spinner"),
              tags$p("Updating drag map...")
            )
          )
        ),
        tabPanel(
          "Preview",
          tags$div(
            class = "preview-toolbar",
            actionButton("refresh_preview", "Refresh preview", class = "btn-sm btn-default"),
            checkboxInput("auto_preview", "Auto-refresh preview", value = FALSE)
          ),
          plotOutput("preview", height = 620)
        ),
        tabPanel(
          "State",
          tags$h4("Key reactives"),
          tags$pre(paste(
            "source_sf()       raw sf from upload / demo / project bundle",
            "projected_sf()    after prepare_dragmapr_sf()",
            "region_col()      chosen grouping column",
            "label_col()       chosen label column",
            "label_table()     make_region_labels() + style flags",
            "region_state()    current region offsets",
            "label_state()     current label offsets",
            "state$label_edits edited label text keyed by label_id",
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
    source_version  = 0L,
    status          = "Showing the bundled HHS demo - upload a shapefile, GeoJSON, or GeoPackage to use your own data. Drag regions and labels, then download the offset CSVs.",
    status_level    = "info",    # "info" | "ok" | "error"
    helper_token    = 0L,
    helper_loading_generation = NULL,
    helper_signature = NULL,
    helper_building = FALSE,     # TRUE while drag_map_prototype() is running
    helper_loading  = FALSE,     # TRUE until the new iframe reports ready
    ingesting       = FALSE,
    region_csv_cache = NULL,
    label_csv_cache  = NULL,
    label_edits      = list(),
    region_palette_override = NULL,
    label_palette_override  = NULL,
    static_title = "dragmapr spatial studio",
    pending_region_col = NULL,
    pending_label_col  = NULL,
    column_select_signature = NULL,
    undo_stack = list(),
    redo_stack = list(),
    history_armed = FALSE,
    history_ignore_until = NULL,
    restoring_history = FALSE
  )

  helper_dir <- tempfile("dragmapr_spatial_studio_")
  dir.create(helper_dir, recursive = TRUE)
  shiny::addResourcePath("dragmapr_spatial_studio", helper_dir)
  helper_file <- file.path(helper_dir, "studio_helper.html")

  set_status <- function(msg, level = "info") {
    state$status       <- msg
    state$status_level <- level
  }

  set_loading <- function(active) {
    session$sendCustomMessage("dragmapr-loading", list(active = isTRUE(active)))
  }

  set_helper_loading <- function(active, generation = state$helper_loading_generation) {
    state$helper_loading <- isTRUE(active)
    if (isTRUE(active)) {
      state$helper_loading_generation <- as.integer(generation)
    }
    session$sendCustomMessage("dragmapr-helper-loading", list(
      active = isTRUE(active),
      generation = generation
    ))
    if (!isTRUE(active)) {
      state$helper_loading_generation <- NULL
    }
  }

  set_source <- function(x) {
    state$source <- x
    state$source_version <- state$source_version + 1L
  }

  finish_ingest <- function() {
    state$ingesting <- FALSE
  }

  clear_project_state <- function() {
    state$region_csv_cache <- NULL
    state$label_csv_cache <- NULL
    state$label_edits <- list()
    state$region_palette_override <- NULL
    state$label_palette_override <- NULL
    state$pending_region_col <- NULL
    state$pending_label_col <- NULL
    state$column_select_signature <- NULL
    state$undo_stack <- list()
    state$redo_stack <- list()
    state$history_armed <- FALSE
    state$history_ignore_until <- NULL
    state$helper_signature <- NULL
  }

  rows_for_message <- function(x) {
    jsonlite::fromJSON(
      jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }

  state_snapshot <- function() {
    list(region_offsets = region_state(), label_offsets = label_state())
  }

  same_snapshot <- function(a, b) {
    if (is.null(a) || is.null(b)) return(FALSE)
    identical(csv_text(a$region_offsets), csv_text(b$region_offsets)) &&
      identical(csv_text(a$label_offsets), csv_text(b$label_offsets))
  }

  push_history <- function(snapshot) {
    if (isTRUE(state$restoring_history)) return()
    ignore_until <- state$history_ignore_until
    if (!is.null(ignore_until) && Sys.time() < ignore_until) {
      state$undo_stack <- list(snapshot)
      state$redo_stack <- list()
      return()
    }
    if (!isTRUE(state$history_armed)) {
      state$undo_stack <- list(snapshot)
      state$redo_stack <- list()
      return()
    }
    stack <- state$undo_stack
    if (length(stack) > 0L && same_snapshot(stack[[length(stack)]], snapshot)) {
      return()
    }
    stack[[length(stack) + 1L]] <- snapshot
    if (length(stack) > 50L) {
      stack <- stack[(length(stack) - 49L):length(stack)]
    }
    state$undo_stack <- stack
    state$redo_stack <- list()
  }

  apply_history_snapshot <- function(snapshot) {
    state$restoring_history <- TRUE
    on.exit({
      state$restoring_history <- FALSE
    }, add = TRUE)
    state$region_csv_cache <- csv_text(snapshot$region_offsets)
    state$label_csv_cache <- csv_text(snapshot$label_offsets)
    session$sendCustomMessage("dragmapr-state", list(
      regionOffsets = rows_for_message(snapshot$region_offsets),
      labelOffsets  = rows_for_message(snapshot$label_offsets)
    ))
    do_refresh()
  }

  base_label_text <- function(label_id) {
    labels <- base_label_table()
    idx <- match(as.character(label_id), as.character(labels$label_id))
    if (is.na(idx)) "" else as.character(labels$label[idx])
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
          "Lowest-cardinality column is '", best_col, "' with ", n_best, " unique values. ",
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
    set_loading(TRUE)
    state$ingesting <- TRUE
    on.exit(finish_ingest(), add = TRUE)
    x <- example_hhs_layout()$states
    set_source(x)
    clear_project_state()
    post_load_hints(x, "bundled HHS demo")
  })

  observeEvent(input$spatial_upload, {
    set_loading(TRUE)
    state$ingesting <- TRUE
    on.exit(finish_ingest(), add = TRUE)
    tryCatch({
      x <- read_dragmapr_sf_upload(input$spatial_upload)
      if (!is.null(x)) {
        set_source(x)
        clear_project_state()
        post_load_hints(x, "upload")
      } else {
        set_loading(FALSE)
      }
    }, error = function(e) {
      set_loading(FALSE)
      set_helper_loading(FALSE)
      set_status(paste("Upload failed:", conditionMessage(e)), "error")
    })
  })

  observeEvent(input$project_upload, {
    set_loading(TRUE)
    state$ingesting <- TRUE
    on.exit(finish_ingest(), add = TRUE)
    tryCatch({
      project <- read_dragmapr_project_upload(input$project_upload)
      if (is.null(project)) {
        set_loading(FALSE)
        return()
      }

      metadata <- project$metadata %||% list()
      set_source(project$source)
      state$static_title <- metadata$title %||% "dragmapr spatial studio"
      updateTextInput(session, "static_title", value = state$static_title)
      state$undo_stack <- list()
      state$redo_stack <- list()
      state$region_csv_cache <- if (!is.null(project$region_offsets)) csv_text(project$region_offsets) else NULL
      state$label_csv_cache <- if (!is.null(project$label_offsets)) csv_text(project$label_offsets) else NULL
      state$pending_region_col <- metadata$region_col %||% NULL
      state$pending_label_col <- metadata$label_col %||% NULL

      edits <- metadata$label_edits %||% list()
      if (is.atomic(edits)) edits <- as.list(edits)
      state$label_edits <- edits

      if (!is.null(project$palette) && all(c("region", "color") %in% names(project$palette))) {
        state$region_palette_override <- stats::setNames(project$palette$color, as.character(project$palette$region))
      } else {
        state$region_palette_override <- NULL
      }

      if (!is.null(project$labels) && all(c("label_id", "label_color") %in% names(project$labels))) {
        state$label_palette_override <- stats::setNames(project$labels$label_color, as.character(project$labels$label_id))
      } else {
        state$label_palette_override <- NULL
      }

      set_status("Loaded project bundle. Restored geometry, offsets, labels, and palettes.", "ok")
    }, error = function(e) {
      set_loading(FALSE)
      set_helper_loading(FALSE)
      set_status(paste("Project load failed:", conditionMessage(e)), "error")
    })
  })

  observeEvent(input$helper_ready_token, {
    ready_payload <- input$helper_ready_token
    ready_generation <- if (is.list(ready_payload)) {
      suppressWarnings(as.integer(ready_payload$generation %||% NA_integer_))
    } else {
      NA_integer_
    }
    if (!isTRUE(identical(ready_generation, state$helper_token))) {
      return()
    }
    set_loading(FALSE)
    set_helper_loading(FALSE, generation = ready_generation)
    if (!isTRUE(state$history_armed)) {
      state$undo_stack <- list(state_snapshot())
      state$redo_stack <- list()
      state$history_armed <- TRUE
    }
    state$history_ignore_until <- Sys.time() + 2
  }, ignoreInit = TRUE)

  # ---- Reactives ----

  source_sf <- reactive(state$source)

  projected_sf <- reactive({
    tryCatch(
      prepare_dragmapr_sf(source_sf()),
      error = function(e) {
        set_status(paste("Could not prepare geometry:", conditionMessage(e)), "error")
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
    override <- state$region_palette_override
    base_palette <- stats::setNames(rep(default_palette, length.out = n), groups)
    if (!is.null(override)) {
      keep <- intersect(names(override), groups)
      base_palette[keep] <- unname(override[keep])
    }
    base_palette
  })

  observeEvent(input$region_color_group, {
    groups <- region_groups()
    selected <- input$region_color_group
    if (is.null(selected) || !selected %in% groups) {
      return()
    }
    value <- unname(region_palette()[[selected]])
    if (requireNamespace("shinyWidgets", quietly = TRUE)) {
      shinyWidgets::updateColorPickr(session, "region_color_value", value = value)
    } else {
      updateTextInput(session, "region_color_value", value = value)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$region_color_value, {
    groups <- region_groups()
    selected <- input$region_color_group
    value <- input$region_color_value
    if (is.null(selected) || !selected %in% groups || !valid_hex_color(value)) {
      return()
    }
    override <- state$region_palette_override %||% character()
    override[[selected]] <- value
    state$region_palette_override <- override
  }, ignoreInit = TRUE)

  base_label_table <- reactive({
    req(region_col(), label_col())
    labels <- make_region_labels(projected_sf(), region_col = region_col(), label_col = label_col())
    label_levels <- stable_unique(labels$label)
    region_levels <- region_groups()
    labels$label <- factor(as.character(labels$label), levels = label_levels, ordered = TRUE)
    labels$region <- factor(as.character(labels$region), levels = region_levels, ordered = TRUE)
    labels[order(labels$label, labels$region, labels$label_id), , drop = FALSE]
  })

  all_label_table <- reactive({
    labels <- base_label_table()
    apply_label_edits(labels, state$label_edits)
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
    pal <- stats::setNames(rep(default_label_colors, length.out = length(ids)), ids)
    if (length(ids) == 0L) {
      return(pal)
    }
    override <- state$label_palette_override
    if (!is.null(override)) {
      keep <- intersect(names(override), ids)
      pal[keep] <- unname(override[keep])
    }
    pal
  })

  observeEvent(input$label_color_group, {
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    selected <- input$label_color_group
    if (is.null(selected) || !selected %in% ids) {
      return()
    }
    value <- unname(label_palette()[[selected]])
    if (requireNamespace("shinyWidgets", quietly = TRUE)) {
      shinyWidgets::updateColorPickr(session, "label_color_value", value = value)
    } else {
      updateTextInput(session, "label_color_value", value = value)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$label_color_value, {
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    selected <- input$label_color_group
    value <- input$label_color_value
    if (is.null(selected) || !selected %in% ids || !valid_hex_color(value)) {
      return()
    }
    override <- state$label_palette_override %||% character()
    override[[selected]] <- value
    state$label_palette_override <- override
  }, ignoreInit = TRUE)

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
    labels <- apply_label_edits(labels, state$label_edits)
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

  observeEvent(list(region_state(), label_state()), {
    push_history(state_snapshot())
  }, ignoreInit = FALSE)

  build_plot <- function(region_offsets, label_offsets) {
    # Suppress the legend when there are too many groups; the studio handles
    # this directly so it works regardless of which package version is installed.
    max_keys <- if (isTRUE(input$legend_show_all)) Inf else LEGEND_THRESHOLD
    plot_labels <- if (isTRUE(input$show_labels)) {
      labels <- label_table()
      if (isTRUE(input$connector_smart)) {
        labels <- apply_smart_connector_types(labels, label_offsets)
      }
      labels
    } else {
      FALSE
    }
    plot <- render_dragged_map(
      projected_sf(),
      region_offsets      = region_offsets,
      region_col          = region_col(),
      labels              = plot_labels,
      label_offsets       = label_offsets,
      region_palette      = region_palette(),
      show_legend         = isTRUE(input$show_legend),
      max_legend_keys     = max_keys,
      legend_position     = input$legend_position %||% "bottom",
      legend_title        = input$legend_title %||% "Region",
      show_label_marker   = !identical(input$label_marker_shape %||% "circle", "none"),
      label_marker_shape  = input$label_marker_shape %||% "circle",
      marker_size         = (input$label_radius %||% 14) / 3,
      connector_linewidth = input$connector_linewidth %||% 0.45,
      connector_linetype  = input$connector_linetype %||% "solid",
      connector_endpoint  = input$connector_endpoint %||% "none",
      label_padding       = 0.12,
      map_background      = input$map_background %||% "white",
      title               = state$static_title
    )
    if (isTRUE(input$show_legend) && !identical(input$legend_position %||% "bottom", "none")) {
      key_count <- length(region_groups())
      if (key_count > 0L) {
        horizontal <- input$legend_position %in% c("top", "bottom")
        legend_cols <- if (horizontal) min(4L, key_count) else 1L
        plot <- plot +
          ggplot2::guides(
            fill = ggplot2::guide_legend(ncol = legend_cols, byrow = TRUE)
          ) +
          ggplot2::theme(
            legend.box = if (horizontal) "horizontal" else "vertical",
            legend.key.size = ggplot2::unit(0.42, "lines"),
            legend.text = ggplot2::element_text(size = 8)
          )
      }
    }
    plot
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
         input$show_labels, input$show_legend, input$label_marker_shape,
         input$connector_linewidth, input$legend_show_all, input$legend_position,
         input$legend_title, input$connector_linetype, input$connector_endpoint,
         input$connector_smart, input$map_background, state$static_title),
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
    icon <- switch(level, ok = "OK", error = "!", "i")
    tags$div(
      class = paste("studio-status", level),
      tags$span(icon, class = "status-icon"),
      tags$span(state$status)
    )
  })

  output$history_controls <- renderUI({
    can_undo <- length(state$undo_stack) > 1L
    can_redo <- length(state$redo_stack) > 0L
    history_button <- function(id, label, class, enabled, title) {
      tags$button(
        id = id,
        type = "button",
        class = paste("btn action-button", class),
        title = title,
        disabled = if (isTRUE(enabled)) NULL else NA,
        tags$span(label, class = "action-label")
      )
    }
    tags$div(
      class = "map-control-actions",
      history_button(
        "undo_layout", "Undo", "btn-sm btn-default",
        can_undo,
        if (can_undo) "Undo the last drag-state change" else "No drag-state changes to undo"
      ),
      history_button(
        "redo_layout", "Redo", "btn-sm btn-default",
        can_redo,
        if (can_redo) "Redo the last undone drag-state change" else "No undone drag-state changes to redo"
      ),
      actionButton("reset_layout", "Reset drag layout", class = "btn-sm btn-warning")
    )
  })

  output$column_controls <- renderUI({
    cols <- available_columns()
    if (length(cols) == 0L) return(tags$p("No non-geometry columns found.", style = "color:#6b7280; font-size:0.83rem;"))
    default_col <- state$pending_region_col %||% if ("hhs_region" %in% cols) "hhs_region" else cols[1]
    default_label <- state$pending_label_col %||% default_col
    tagList(
      studio_select(
        "region_col", "Group / region column",
        choices = cols,
        selected = choose_column(input$region_col, cols, default_col),
        placeholder = "Choose grouping column"
      ),
      tags$div(class = "studio-field-gap"),
      studio_select(
        "label_col",  "Label column",
        choices = cols,
        selected = choose_column(input$label_col, cols, default_label),
        placeholder = "Choose label column"
      )
    )
  })

  output$color_pickers <- renderUI({
    req(region_col())
    groups <- region_groups()
    pal <- region_palette()
    selected <- input$region_color_group
    if (is.null(selected) || !selected %in% groups) {
      selected <- groups[1]
    }
    if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
      return(tagList(
        tags$p(
          paste0(length(groups), " categories. Choose a category, then set its color."),
          class = "studio-help"
        ),
        studio_select(
          "region_color_group", "Category",
          choices = groups,
          selected = selected,
          placeholder = "Choose category"
        ),
        textInput("region_color_value", "Color", value = unname(pal[[selected]]))
      ))
    }
    tagList(
      tags$p(
        paste0(length(groups), " categories. Choose a category, then set its color."),
        class = "studio-help"
      ),
      studio_select(
        "region_color_group", "Category",
        choices = groups,
        selected = selected,
        placeholder = "Choose category"
      ),
      tags$div(
        class = "region-color-editor",
        tags$div(
          tags$label("Color", `for` = "region_color_value", class = "control-label"),
          shinyWidgets::colorPickr(
            inputId  = "region_color_value",
            label    = NULL,
            selected = unname(pal[[selected]]),
            theme    = "nano",
            update   = "save",
            inline   = FALSE,
            swatches = unique(c(unname(pal), default_palette)),
            width    = "34px"
          )
        )
      )
    )
  })

  # Renders only the size sliders that are relevant for the current
  # annotation mode and label marker shape, keeping the sidebar uncluttered.
  output$label_size_controls <- renderUI({
    mode  <- input$annotation_mode    %||% "labels"
    shape <- input$label_marker_shape %||% "circle"

    if (identical(mode, "boxes")) {
      tagList(
        tags$p("Box size", class = "slider-group-label"),
        tags$div(
          class = "slider-pair",
          sliderInput("box_width",  "Width",  min = 110, max = 280, value = 170, step = 5),
          sliderInput("box_height", "Height", min = 48,  max = 150, value = 76,  step = 4)
        )
      )
    } else if (identical(shape, "circle")) {
      sliderInput("label_radius", "Circle radius (px)",
                  min = 8, max = 60, value = 14, step = 1)
    } else if (identical(shape, "rect")) {
      tagList(
        tags$p("Marker size", class = "slider-group-label"),
        tags$div(
          class = "slider-pair",
          sliderInput("label_width",  "Width",  min = 32, max = 180, value = 64, step = 4),
          sliderInput("label_height", "Height", min = 22, max = 90,  value = 30, step = 2)
        )
      )
    } else {
      # "none" - text only: text size slider above is all that is needed
      NULL
    }
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

    studio_multi_select(
      "label_filter",
      "Visible labels",
      choices = choices,
      selected = selected,
      placeholder = "Choose labels to show",
      all_label = "All labels"
    )
  })

  output$label_editor_ui <- renderUI({
    labels <- base_label_table()
    if (nrow(labels) == 0L) {
      return(NULL)
    }
    choices <- stats::setNames(
      as.character(labels$label_id),
      paste0(as.character(labels$label), " (", as.character(labels$region), ")")
    )
    selected <- input$edit_label_id
    if (is.null(selected) || !selected %in% unname(choices)) {
      selected <- unname(choices)[1]
    }
    idx <- match(selected, as.character(labels$label_id))
    edits <- isolate(state$label_edits)
    value <- edits[[selected]] %||% as.character(labels$label[idx])

    tags$div(
      class = "label-editor",
      studio_select(
        "edit_label_id", "Edit label text",
        choices = choices,
        selected = selected,
        placeholder = "Choose a label"
      ),
      tags$div(class = "studio-field-gap"),
      textAreaInput("edit_label_text", NULL, value = value, rows = 2, resize = "vertical"),
      tags$div(
        class = "studio-action-row",
        actionButton("apply_label_text", "Apply label text", class = "btn-sm btn-primary"),
        actionButton("reset_label_text", "Reset label", class = "btn-sm btn-default"),
        actionButton("reset_all_label_text", "Reset all labels", class = "btn-sm btn-default")
      )
    )
  })

  output$label_color_ui <- renderUI({
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    n <- length(ids)
    current_label_palette <- label_palette()

    if (n == 0L) {
      return(NULL)
    }

    choices <- stats::setNames(
      ids,
      paste0(as.character(labels$label), " (", as.character(labels$region), ")")
    )
    selected <- input$label_color_group
    if (is.null(selected) || !selected %in% ids) {
      selected <- ids[1]
    }

    if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
      return(tagList(
        tags$p(
          paste0(n, " labels. Choose a label, then set its color."),
          class = "studio-help"
        ),
        studio_select(
          "label_color_group", "Label",
          choices = choices,
          selected = selected,
          placeholder = "Choose label"
        ),
        textInput("label_color_value", "Color", value = unname(current_label_palette[[selected]]))
      ))
    }

    tagList(
      tags$p(
        paste0(n, " labels. Choose a label, then set its color."),
        class = "studio-help"
      ),
      studio_select(
        "label_color_group", "Label",
        choices = choices,
        selected = selected,
        placeholder = "Choose label"
      ),
      tags$div(
        class = "region-color-editor",
        tags$div(
          tags$label("Color", `for` = "label_color_value", class = "control-label"),
          shinyWidgets::colorPickr(
            inputId  = "label_color_value",
            label    = NULL,
            selected = unname(current_label_palette[[selected]]),
            theme    = "nano",
            update   = "save",
            inline   = FALSE,
            swatches = unique(c(unname(current_label_palette), default_label_colors)),
            width    = "34px"
          )
        )
      )
    )
  })

  observeEvent(input$edit_label_id, {
    id <- input$edit_label_id
    if (is.null(id) || !nzchar(id)) return()
    edits <- state$label_edits
    value <- edits[[id]] %||% base_label_text(id)
    updateTextAreaInput(session, "edit_label_text", value = value)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_label_text, {
    id <- input$edit_label_id
    if (is.null(id) || !nzchar(id)) return()
    value <- input$edit_label_text %||% ""
    edits <- state$label_edits
    if (identical(value, base_label_text(id))) {
      edits[[id]] <- NULL
    } else {
      edits[[id]] <- value
    }
    state$label_edits <- edits
    set_status("Applied selected label text.", "ok")
  }, ignoreInit = TRUE)

  observeEvent(input$apply_static_title, {
    title <- input$static_title %||% "dragmapr spatial studio"
    state$static_title <- if (is_blank(title)) "dragmapr spatial studio" else title
    updateTextInput(session, "static_title", value = state$static_title)
    set_status("Applied static map title.", "ok")
  }, ignoreInit = TRUE)

  observeEvent(input$reset_label_text, {
    id <- input$edit_label_id
    if (is.null(id) || !nzchar(id)) return()
    edits <- state$label_edits
    edits[[id]] <- NULL
    state$label_edits <- edits
    updateTextAreaInput(session, "edit_label_text", value = base_label_text(id))
    set_status("Reset selected label text.", "ok")
  }, ignoreInit = TRUE)

  observeEvent(input$reset_all_label_text, {
    state$label_edits <- list()
    id <- input$edit_label_id
    if (!is.null(id) && nzchar(id)) {
      updateTextAreaInput(session, "edit_label_text", value = base_label_text(id))
    }
    set_status("Reset all label text.", "ok")
  }, ignoreInit = TRUE)

  observeEvent(available_columns(), {
    cols <- available_columns()
    if (length(cols) == 0L) return()
    default_col <- state$pending_region_col %||% if ("hhs_region" %in% cols) "hhs_region" else cols[1]
    default_label <- state$pending_label_col %||% default_col
    selected_region <- choose_column(state$pending_region_col %||% input$region_col, cols, default_col)
    selected_label <- choose_column(state$pending_label_col %||% input$label_col, cols, default_label)
    signature <- list(cols = cols, region = selected_region, label = selected_label)
    if (!identical(signature, state$column_select_signature)) {
      updateSelectInput(session, "region_col", choices = cols, selected = selected_region)
      updateSelectInput(session, "label_col", choices = cols, selected = selected_label)
      state$column_select_signature <- signature
    }
    state$pending_region_col <- NULL
    state$pending_label_col <- NULL
  }, ignoreInit = FALSE)

  # Rebuild helper HTML for data, grouping, or label-column changes. Palette
  # and label-only style changes are sent to the existing iframe with
  # postMessage so color edits do not reset the drag state or show a map overlay.
  observeEvent(
    list(projected_sf(), region_col(), label_col(), input$show_helper_panel),
    {
      signature <- list(
        source_version = state$source_version,
        region_col = region_col(),
        label_col = label_col(),
        show_helper_panel = isTRUE(input$show_helper_panel %||% TRUE)
      )
      if (identical(signature, state$helper_signature)) {
        return()
      }
      next_generation <- state$helper_token + 1L
      state$helper_building <- TRUE
      set_helper_loading(TRUE, generation = next_generation)
      on.exit({
        state$helper_building <- FALSE
      }, add = TRUE)
      tryCatch({
        drag_map_prototype(
          projected_sf(),
          region_col          = region_col(),
          label_col           = label_col(),
          labels              = label_table(),
          label_marker        = !identical(input$label_marker_shape %||% "circle", "none"),
          label_marker_shape  = input$label_marker_shape %||% "circle",
          label_text_size     = input$label_text_size %||% 11,
          label_radius        = input$label_radius %||% 14,
          label_width         = input$label_width  %||% 64,
          label_height        = input$label_height %||% 30,
          label_box_width     = input$box_width  %||% 170,
          label_box_height    = input$box_height %||% 76,
          connector_linewidth = (input$connector_linewidth %||% 0.45) * 3,
          region_offsets      = isolate(region_state()),
          label_offsets       = isolate(label_state()),
          region_palette      = region_palette(),
          show_legend         = isTRUE(input$show_legend),
          max_legend_keys     = if (isTRUE(input$legend_show_all)) 1000000 else LEGEND_THRESHOLD,
          legend_position     = input$legend_position %||% "bottom",
          legend_title        = input$legend_title %||% "Region",
          map_background      = input$map_background %||% "white",
          connector_linetype  = input$connector_linetype %||% "solid",
          connector_endpoint  = input$connector_endpoint %||% "none",
          connector_smart     = isTRUE(input$connector_smart),
          side_panel          = isTRUE(input$show_helper_panel %||% TRUE),
          file                = helper_file
        )
        state$helper_token    <- next_generation
        state$helper_signature <- signature
      }, error = function(e) {
        set_loading(FALSE)
        set_helper_loading(FALSE, generation = next_generation)
        set_status(paste("Could not build the interactive helper:", conditionMessage(e)), "error")
      })
    },
    ignoreInit = FALSE
  )

  observeEvent(input$show_labels, {
    session$sendCustomMessage("dragmapr-labels", list(labels = isTRUE(input$show_labels)))
  }, ignoreInit = FALSE)

  observeEvent(label_table(), {
    if (isTRUE(state$helper_loading) || isTRUE(state$helper_building)) return()
    session$sendCustomMessage("dragmapr-label-data", list(labels = rows_for_message(label_table())))
  }, ignoreInit = TRUE)

  observeEvent(region_palette(), {
    if (isTRUE(state$helper_loading) || isTRUE(state$helper_building)) return()
    session$sendCustomMessage("dragmapr-region-palette", list(
      palette = as.list(region_palette())
    ))
  }, ignoreInit = TRUE)

  observeEvent(label_palette(), {
    if (isTRUE(state$helper_loading) || isTRUE(state$helper_building)) return()
    session$sendCustomMessage("dragmapr-label-colors", list(
      colors = as.list(label_palette())
    ))
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$label_marker_shape, input$label_text_size,
         input$label_radius, input$label_width, input$label_height,
         input$box_width, input$box_height, input$connector_linewidth,
         input$connector_linetype, input$connector_endpoint, input$connector_smart,
         input$show_legend, input$legend_show_all, input$legend_position,
         input$legend_title, input$map_background),
    {
      session$sendCustomMessage("dragmapr-label-options", list(options = list(
        labelMarker = !identical(input$label_marker_shape %||% "circle", "none"),
        labelMarkerShape = input$label_marker_shape %||% "circle",
        labelTextSize = input$label_text_size %||% 11,
        labelRadius = input$label_radius %||% 14,
        labelWidth = input$label_width %||% 64,
        labelHeight = input$label_height %||% 30,
        labelBoxWidth = input$box_width %||% 170,
        labelBoxHeight = input$box_height %||% 76,
        connectorLinewidth = (input$connector_linewidth %||% 0.45) * 3,
        connectorLinetype = input$connector_linetype %||% "solid",
        connectorEndpoint = input$connector_endpoint %||% "none",
        connectorSmart = isTRUE(input$connector_smart),
        showLegend = isTRUE(input$show_legend),
        maxLegendKeys = if (isTRUE(input$legend_show_all)) 1000000 else LEGEND_THRESHOLD,
        legendPosition = input$legend_position %||% "bottom",
        legendTitle = input$legend_title %||% "Region",
        mapBackground = input$map_background %||% "white"
      )))
    },
    ignoreInit = TRUE
  )

  observeEvent(input$undo_layout, {
    stack <- state$undo_stack
    if (length(stack) <= 1L) {
      set_status("Nothing to undo yet.", "info")
      return()
    }
    current <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    state$redo_stack <- c(state$redo_stack, list(current))
    state$undo_stack <- stack
    apply_history_snapshot(stack[[length(stack)]])
    set_status("Undid last drag-state change.", "ok")
  }, ignoreInit = TRUE)

  observeEvent(input$redo_layout, {
    redo <- state$redo_stack
    if (length(redo) == 0L) {
      set_status("Nothing to redo yet.", "info")
      return()
    }
    snapshot <- redo[[length(redo)]]
    redo <- redo[-length(redo)]
    state$redo_stack <- redo
    state$undo_stack <- c(state$undo_stack, list(snapshot))
    apply_history_snapshot(snapshot)
    set_status("Redid drag-state change.", "ok")
  }, ignoreInit = TRUE)

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

  observeEvent(input$script_download_requested, {
    showModal(modalDialog(
      title = "Use the R script with a Project ZIP",
      tags$p("The R script recreates the static map from a Spatial Studio project bundle."),
      tags$ul(
        tags$li("Download Project ZIP and keep it in the same folder as the script."),
        tags$li("If the ZIP is somewhere else, edit project_path in the script."),
        tags$li("Use Static bundle when you want the script, project files, PNG, and PDF together.")
      ),
      easyClose = TRUE,
      footer = modalButton("Got it")
    ))
  }, ignoreInit = TRUE)

  output$helper <- renderUI({
    req(state$helper_token > 0L, file.exists(helper_file))
    tags$iframe(
      src   = paste0("dragmapr_spatial_studio/studio_helper.html?v=", state$helper_token),
      class = "studio-helper-frame"
    )
  })

  output$preview           <- renderPlot(preview_plot())
  output$region_state_text <- renderText(region_csv_raw() %||% "Waiting for drag state...")
  output$label_state_text  <- renderText(label_csv_raw()  %||% "Waiting for drag state...")

  # ---- Download handlers ----

  static_width <- function() {
    value <- suppressWarnings(as.numeric(input$static_width %||% 10))
    if (!is.finite(value) || value <= 0) 10 else value
  }
  static_height <- function() {
    value <- suppressWarnings(as.numeric(input$static_height %||% 8))
    if (!is.finite(value) || value <= 0) 8 else value
  }
  static_dpi <- function() {
    value <- suppressWarnings(as.numeric(input$static_dpi %||% 300))
    if (!is.finite(value) || value <= 0) 300 else value
  }
  static_title <- function() {
    state$static_title
  }
  export_metadata <- function() {
    list(
      region_col          = region_col(),
      label_col           = label_col(),
      title               = static_title(),
      width               = static_width(),
      height              = static_height(),
      dpi                 = static_dpi(),
      crs_epsg            = sf::st_crs(projected_sf())$epsg,
      show_labels         = isTRUE(input$show_labels),
      show_legend         = isTRUE(input$show_legend),
      legend_position     = input$legend_position %||% "bottom",
      legend_title        = input$legend_title %||% "Region",
      map_background      = input$map_background %||% "white",
      label_marker_shape  = input$label_marker_shape %||% "circle",
      marker_size         = (input$label_radius %||% 14) / 3,
      connector_linewidth = input$connector_linewidth %||% 0.45,
      connector_linetype  = input$connector_linetype %||% "solid",
      connector_endpoint  = input$connector_endpoint %||% "none",
      connector_smart     = isTRUE(input$connector_smart),
      label_padding       = 0.12,
      label_edits         = state$label_edits,
      created             = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      dragmapr_version    = as.character(utils::packageVersion("dragmapr"))
    )
  }
  recreate_script_text <- function(project_path = ".") {
    paste(
      "# Recreate a static map from a dragmapr Spatial Studio project.",
      "#",
      "# Before running this script:",
      "# 1. Download the Project ZIP from Spatial Studio.",
      "# 2. Put that ZIP in the same folder as this script, or edit project_path below.",
      "# 3. Run install.packages(\"dragmapr\") if dragmapr is not installed yet.",
      "#",
      "# The Static bundle download already includes the project files, this script,",
      "# and ready-made PNG/PDF outputs.",
      "",
      "library(dragmapr)",
      "",
      paste0("project_path <- ", deparse(project_path)),
      "",
      "if (!file.exists(project_path)) {",
      "  stop(",
      "    \"Could not find \", project_path, \".\\n\",",
      "    \"Download Project ZIP from Spatial Studio and place it next to this script, \",",
      "    \"or edit project_path to the ZIP/project folder location.\",",
      "    call. = FALSE",
      "  )",
      "}",
      "",
      "render_dragmapr_project(",
      "  project_path,",
      "  file = \"dragmapr-static-map.png\",",
      paste0("  width = ", static_width(), ","),
      paste0("  height = ", static_height(), ","),
      paste0("  dpi = ", static_dpi(), ","),
      paste0("  title = ", deparse(static_title())),
      ")",
      sep = "\n"
    )
  }
  export_validation_message <- function() {
    groups <- region_groups()
    offsets <- region_state()
    labels <- label_table()
    label_offsets <- label_state()
    missing_regions <- setdiff(groups, as.character(offsets$region))
    missing_labels <- setdiff(as.character(labels$label_id), as.character(label_offsets$label_id))
    details <- character()
    if (length(missing_regions) > 0L) {
      details <- c(details, paste0(length(missing_regions), " region offset row(s) missing; zero movement will be used."))
    }
    if (length(missing_labels) > 0L) {
      details <- c(details, paste0(length(missing_labels), " label offset row(s) missing; anchor positions will be used."))
    }
    if (length(details) == 0L) {
      "Static export is ready: geometry, labels, palette, and offsets are aligned."
    } else {
      paste(details, collapse = " ")
    }
  }
  write_project_files <- function(bundle_dir, include_static = FALSE) {
    dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
    sf::st_write(projected_sf(),
                 file.path(bundle_dir, "source.gpkg"),
                 driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    utils::write.csv(region_state(),
                     file.path(bundle_dir, "drag_region_offsets.csv"),
                     row.names = FALSE)
    utils::write.csv(label_state(),
                     file.path(bundle_dir, "drag_label_offsets.csv"),
                     row.names = FALSE)
    utils::write.csv(label_table(),
                     file.path(bundle_dir, "labels.csv"),
                     row.names = FALSE)
    utils::write.csv(
      data.frame(region = names(region_palette()),
                 color  = unname(region_palette()),
                 stringsAsFactors = FALSE),
      file.path(bundle_dir, "palette.csv"),
      row.names = FALSE
    )
    writeLines(
      jsonlite::toJSON(export_metadata(), auto_unbox = TRUE, pretty = TRUE),
      file.path(bundle_dir, "metadata.json")
    )
    writeLines(recreate_script_text("."), file.path(bundle_dir, "recreate-static-map.R"))
    if (isTRUE(include_static)) {
      plot <- current_plot()
      ggplot2::ggsave(file.path(bundle_dir, "dragmapr-static-map.png"), plot,
                      width = static_width(), height = static_height(), dpi = static_dpi())
      ggplot2::ggsave(file.path(bundle_dir, "dragmapr-static-map.pdf"), plot,
                      width = static_width(), height = static_height(), dpi = static_dpi())
    }
    invisible(bundle_dir)
  }
  zip_directory <- function(file, directory) {
    old_wd <- setwd(directory)
    on.exit(setwd(old_wd), add = TRUE)
    utils::zip(file, list.files(".", recursive = TRUE))
  }

  output$download_png <- downloadHandler(
    filename    = function() "dragmapr-spatial-studio.png",
    content     = function(file) {
      set_status(export_validation_message(), "ok")
      ggplot2::ggsave(file, current_plot(), width = static_width(), height = static_height(), dpi = static_dpi())
    },
    contentType = "image/png"
  )
  output$download_script <- downloadHandler(
    filename    = function() "recreate-static-map.R",
    content     = function(file) {
      set_status("Downloaded an R script that recreates the static map from a project ZIP.", "ok")
      writeLines(recreate_script_text("dragmapr-project.zip"), file)
    },
    contentType = "text/plain"
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
  output$download_labels <- downloadHandler(
    filename    = function() "drag_labels.csv",
    content     = function(file) utils::write.csv(label_table(), file, row.names = FALSE),
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
      write_project_files(bundle_dir, include_static = FALSE)
      set_status("Downloaded a project ZIP with geometry, offsets, labels, palette, metadata, and recreate-static-map.R.", "ok")
      zip_directory(file, bundle_dir)
    }
  )
  output$download_static_bundle <- downloadHandler(
    filename    = "dragmapr-static-bundle.zip",
    contentType = "application/zip",
    content     = function(file) {
      bundle_dir <- tempfile("dragmapr_static_bundle_")
      write_project_files(bundle_dir, include_static = TRUE)
      set_status("Downloaded a static bundle with PNG, PDF, data files, metadata, and recreate-static-map.R.", "ok")
      zip_directory(file, bundle_dir)
    }
  )
}

apply_smart_connector_types <- function(labels, label_offsets) {
  if (nrow(labels) == 0L) return(labels)
  offsets <- normalize_label_state(label_offsets, source = "`label_offsets`")
  idx <- match(as.character(labels$label_id), as.character(offsets$label_id))
  dx <- ifelse(is.na(idx), 0, offsets$dx_m[idx])
  dy <- ifelse(is.na(idx), 0, offsets$dy_m[idx])
  distance <- sqrt(dx^2 + dy^2)
  labels$connector_type <- ifelse(
    distance < 20000,
    "straight",
    ifelse(abs(dx) > abs(dy) * 1.6 | abs(dy) > abs(dx) * 1.6, "elbow", "curve")
  )
  labels
}

shinyApp(ui, server)
