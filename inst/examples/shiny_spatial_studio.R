# if (!requireNamespace("shiny", quietly = TRUE)) {
#   stop("Install shiny to run this example.", call. = FALSE)
# }

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
  selectizeInput(inputId, label, choices = choices, selected = selected,
                 options = list(placeholder = placeholder))
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
studio_color_value <- function(value, default) {
  if (valid_hex_color(value)) value else default
}
studio_color_input <- function(inputId, label, value) {
  if (requireNamespace("shinyWidgets", quietly = TRUE)) {
    return(shinyWidgets::colorPickr(
      inputId = inputId,
      label = label,
      selected = value,
      theme = "nano",
      update = "save",
      inline = FALSE,
      width = "34px"
    ))
  }
  textInput(inputId, label, value = value)
}
update_studio_color_input <- function(session, inputId, value) {
  if (requireNamespace("shinyWidgets", quietly = TRUE)) {
    shinyWidgets::updateColorPickr(session, inputId, value = value)
  } else {
    updateTextInput(session, inputId, value = value)
  }
}

studio_sidebar_panel <- function(title, subtitle = NULL, ..., open = FALSE) {
  tags$details(
    class = "studio-card studio-panel",
    open = if (isTRUE(open)) NA else NULL,
    tags$summary(
      class = "studio-panel-summary",
      tags$span(title, class = "studio-panel-title"),
      if (!is.null(subtitle) && nzchar(subtitle)) {
        tags$span(subtitle, class = "studio-panel-subtitle")
      }
    ),
    tags$div(class = "studio-panel-body", ...)
  )
}

# Legend is suppressed automatically in render_dragged_map() above this count.
LEGEND_THRESHOLD  <- 25L

# Labels are automatically thinned above this count unless the user
# explicitly chooses labels in the label picker. This keeps the Studio
# readable when a child grouping has dozens or hundreds of regions.
LABEL_AUTO_LIMIT <- 25L
LABEL_AUTO_MIN   <- 1L
LABEL_AUTO_MAX   <- 250L

# Restrained browser-side branch bloom timing. Keep the default calm:
# one timeline, no bounce, no stagger, no path-morph fight.
BLOOM_DURATION_MS <- 400
BLOOM_OVERSHOOT   <- 0
BLOOM_EASING      <- "cubic-out"
BLOOM_ANIMATION   <- "leaf_flip"
BLOOM_DRAG_THRESHOLD_PX <- 8

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

default_studio_region_col <- function(x, cols) {
  # Column-name agnostic default: no spatial file is required to use any
  # particular naming. Detection works in three relative passes:
  #   1. exact well-known names,
  #   2. substring matches ("COUNTY_NAM", "MUN_NAME", "NAMELSAD", ...),
  #   3. data-driven scoring over cardinality and column type.
  # One-row-per-region files (cardinality == nrow) are valid grouping columns
  # and must not be excluded.
  if (length(cols) == 0L) return(NULL)
  rec <- tryCatch(recommend_dragmapr_hierarchy(x), error = function(e) NULL)
  if (!is.null(rec$parent) && rec$parent %in% cols) {
    return(rec$parent)
  }
  lower <- tolower(cols)
  exact_priority <- c("hhs_region", "region", "group", "county", "district", "zone")
  for (candidate in exact_priority) {
    idx <- which(lower == candidate)
    if (length(idx) > 0L) return(cols[idx[1]])
  }

  n <- max(1L, nrow(x))
  vals <- lapply(cols, function(col) as.character(x[[col]]))
  cardinality <- vapply(vals, function(v) {
    length(unique(stats::na.omit(trimws(v))))
  }, integer(1L))
  text_like <- vapply(seq_along(cols), function(i) {
    is.character(x[[cols[i]]]) || is.factor(x[[cols[i]]])
  }, logical(1L))
  id_like <- grepl(
    "(id|objectid|gnis|ssn|fips|geoid|code|census|pop|acre|mile|area|length|leng|perimeter|density|den)$|^(id|fid|gid|objectid|geoid|fips)",
    lower
  )
  named <- grepl(
    "region|group|county|district|zone|name|nam$|mun|city|town|state|prov|ward|parish|borough|territor|type|class|category",
    lower
  )

  usable <- cardinality > 1L & !id_like
  pick_best <- function(idx) {
    if (length(idx) == 0L) return(NULL)
    # Prefer a manageable number of groups: the largest cardinality that
    # still stays at or below 100 groups; otherwise the smallest available.
    small <- idx[cardinality[idx] <= 100L]
    if (length(small) > 0L) {
      return(cols[small[order(cardinality[small], decreasing = TRUE)][1]])
    }
    cols[idx[order(cardinality[idx])][1]]
  }

  out <- pick_best(which(usable & named & text_like))
  if (!is.null(out)) return(out)
  out <- pick_best(which(usable & text_like))
  if (!is.null(out)) return(out)
  out <- pick_best(which(usable & named))
  if (!is.null(out)) return(out)
  out <- pick_best(which(usable))
  if (!is.null(out)) return(out)
  cols[1]
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

normalize_studio_label_offsets <- function(offsets) {
  empty <- data.frame(
    label_id = character(), region = character(), dx_m = numeric(), dy_m = numeric(),
    stringsAsFactors = FALSE
  )
  if (is.null(offsets) || !is.data.frame(offsets)) return(empty)
  names(offsets) <- tolower(names(offsets))
  required <- c("label_id", "region", "dx_m", "dy_m")
  if (!all(required %in% names(offsets))) return(empty)
  out <- data.frame(
    label_id = trimws(as.character(offsets$label_id)),
    region = trimws(as.character(offsets$region)),
    dx_m = suppressWarnings(as.numeric(offsets$dx_m)),
    dy_m = suppressWarnings(as.numeric(offsets$dy_m)),
    stringsAsFactors = FALSE
  )
  valid <- nzchar(out$label_id) & nzchar(out$region) & is.finite(out$dx_m) & is.finite(out$dy_m)
  out <- out[valid & !duplicated(out$label_id), , drop = FALSE]
  rownames(out) <- NULL
  out
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
  border: 1px solid #e2e8f0;
  border-radius: 10px;
  background: #ffffff;
  margin-bottom: 10px;
  box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
  overflow: hidden;
}
.studio-card[open] {
  border-color: #cbd5e1;
  box-shadow: 0 3px 12px rgba(15, 23, 42, 0.06);
}
.studio-panel-summary {
  list-style: none;
  cursor: pointer;
  display: grid;
  grid-template-columns: minmax(0, 1fr) auto;
  gap: 8px;
  align-items: center;
  padding: 10px 12px;
  background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
  border-bottom: 1px solid transparent;
}
.studio-card[open] > .studio-panel-summary {
  border-bottom-color: #e2e8f0;
}
.studio-panel-summary::-webkit-details-marker { display: none; }
.studio-panel-summary::after {
  content: '+';
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  border-radius: 999px;
  background: #eef2ff;
  color: #334155;
  font-size: 0.95rem;
  font-weight: 700;
  line-height: 1;
}
.studio-card[open] > .studio-panel-summary::after {
  content: '-';
  background: #dbeafe;
  color: #1d4ed8;
}
.studio-panel-title {
  display: block;
  font-size: 0.73rem;
  font-weight: 800;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: #263445;
}
.studio-panel-subtitle {
  display: block;
  margin-top: 2px;
  font-size: 0.74rem;
  font-weight: 500;
  letter-spacing: normal;
  text-transform: none;
  color: #64748b;
}
.studio-panel-body {
  padding: 10px 12px 12px;
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
.studio-subgroup-title {
  font-size: 0.68rem;
  font-weight: 800;
  color: #64748b;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  margin: 12px 0 6px;
}
.studio-subgroup-title:first-child { margin-top: 0; }
.studio-card .form-group { margin-bottom: 10px; }
.studio-card .checkbox { margin-top: 4px; margin-bottom: 7px; }
.studio-card .checkbox label {
  display: flex;
  align-items: center;
  gap: 8px;
  width: 100%;
  min-height: 34px;
  padding: 7px 8px;
  border: 1px solid #e2e8f0;
  border-radius: 7px;
  background: #f8fafc;
  color: #334155;
  font-size: 0.8rem;
  font-weight: 600;
  line-height: 1.25;
}
.studio-card .checkbox label:hover {
  border-color: #cbd5e1;
  background: #f1f5f9;
}
.studio-card .checkbox input[type='checkbox'] {
  position: static;
  margin: 0 !important;
  accent-color: #2563eb;
}
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
  display: flex;
  align-items: center;
  justify-content: center;
  background: rgba(248, 250, 252, 0.78);
  backdrop-filter: blur(3px);
  opacity: 0;
  visibility: hidden;
  pointer-events: none;
  transition: opacity 0.18s ease, visibility 0s linear 0.18s;
}
body.studio-busy .studio-load-veil,
body.dragmapr-loading .studio-load-veil {
  opacity: 1;
  visibility: visible;
  pointer-events: auto;
  transition-delay: 0s;
}
body.studio-busy .container-fluid {
  user-select: none;
}
body.studio-busy-processing .studio-load-panel { transform: scale(0.98); }
body.studio-busy-loading .studio-load-panel { transform: scale(1); }
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

/* ---- Branch animation soft state ------------------------------------------ */
.studio-sidebar,
.col-sm-3 > .well {
  transition: opacity 160ms ease, filter 160ms ease;
}
body.studio-animating .studio-sidebar,
body.studio-animating .col-sm-3 > .well {
  opacity: 0.74;
  filter: saturate(0.9);
}
body.studio-animating .studio-sidebar .btn,
body.studio-animating .studio-sidebar input,
body.studio-animating .studio-sidebar select,
body.studio-animating .studio-sidebar textarea,
body.studio-animating .col-sm-3 > .well .btn,
body.studio-animating .col-sm-3 > .well input,
body.studio-animating .col-sm-3 > .well select,
body.studio-animating .col-sm-3 > .well textarea {
  pointer-events: none;
}

/* ---- Subtle divider between control clusters ---- */
.studio-divider {
  border-top: 1px dashed #dbe3ee;
  margin: 14px 0 12px;
}

/* ---- Compact detected/setup summary ---- */
.studio-setup-card {
  background: #ffffff;
  border: 1px solid #dbe3ee;
  border-radius: 10px;
  padding: 12px 14px;
  margin-bottom: 12px;
  box-shadow: 0 4px 14px rgba(15, 23, 42, 0.06);
}
.studio-setup-card .setup-title {
  font-weight: 700;
  font-size: 0.82rem;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  color: #64748b;
  margin-bottom: 6px;
}
.studio-setup-card .setup-row {
  display: flex;
  justify-content: space-between;
  gap: 10px;
  font-size: 0.82rem;
  line-height: 1.5;
}
.studio-setup-card .setup-row span:first-child { color: #64748b; }
.studio-setup-card .setup-row span:last-child {
  color: #172033;
  font-weight: 600;
  text-align: right;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  max-width: 60%;
}
.studio-setup-card .setup-tip {
  margin-top: 8px;
  padding-top: 8px;
  border-top: 1px dashed #e2e8f0;
  font-size: 0.78rem;
  color: #475569;
  line-height: 1.4;
}
.studio-detected-card {
  margin: 0 0 10px;
  box-shadow: none;
}
.studio-crs-card {
  margin-top: 10px;
  padding: 10px 11px;
  border: 1px solid #dbe3ee;
  border-radius: 8px;
  background: #f8fafc;
}
.studio-crs-card .crs-head {
  display: flex;
  justify-content: space-between;
  gap: 8px;
  font-size: 0.78rem;
  font-weight: 700;
  color: #172033;
}
.studio-crs-card .crs-head span:last-child {
  color: #475569;
  font-weight: 600;
  text-align: right;
}
.studio-crs-card .crs-row {
  display: flex;
  justify-content: space-between;
  gap: 8px;
  margin-top: 5px;
  font-size: 0.76rem;
  color: #475569;
}
.studio-crs-card .crs-row span:last-child {
  color: #172033;
  text-align: right;
  overflow: hidden;
  text-overflow: ellipsis;
}
.studio-crs-card .crs-meaning {
  margin-top: 8px;
  padding-top: 8px;
  border-top: 1px dashed #dbe3ee;
  font-size: 0.76rem;
  line-height: 1.35;
  color: #334155;
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
.download-group { margin-top: 10px; }
.download-group:first-child { margin-top: 0; }
.download-group-label {
  font-size: 0.67rem;
  font-weight: 700;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: #94a3b8;
  margin-bottom: 5px;
}
.download-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
}
.download-grid .btn { width: 100%; font-size: 0.78rem; padding: 5px 8px; }
.download-grid .btn-span { grid-column: 1 / -1; }
.studio-card .btn {
  border-radius: 6px;
  font-weight: 600;
}
.studio-card .shiny-input-container:not(.shiny-input-container-inline) {
  width: 100%;
}

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
.workspace-tabs {
  position: relative;
}
.workspace-tabs > .tabbable > .nav-tabs {
  min-height: 44px;
  padding-right: 178px;
  display: flex;
  align-items: center;
  flex-wrap: wrap;
}
.workspace-tabs > .tabbable > .nav-tabs > li > a {
  margin-bottom: 0;
}
.workspace-offset-toggle {
  position: absolute;
  top: 0;
  right: 10px;
  min-height: 44px;
  z-index: 3;
  display: flex;
  align-items: center;
  justify-content: flex-end;
}
.workspace-offset-toggle .form-group,
.workspace-offset-toggle .checkbox {
  margin: 0;
}
.workspace-offset-toggle .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  min-height: 30px;
  padding: 4px 10px 4px 8px;
  border: 1px solid #d9e0ea;
  border-radius: 999px;
  background: #f8fafc;
  color: #475569;
  font-size: 0.82rem;
  font-weight: 600;
  line-height: 1;
  white-space: nowrap;
  box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
}
.workspace-offset-toggle .checkbox label:hover {
  background: #eef2f7;
  border-color: #cbd5e1;
}
.workspace-offset-toggle .checkbox input[type='checkbox'] {
  position: static;
  margin: 0;
}
@media (max-width: 700px) {
  .workspace-tabs > .tabbable > .nav-tabs {
    padding-right: 0;
  }
  .workspace-offset-toggle {
    position: static;
    min-height: 42px;
    justify-content: flex-end;
    padding: 6px 10px;
    border-right: 1px solid #d9e0ea;
    border-left: 1px solid #d9e0ea;
    background: #ffffff;
  }
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
  align-items: center;
  gap: 8px;
  margin: 0 0 10px;
  padding: 8px 10px;
  border: 1px solid #d9e0ea;
  border-radius: 8px;
  background: #f8fafc;
}
.preview-toolbar .btn {
  margin: 0;
  min-height: 32px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  border-radius: 999px;
  padding: 5px 12px;
  font-weight: 600;
}
.preview-toolbar .form-group,
.preview-toolbar .checkbox {
  margin: 0;
}
.preview-toolbar .checkbox label {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  min-height: 32px;
  margin: 0;
  padding: 5px 10px;
  border: 1px solid #dbe3ee;
  border-radius: 999px;
  background: #ffffff;
  color: #475569;
  font-size: 0.82rem;
  font-weight: 600;
  white-space: nowrap;
}
.preview-toolbar .checkbox input[type='checkbox'] {
  position: static;
  margin: 0;
}
@media (max-width: 520px) {
  .preview-toolbar {
    align-items: stretch;
  }
  .preview-toolbar .btn,
  .preview-toolbar .checkbox label {
    width: 100%;
  }
}
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
  visibility: hidden;
  pointer-events: none;
  transition: opacity 0.16s ease, visibility 0s linear 0.16s;
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


/* Hide studio glasstabs dropdowns while the app is busy */
body.studio-busy .gt-ms-dropdown,
body.studio-busy .gt-select-dropdown,
body.studio-busy .gt-dropdown,
body.studio-busy #legend_filter-dropdown,
body.studio-busy #label_filter-dropdown,
body.studio-busy #region_col-dropdown,
body.studio-busy #label_col-dropdown,
body.studio-busy #bloom_child_col-dropdown,
body.studio-busy #bloom_parents-dropdown,
body.studio-busy #annotation_mode-dropdown,
body.studio-busy #label_marker_shape-dropdown,
body.studio-busy #legend_position-dropdown,
body.studio-busy #connector_type-dropdown,
body.studio-busy #connector_linetype-dropdown,
body.studio-busy #connector_endpoint-dropdown,
body.studio-busy #movement_connector_linetype-dropdown,
body.studio-busy #movement_connector_endpoint-dropdown,
body.studio-busy #map_background-dropdown {
  display: none !important;
  visibility: hidden !important;
  pointer-events: none !important;
}

"

# ---- UI -----------------------------------------------------------------------

ui <- function(request) {
  debug_mode <- grepl("(^|&)debug=1(&|$)", request$QUERY_STRING %||% "")

  fluidPage(
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
        activeGeneration: null,
        readyGeneration: null,
        fallbackTimer: null
      };
      // The dotted bloom frame now has two gestures: drag moves the branch,
      // plain click compresses/unblooms the branch. The server separates
      // those by comparing the helper-reported offsets with current state.
      var studioBloomBoundaryBehavior = 'drag';
      var studioIgnoreRegionClicksUntil = 0;
      var studioAnimationFlushTimer = null;
      var studioAnimationState = {active: false, mode: null, parent: null};
      var studioDeferredVisuals = {
        labels: null,
        labelValues: null,
        legendValues: null,
        labelData: null,
        labelOptions: null,
        labelColors: null
      };
      function helperPostMessage(payload) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(payload, helperTargetOrigin());
        }
      }
      function scheduleStudioVisualFlush(delayMs) {
        window.clearTimeout(studioAnimationFlushTimer);
        studioAnimationFlushTimer = window.setTimeout(flushStudioDeferredVisuals, Math.max(0, delayMs || 0));
      }
      function hasDeferredStudioVisuals() {
        return !!(
          studioDeferredVisuals.labels ||
          studioDeferredVisuals.labelValues ||
          studioDeferredVisuals.legendValues ||
          studioDeferredVisuals.labelData ||
          studioDeferredVisuals.labelOptions ||
          studioDeferredVisuals.labelColors
        );
      }
      function flushStudioDeferredVisuals() {
        if (studioAnimationState.active) {
          scheduleStudioVisualFlush(80);
          return;
        }
        var pending = studioDeferredVisuals;
        studioDeferredVisuals = {
          labels: null,
          labelValues: null,
          legendValues: null,
          labelData: null,
          labelOptions: null,
          labelColors: null
        };
        if (pending.labelData) helperPostMessage(pending.labelData);
        if (pending.labels) helperPostMessage(pending.labels);
        if (pending.labelColors) helperPostMessage(pending.labelColors);
        if (pending.labelOptions) helperPostMessage(pending.labelOptions);
        if (pending.legendValues) helperPostMessage(pending.legendValues);
        if (pending.labelValues) helperPostMessage(pending.labelValues);
      }
      function postOrDeferStudioVisual(key, payload) {
        if (studioAnimationState.active) {
          studioDeferredVisuals[key] = payload;
          scheduleStudioVisualFlush(80);
          return;
        }
        helperPostMessage(payload);
      }
      function setStudioAnimationState(message) {
        message = message || {};
        studioAnimationState = {
          active: !!message.active,
          mode: message.mode || null,
          parent: message.parent || null
        };
        document.body.classList.toggle('studio-animating', studioAnimationState.active);
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('helper_animation_state', {
            active: studioAnimationState.active,
            mode: studioAnimationState.mode,
            parent: studioAnimationState.parent,
            at: Date.now()
          }, {priority: 'event'});
        }
        if (!studioAnimationState.active && hasDeferredStudioVisuals()) {
          scheduleStudioVisualFlush(30);
        }
      }
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
      function cleanupHelperCornerControls() {
        // The drag helper can render a tiny empty corner handle for its
        // internal side panel. Spatial Studio already provides the Offset
        // panel toggle in the tab row, so hide that stray empty helper control.
        var iframe = getHelperFrame();
        if (!iframe || !iframe.contentWindow) return;
        var doc = null;
        try {
          doc = iframe.contentDocument || iframe.contentWindow.document;
        } catch (e) {
          return;
        }
        if (!doc || !doc.body) return;

        if (!doc.getElementById('studio-helper-corner-cleanup-style')) {
          var style = doc.createElement('style');
          style.id = 'studio-helper-corner-cleanup-style';
          style.textContent = '.studio-hidden-corner-control { display: none !important; }';
          (doc.head || doc.documentElement).appendChild(style);
        }

        var nodes = doc.querySelectorAll(
          'input[type=checkbox], label, button, .btn, .checkbox, .form-check, [role=button]'
        );
        nodes.forEach(function(node) {
          var rect = node.getBoundingClientRect();
          if (!rect || rect.width === 0 || rect.height === 0) return;
          var text = (node.textContent || '').replace(/\\s+/g, ' ').trim();
          var title = (
            node.getAttribute('title') ||
            node.getAttribute('aria-label') ||
            ''
          ).trim();
          var isTopLeft = rect.left >= -2 && rect.left <= 64 && rect.top >= -2 && rect.top <= 64;
          var isSmall = rect.width <= 72 && rect.height <= 44;
          var isEmpty = !text && !title;
          if (isTopLeft && isSmall && isEmpty) {
            node.classList.add('studio-hidden-corner-control');
          }
        });
      }
      function scheduleHelperCornerCleanup() {
        window.setTimeout(cleanupHelperCornerControls, 0);
        window.setTimeout(cleanupHelperCornerControls, 250);
        window.setTimeout(cleanupHelperCornerControls, 900);
      }
      var studioBoundaryLabelPrefix = 'Drag to';
      function scheduleHelperBoundaryLabelRewrite() {
        // No late DOM text rewrite here. The helper HTML is rewritten before
        // its JavaScript runs in drag_map_prototype(), which removes the
        // visible boundary-label flash.
      }
      function closeStudioDropdowns() {
  var dropdownSelector = [
    '.gt-ms-dropdown',
    '.gt-select-dropdown',
    '.gt-dropdown',
    '#legend_filter-dropdown',
    '#label_filter-dropdown',
    '#region_col-dropdown',
    '#label_col-dropdown',
    '#bloom_child_col-dropdown',
    '#bloom_parents-dropdown',
    '#annotation_mode-dropdown',
    '#label_marker_shape-dropdown',
    '#legend_position-dropdown',
    '#connector_type-dropdown',
    '#connector_linetype-dropdown',
    '#connector_endpoint-dropdown',
    '#movement_connector_linetype-dropdown',
    '#movement_connector_endpoint-dropdown',
    '#map_background-dropdown'
  ].join(', ');

  document.querySelectorAll(dropdownSelector).forEach(function(dropdown) {
    dropdown.classList.remove('open', 'show', 'active');
    dropdown.setAttribute('aria-hidden', 'true');
  });

  document.querySelectorAll(
    '.gt-ms-wrap, .gt-select-wrap, .gt-open, .gt-ms-open, .gt-select-open'
  ).forEach(function(node) {
    if (node.classList) {
      node.classList.remove('gt-open', 'gt-ms-open', 'gt-select-open', 'open', 'show');
    }
  });
}

// ---- Unified full-app busy veil ----------------------------------------
// One blocker, two modes: loading (reading spatial data) and
// processing (rebuilding the drag helper). lockCount stops an early
// task from hiding the veil while a later task is still running.
var studioBusy = {
  mode: null,
  generation: null,
  lockCount: 0,
  showTimer: null,
  hideTimer: null,
  safetyTimer: null,
  shownAt: null,
  minVisibleMs: 550,
  showDelayMs: 120
};

function setBusyText(mode) {
  var title = document.querySelector('.studio-load-panel strong');
  var body = document.querySelector('.studio-load-panel span');
  if (!title || !body) return;
  if (mode === 'loading') {
    title.textContent = 'Loading spatial data';
    body.textContent = 'Reading geometry, columns, labels, and map state...';
    return;
  }
  if (mode === 'processing') {
    title.textContent = 'Processing changes';
    body.textContent = 'Updating the drag map and syncing controls...';
    return;
  }
  title.textContent = 'Working';
  body.textContent = 'Please wait while the app finishes the current task...';
}

function applyGlobalBusy(active, mode) {
  document.body.classList.toggle('studio-busy', !!active);
  document.body.classList.toggle('studio-busy-loading',
    !!active && mode === 'loading');
  document.body.classList.toggle('studio-busy-processing',
    !!active && mode === 'processing');
  // Drives the existing .studio-load-veil CSS.
  document.body.classList.toggle('dragmapr-loading', !!active);
  var appRoot = document.querySelector('.container-fluid');
  if (appRoot) {
    if (active) {
      appRoot.setAttribute('aria-busy', 'true');
      appRoot.setAttribute('aria-disabled', 'true');
    } else {
      appRoot.removeAttribute('aria-busy');
      appRoot.removeAttribute('aria-disabled');
    }
  }
}

function requestStudioBusy(mode, generation) {
  mode = mode || 'processing';
  window.clearTimeout(studioBusy.hideTimer);
  window.clearTimeout(studioBusy.showTimer);
  window.clearTimeout(studioBusy.safetyTimer);
  studioBusy.mode = mode;
  studioBusy.generation = generation || null;
  studioBusy.lockCount = Math.max(1, studioBusy.lockCount + 1);
  closeStudioDropdowns();
  if (document.activeElement && document.activeElement.blur) {
    document.activeElement.blur();
  }
  setBusyText(mode);
  // Never let a missed release freeze the app forever.
  studioBusy.safetyTimer = window.setTimeout(function() {
    releaseStudioBusy(null, true);
  }, 120000);
  if (mode === 'loading') {
    // Loading shows immediately.
    studioBusy.shownAt = Date.now();
    applyGlobalBusy(true, mode);
    return;
  }
  // Processing waits briefly to avoid flashes on very fast rebuilds.
  studioBusy.showTimer = window.setTimeout(function() {
    studioBusy.shownAt = Date.now();
    applyGlobalBusy(true, mode);
  }, studioBusy.showDelayMs);
}

function releaseStudioBusy(generation, force) {
  if (generation && studioBusy.generation &&
      generation !== studioBusy.generation && !force) {
    return;
  }
  studioBusy.lockCount = Math.max(0, studioBusy.lockCount - 1);
  if (studioBusy.lockCount > 0 && !force) return;
  studioBusy.lockCount = 0;
  window.clearTimeout(studioBusy.showTimer);
  window.clearTimeout(studioBusy.safetyTimer);
  var elapsed = studioBusy.shownAt ? Date.now() - studioBusy.shownAt : 9999;
  var wait = Math.max(0, studioBusy.minVisibleMs - elapsed);
  window.clearTimeout(studioBusy.hideTimer);
  studioBusy.hideTimer = window.setTimeout(function() {
    applyGlobalBusy(false, null);
    studioBusy.mode = null;
    studioBusy.generation = null;
    studioBusy.shownAt = null;
  }, wait);
}

// Immediate feedback when a rebuild-triggering control changes. Only used
// for inputs that are normally followed by a helper rebuild (which
// re-claims the veil with a generation and later releases it). The short
// safety timer covers the rare case where no rebuild starts, e.g. an
// invalid bloom column.
function preShowHelperBusy() {
  requestStudioBusy('processing', null);
  window.clearTimeout(studioBusy.safetyTimer);
  studioBusy.safetyTimer = window.setTimeout(function() {
    if (studioBusy.mode === 'processing' && studioBusy.generation === null) {
      releaseStudioBusy(null, true);
    }
  }, 4000);
}
      function markHelperReady(generation) {
        generation = helperGeneration(generation);
        if (!generation || generation !== helperState.activeGeneration) {
          return;
        }
        window.clearTimeout(helperState.fallbackTimer);
        helperState.readyGeneration = generation;
        releaseStudioBusy(generation, true);
        scheduleHelperCornerCleanup();
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
      document.addEventListener('click', function(event) {
        var control = event.target.closest('.gt-ms-option, .gt-ms-all, .gt-ms-clear');
        if (!control) return;
        var dropdown = control.closest('.gt-ms-dropdown');
        var inputId = null;
        if (dropdown && dropdown.id && dropdown.id.endsWith('-dropdown')) {
          inputId = dropdown.id.slice(0, -'-dropdown'.length);
        } else {
          var wrap = control.closest('#legend_filter-wrap, #label_filter-wrap');
          inputId = wrap ? wrap.getAttribute('data-input-id') : null;
        }
        if (inputId !== 'legend_filter' && inputId !== 'label_filter') return;
        window.setTimeout(function() {
          var source = document.getElementById(inputId + '-dropdown') ||
            document.getElementById(inputId + '-wrap');
          if (!source) return;
          var values = Array.from(source.querySelectorAll('.gt-ms-option.checked')).map(function(option) {
            return option.getAttribute('data-value');
          });
          Shiny.setInputValue(
            'studio_filter_change',
            {id: inputId, values: values, at: Date.now()},
            {priority: 'event'}
          );
        }, 400);
      });
      Shiny.addCustomMessageHandler('dragmapr-labels', function(message) {
        postOrDeferStudioVisual('labels', {
          type: 'dragmapr-set-labels',
          labels: !!message.labels
        });
      });
      Shiny.addCustomMessageHandler('dragmapr-legend-values', function(message) {
        postOrDeferStudioVisual('legendValues', {
          type: 'dragmapr-set-legend-values',
          values: message.values
        });
      });
      Shiny.addCustomMessageHandler('dragmapr-label-values', function(message) {
        postOrDeferStudioVisual('labelValues', {
          type: 'dragmapr-set-label-values',
          values: message.values
        });
      });
      Shiny.addCustomMessageHandler('dragmapr-side-panel', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage(
            {type: 'dragmapr-set-side-panel', sidePanel: !!message.sidePanel}, helperTargetOrigin()
          );
          scheduleHelperCornerCleanup();
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-reset-view', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow)
          iframe.contentWindow.postMessage({type: 'dragmapr-reset-view'}, helperTargetOrigin());
      });
      Shiny.addCustomMessageHandler('dragmapr-reset-state', function(message) {
        var iframe = getHelperFrame();
        if (iframe && iframe.contentWindow) {
          iframe.contentWindow.postMessage({type: 'dragmapr-reset-state'}, helperTargetOrigin());
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-bloom', function(message) {
        message = message || {};
        studioBloomBoundaryBehavior = String(message.boundaryBehavior || 'drag').toLowerCase();
        studioBoundaryLabelPrefix = message.boundaryLabel || 'Drag to';
        helperPostMessage({
          type: 'dragmapr-set-bloom',
          expanded: message.expanded || [],
          boundary: message.boundary !== false,
          boundaryBehavior: studioBloomBoundaryBehavior,
          boundaryLabel: message.boundaryLabel || 'Drag to',
          boundaryDragThreshold: message.boundaryDragThreshold || 8,
          regionOffsets: message.regionOffsets || []
        });
        scheduleHelperBoundaryLabelRewrite();
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
        postOrDeferStudioVisual('labelData', {
          type: 'dragmapr-set-label-data',
          labels: message.labels || []
        });
      });
      Shiny.addCustomMessageHandler('dragmapr-label-options', function(message) {
        postOrDeferStudioVisual('labelOptions', {
          type: 'dragmapr-set-label-options',
          options: message.options || {}
        });
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
        postOrDeferStudioVisual('labelColors', {
          type: 'dragmapr-set-label-colors',
          colors: message.colors || {}
        });
      });
      Shiny.addCustomMessageHandler('dragmapr-loading', function(message) {
        var active = !!(message && message.active);
        if (active) {
          requestStudioBusy('loading', null);
        } else {
          releaseStudioBusy(null, true);
        }
      });
      Shiny.addCustomMessageHandler('dragmapr-helper-loading', function(message) {
        var active = !!(message && message.active);
        var generation = helperGeneration(message && message.generation);
        if (active) {
          helperState.activeGeneration = generation;
          helperState.readyGeneration = null;
          scheduleHelperFallback(generation);
          requestStudioBusy('processing', generation);
        } else if (!generation || generation === helperState.activeGeneration) {
          window.clearTimeout(helperState.fallbackTimer);
          helperState.activeGeneration = null;
          helperState.readyGeneration = null;
          releaseStudioBusy(generation, true);
        }
      });
      // Full-page loading is controlled from the Shiny server via
      // the dragmapr-loading custom message. Avoid also starting it from
      // raw browser events, because that can overlap with the helper overlay.

      window.addEventListener('message', function(event) {
        var iframe = getHelperFrame();
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-ready') {
          markHelperReady(event.data.generation);
          scheduleHelperBoundaryLabelRewrite();
        }
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-animation-state') {
          setStudioAnimationState(event.data);
        }
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-boundary-drag') {
          // A real dotted-frame drag is sent separately from a click. Save the
          // reported offsets and keep the branch open.
          studioIgnoreRegionClicksUntil = Date.now() + 220;
          Shiny.setInputValue('boundary_drag_offsets', {
            parent: event.data.parent || null,
            regionOffsets: event.data.regionOffsets || [],
            nonce: Date.now()
          }, {priority: 'event'});
        }
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-collapse-branch') {
          // Plain click on the dotted frame. The helper starts the reversible
          // branch-return animation locally and sends the latest offsets so the
          // server can collapse without an abrupt visual jump.
          studioIgnoreRegionClicksUntil = Date.now() + 220;
          Shiny.setInputValue('collapse_branch', {
            parent: event.data.parent || null,
            regionOffsets: event.data.regionOffsets || [],
            boundaryBehavior: studioBloomBoundaryBehavior,
            nonce: Date.now()
          }, {priority: 'event'});
        }
        if (iframe && event.source === iframe.contentWindow &&
            event.data && event.data.type === 'dragmapr-region-click') {
          if (Date.now() < studioIgnoreRegionClicksUntil) return;
          // Plain click (no drag) on a region: used to bloom a parent
          // into its children or compress an expanded branch.
          Shiny.setInputValue('helper_region_click', {
            region: event.data.region || null,
            regionOffsets: event.data.regionOffsets || [],
            nonce: Date.now()
          }, {priority: 'event'});
        }
      });
      document.addEventListener('load', function(event) {
        if (event.target && event.target.matches &&
            event.target.matches('iframe.studio-helper-frame')) {
          scheduleHelperFallback(currentHelperGeneration());
          scheduleHelperCornerCleanup();
          scheduleHelperBoundaryLabelRewrite();
        }
      }, true);
      document.addEventListener('click', function(event) {
        var scriptButton = event.target && event.target.closest &&
          event.target.closest('#download_script');
        if (scriptButton) {
          Shiny.setInputValue('script_download_requested', Date.now(), {priority: 'event'});
        }
      }, true);
      document.addEventListener('keydown', function(event) {
        if (!event.ctrlKey && !event.metaKey) return;
        var tag = (event.target || document.activeElement || {}).tagName || '';
        if (/^(INPUT|TEXTAREA|SELECT)$/i.test(tag)) return;
        if (event.key === 'Escape') {
          event.preventDefault();
          Shiny.setInputValue('reset_view_requested', Date.now(), {priority: 'event'});
          return;
        }
        if (event.key === 'z' || event.key === 'Z') {
          event.preventDefault();
          var btn = document.getElementById(event.shiftKey ? 'redo_layout' : 'undo_layout');
          if (btn && !btn.disabled) btn.click();
        } else if (event.key === 'y' || event.key === 'Y') {
          event.preventDefault();
          var btn = document.getElementById('redo_layout');
          if (btn && !btn.disabled) btn.click();
        }
      });
      if (window.jQuery) {
        // Only inputs that trigger a full helper rebuild. Presentation
        // controls (filters, styles, background) update the live iframe
        // over postMessage and must not lock the app.
        var rebuildInputs = [
          'region_col',
          'label_col',
          'bloom_child_col'
        ];
        // Re-rendered panels re-register their inputs and fire
        // shiny:inputchanged with UNCHANGED values (e.g. the Bloom panel
        // refreshes after each expansion). Only treat a genuine value
        // change as a rebuild trigger, otherwise client-side blooms would
        // flash a pointless processing veil.
        var lastRebuildInputValues = {};
        jQuery(document).on('shiny:inputchanged', function(event) {
          if (!event || !event.name) return;
          if (rebuildInputs.indexOf(event.name) < 0) return;
          var serialized;
          try {
            serialized = JSON.stringify(event.value == null ? null : event.value);
          } catch (e) {
            serialized = String(event.value);
          }
          var seen = Object.prototype.hasOwnProperty.call(
            lastRebuildInputValues, event.name
          );
          if (seen && lastRebuildInputValues[event.name] === serialized) {
            return;  // re-registration, not a user change
          }
          lastRebuildInputValues[event.name] = serialized;
          if (!seen) return;  // first registration at startup is a baseline
          preShowHelperBusy();
        });
      }
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

        studio_sidebar_panel(
          "Start",
          "Load data and review setup",
          open = TRUE,
          fileInput(
            "spatial_upload", "Upload spatial file",
            multiple = TRUE,
            accept = c(".zip", ".shp", ".dbf", ".shx", ".prj", ".cpg", ".gpkg", ".geojson", ".json")
          ),
          tags$div(
            class = "studio-action-row",
            actionButton("load_demo", "Use bundled HHS demo", class = "btn-sm btn-default")
          ),
          tags$div("Open saved project", class = "studio-subgroup-title"),
          tags$p(
            "Reopen a Project ZIP previously downloaded from Spatial Studio.",
            class = "studio-help"
          ),
          fileInput(
            "project_upload", "Open project ZIP",
            multiple = FALSE,
            accept = ".zip"
          ),
          tags$div(class = "studio-divider"),
          tags$div("Detected setup", class = "studio-subgroup-title"),
          uiOutput("start_summary")
        ),

        studio_sidebar_panel(
          "Grouping & bloom",
          "What to group by and what it expands into",
          open = TRUE,
          uiOutput("column_controls"),
          tags$div(class = "studio-divider"),
          uiOutput("bloom_controls")
        ),

        studio_sidebar_panel(
          "Labels & text boxes",
          "Short names on the map, or callout notes",
          open = TRUE,
          tags$p(
            "Labels are short names. Text boxes are notes or callouts.",
            class = "studio-help"
          ),
          tags$div("Visibility", class = "studio-subgroup-title"),
          checkboxInput("show_labels", "Show labels", value = TRUE),
          checkboxInput("label_auto_limit", "Auto-limit crowded labels", value = TRUE),
          numericInput(
            "label_auto_limit_n", "Auto label limit",
            value = LABEL_AUTO_LIMIT, min = LABEL_AUTO_MIN,
            max = LABEL_AUTO_MAX, step = 1
          ),
          uiOutput("label_filter_ui"),
          tags$div(class = "studio-divider"),
          tags$div("Label text", class = "studio-subgroup-title"),
          uiOutput("label_editor_ui"),
          tags$div(class = "studio-divider"),
          studio_select(
            "annotation_mode", "Label style",
            choices  = c("Short labels" = "labels", "Text boxes" = "boxes"),
            selected = "labels",
            placeholder = "Choose label style"
          ),
          tags$div(class = "studio-field-gap"),
          checkboxInput("show_connectors", "Show connector lines", value = FALSE),
          checkboxInput(
            "connector_smart",
            tags$span(
              "Smart connector style",
              title = "Choose straight, elbow, or curved connector paths from the current label displacement."
            ),
            value = FALSE
          ),
          tags$p(
            "Smart style chooses connector geometry from the current label displacement.",
            class = "studio-help"
          )
        ),

        studio_sidebar_panel(
          "Appearance",
          "Colors, legend, label style, and background",
          tags$div("Colors", class = "studio-subgroup-title"),
          uiOutput("color_pickers"),
          tags$div(class = "studio-field-gap"),
          uiOutput("label_color_ui"),
          tags$div(class = "studio-divider"),
          tags$div("Legend", class = "studio-subgroup-title"),
          checkboxInput("show_legend", "Show legend in drag and preview", value = TRUE),
          checkboxInput("legend_reflect_bloom", "Legend reflects bloom", value = FALSE),
          checkboxInput("legend_show_all", "Allow more than 25 legend keys", value = FALSE),
          uiOutput("legend_filter_ui"),
          textInput("legend_title", "Legend title", value = "Region"),
          studio_select(
            "legend_position",
            "Legend position",
            choices = c("Bottom" = "bottom", "Top" = "top", "Left" = "left", "Right" = "right", "None" = "none"),
            selected = "bottom",
            placeholder = "Choose legend position"
          ),
          tags$div(class = "studio-divider"),
          tags$div("Label appearance", class = "studio-subgroup-title"),
          studio_select(
            "label_marker_shape", "Text label marker",
            choices = c("Circle" = "circle", "Rounded box" = "rect", "Text only" = "none"),
            selected = "circle",
            placeholder = "Choose marker style"
          ),
          tags$div(class = "studio-field-gap"),
          sliderInput("label_text_size", "Text size (px)",
                      min = 7, max = 22, value = 11, step = 1),
          uiOutput("label_size_controls"),
          tags$div(class = "studio-divider"),
          tags$div("Map background", class = "studio-subgroup-title"),
          studio_select(
            "map_background", "Map background",
            choices = c("White" = "white", "Transparent" = "transparent",
                        "Light grid" = "light_grid", "Dark" = "dark"),
            selected = "white",
            placeholder = "Choose map background"
          )
        ),

        studio_sidebar_panel(
          "Export & project",
          "Save your work and download outputs",
          tags$div("Static output", class = "studio-subgroup-title"),
          textInput("static_title", "Static map title", value = "dragmapr spatial studio"),
          actionButton("apply_static_title", "Apply title", class = "btn-sm btn-primary"),
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
          tags$div(class = "studio-divider"),
          tags$div("Downloads", class = "studio-subgroup-title"),

          tags$div(
            class = "download-group",
            tags$div("Map output", class = "download-group-label"),
            tags$div(
              class = "download-grid",
              downloadButton("download_png", "PNG", class = "btn-default btn-span")
            )
          ),

          tags$div(
            class = "download-group",
            tags$div("Data files", class = "download-group-label"),
            tags$div(
              class = "download-grid",
              downloadButton("download_region_csv", "Region CSV"),
              downloadButton("download_label_csv",  "Label CSV"),
              downloadButton("download_labels",     "Labels table"),
              downloadButton("download_geojson",    "GeoJSON"),
              downloadButton("download_gpkg",       "GPKG")
            )
          ),

          tags$div(
            class = "download-group",
            tags$div("Code & bundles", class = "download-group-label"),
            tags$div(
              class = "download-grid",
              downloadButton(
                "download_script", "R script",
                onclick = "Shiny.setInputValue('script_download_requested', Date.now(), {priority: 'event'});"
              ),
              downloadButton("download_html",          "HTML helper"),
              downloadButton("download_bundle",        "Project ZIP"),
              downloadButton("download_static_bundle", "Static bundle")
            )
          )
        ),

        studio_sidebar_panel(
          "Advanced",
          "Connector styling and movement context layers",
          tags$div("Connector line style", class = "studio-subgroup-title"),
          studio_color_input("connector_color", "Line color", value = "#334155"),
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
                      min = 0.25, max = 2.5, value = 0.45, step = 0.05),
          tags$div(class = "studio-divider"),
          tags$div("Context layers", class = "studio-subgroup-title"),
          checkboxInput("show_origin_outlines", "Show origin outlines", value = FALSE),
          checkboxInput("show_movement_connectors", "Show movement connectors", value = FALSE),
          checkboxInput("show_movement_band", "Show swept movement shadow", value = FALSE),
          checkboxInput("show_drag_trail", "Show drag preview trail", value = FALSE),
          tags$div(class = "studio-divider"),
          tags$div("Movement connector style", class = "studio-subgroup-title"),
          studio_color_input("movement_connector_color", "Movement connector color", value = "#64748b"),
          sliderInput("movement_connector_opacity", "Movement connector opacity",
                      min = 0, max = 1, value = 0.72, step = 0.05),
          sliderInput("movement_connector_linewidth", "Movement connector thickness",
                      min = 0.25, max = 3, value = 0.45, step = 0.05),
          studio_select(
            "movement_connector_linetype", "Movement connector line style",
            choices = c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted"),
            selected = "solid",
            placeholder = "Choose movement line style"
          ),
          tags$div(class = "studio-field-gap"),
          studio_select(
            "movement_connector_endpoint", "Movement connector arrow",
            choices = c("Closed arrow" = "closed", "Open arrow" = "open", "None" = "none"),
            selected = "closed",
            placeholder = "Choose movement arrow"
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
        tags$div(
          class = "workspace-tabs",
          tabsetPanel(
            id = "workspace_tab",
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
                checkboxInput("auto_preview", "Auto-refresh after dragging", value = TRUE)
              ),
              plotOutput("preview", height = 620)
            ),
            if (isTRUE(debug_mode)) tabPanel(
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
          ),
          conditionalPanel(
            condition = "input.workspace_tab === 'Drag'",
            class = "workspace-offset-toggle",
            checkboxInput("show_helper_panel", "Offset panel", value = TRUE)
          )
        )
      )
    )
  )
}

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
    region_csv_cache      = NULL,  # child/user drag delta for current column
    region_base_csv_cache = NULL,  # inherited parent placement for current column
    label_csv_cache       = NULL,
    column_offset_store = list(),  # per-column {region, label} cache keyed by region_context_key()
    active_region_path = NULL,     # character vector: e.g. c("COUNTY", "MUN") when hierarchy active
    column_ui_nonce = 0L,          # bumped by "Use recommended grouping"
    bloom_parents = character(),   # expanded parent keys (max BLOOM_MAX_PARENTS)
    bloom_child_col_active = NULL, # child column currently in effect for bloom
    bloom_version = 0L,            # bumped to force a helper rebuild
    label_edits      = list(),
    region_palette_override = NULL,
    label_palette_override  = NULL,
    static_title = "dragmapr spatial studio",
    pending_region_col = NULL,
    pending_label_col  = NULL,
    legend_filter_selection = NULL,
    label_filter_selection = NULL,
    column_select_signature = NULL,
    undo_stack = list(),
    redo_stack = list(),
    history_armed = FALSE,
    history_ignore_until = NULL,
    boundary_drag_ignore_until = NULL,
    boundary_drag_parent = NULL,
    restoring_history = FALSE,
    animation_active = FALSE,
    preview_refresh_pending = FALSE
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
    session$onFlushed(function() {
      helper_busy    <- isolate(isTRUE(state$helper_loading) || isTRUE(state$helper_building))
      current_ver    <- isolate(state$source_version)
      current_sig    <- isolate(state$helper_signature)
      # A rebuild is imminent when the signature source_version is behind the
      # current source version - the prototype observer will fire next flush.
      rebuild_pending <- is.null(current_sig) ||
        !identical(current_sig$source_version, current_ver)
      if (!helper_busy && !rebuild_pending) {
        set_loading(FALSE)
      }
      # Otherwise the veil stays until helper_ready_token fires.
    }, once = TRUE)
  }

  clear_project_state <- function() {
    state$region_csv_cache <- NULL
    state$region_base_csv_cache <- NULL
    state$label_csv_cache <- NULL
    state$active_region_path <- NULL
    state$label_edits <- list()
    state$region_palette_override <- NULL
    state$label_palette_override <- NULL
    state$pending_region_col <- NULL
    state$pending_label_col <- NULL
    state$legend_filter_selection <- NULL
    state$label_filter_selection <- NULL
    state$column_select_signature <- NULL
    state$undo_stack <- list()
    state$redo_stack <- list()
    state$history_armed <- FALSE
    state$history_ignore_until <- NULL
    state$boundary_drag_ignore_until <- NULL
    state$boundary_drag_parent <- NULL
    state$helper_signature <- NULL
    state$column_offset_store <- list()
  }

  empty_region_offsets <- function(groups) {
    data.frame(region = as.character(groups), dx_m = 0, dy_m = 0,
               stringsAsFactors = FALSE)
  }

  align_region_offsets <- function(offsets, groups) {
    groups <- as.character(groups)
    out <- empty_region_offsets(groups)
    if (is.null(offsets) || !is.data.frame(offsets) || nrow(offsets) == 0L) return(out)
    names(offsets) <- tolower(names(offsets))
    if (!all(c("region", "dx_m", "dy_m") %in% names(offsets))) return(out)
    idx <- match(out$region, as.character(offsets$region))
    hit <- !is.na(idx)
    out$dx_m[hit] <- suppressWarnings(as.numeric(offsets$dx_m[idx[hit]]))
    out$dy_m[hit] <- suppressWarnings(as.numeric(offsets$dy_m[idx[hit]]))
    out$dx_m[!is.finite(out$dx_m)] <- 0
    out$dy_m[!is.finite(out$dy_m)] <- 0
    out
  }

  combine_region_offsets <- function(base, delta, groups) {
    base  <- align_region_offsets(base,  groups)
    delta <- align_region_offsets(delta, groups)
    data.frame(region = as.character(groups),
               dx_m = base$dx_m + delta$dx_m,
               dy_m = base$dy_m + delta$dy_m,
               stringsAsFactors = FALSE)
  }

  shift_label_table_by_offsets <- function(labels, offsets) {
    if (is.null(labels) || !is.data.frame(labels) || nrow(labels) == 0L) return(labels)
    if (is.null(offsets) || !is.data.frame(offsets) || nrow(offsets) == 0L) return(labels)
    if (!all(c("region", "dx_m", "dy_m") %in% names(offsets))) return(labels)
    out <- labels
    idx <- match(as.character(out$region), as.character(offsets$region))
    hit <- !is.na(idx)
    if ("x" %in% names(out)) out$x[hit] <- out$x[hit] + offsets$dx_m[idx[hit]]
    if ("y" %in% names(out)) out$y[hit] <- out$y[hit] + offsets$dy_m[idx[hit]]
    out
  }

  INTERNAL_REGION_COL <- "..dragmapr_region_key.."
  BLOOM_MAX_PARENTS   <- 2L  # keep the page readable: at most two expanded parents

  clean_group_value <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- "(missing)"
    x
  }

  region_context_key <- function(path) {
    paste(as.character(path), collapse = "")
  }

  make_region_path <- function(x, path) {
    path <- as.character(path)
    path <- path[path %in% names(x)]
    if (length(path) == 0L) return(character(nrow(x)))
    if (length(path) == 1L) return(clean_group_value(x[[path[1L]]]))
    parts <- lapply(path, function(col) paste0(col, "=", clean_group_value(x[[col]])))
    do.call(paste, c(parts, sep = " | "))
  }

  clean_region_display <- function(keys, max_chars = 42L, include_path = FALSE) {
    keys <- as.character(keys)
    vapply(keys, function(key) {
      parts <- strsplit(key, " | ", fixed = TRUE)[[1L]]
      values <- sub("^[^=]+=", "", parts)
      text <- if (isTRUE(include_path) && length(values) > 1L) {
        paste(values, collapse = " / ")
      } else {
        utils::tail(values, 1L)
      }
      text <- trimws(text)
      if (!nzchar(text)) text <- "(missing)"
      if (nchar(text, type = "width") > max_chars) {
        paste0(substr(text, 1L, max(1L, max_chars - 3L)), "...")
      } else {
        text
      }
    }, character(1L), USE.NAMES = FALSE)
  }

  clean_region_choices <- function(keys, max_chars = 42L) {
    keys <- as.character(keys)
    labels <- clean_region_display(keys, max_chars = max_chars)
    dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    if (any(dup)) {
      labels[dup] <- clean_region_display(keys[dup], max_chars = max_chars, include_path = TRUE)
    }
    stats::setNames(keys, labels)
  }

  inherit_offsets_across_paths <- function(x, old_path, new_path, old_offsets) {
    if (is.null(old_offsets) || !is.data.frame(old_offsets) || nrow(old_offsets) == 0L) return(NULL)
    old_path <- as.character(old_path)
    new_path <- as.character(new_path)
    if (!all(old_path %in% names(x)) || !all(new_path %in% names(x))) return(NULL)
    old_groups <- make_region_path(x, old_path)
    new_groups <- make_region_path(x, new_path)
    match_idx <- match(old_groups, as.character(old_offsets$region))
    row_dx <- ifelse(is.na(match_idx), 0,
                     suppressWarnings(as.numeric(old_offsets$dx_m[match_idx])))
    row_dy <- ifelse(is.na(match_idx), 0,
                     suppressWarnings(as.numeric(old_offsets$dy_m[match_idx])))
    row_dx[!is.finite(row_dx)] <- 0
    row_dy[!is.finite(row_dy)] <- 0
    out <- stats::aggregate(
      cbind(dx_m, dy_m) ~ region,
      data = data.frame(region = new_groups, dx_m = row_dx, dy_m = row_dy,
                        stringsAsFactors = FALSE),
      FUN = mean
    )
    out$region <- as.character(out$region)
    out
  }

  # ---- Selective bloom expansion -------------------------------------------
  # The user picks a child column ("bloom into") and up to BLOOM_MAX_PARENTS
  # parent regions. Only those parents are subdivided into children; the rest
  # of the map keeps the parent grouping. Everything is keyed by the same
  # composite region paths the helper uses as drag_region values, so this is
  # entirely column-name agnostic.

  bloom_child_keys <- function(x, path, child_col) {
    make_region_path(x, unique(c(as.character(path), child_col)))
  }

  # Adopt an offsets snapshot sent by the helper alongside click/collapse
  # messages, so bloom math never runs on polling-lagged state. Shiny
  # deserializes the JSON array as a data frame (simplifyDataFrame), but a
  # plain list of rows is also accepted defensively.
  normalize_offset_rows_from_helper <- function(rows) {
    if (is.null(rows) || length(rows) == 0L) {
      return(NULL)
    }

    df <- NULL

    if (is.data.frame(rows)) {
      if (all(c("region", "dx_m", "dy_m") %in% names(rows))) {
        df <- data.frame(
          region = as.character(rows$region),
          dx_m   = suppressWarnings(as.numeric(rows$dx_m)),
          dy_m   = suppressWarnings(as.numeric(rows$dy_m)),
          stringsAsFactors = FALSE
        )
      }
    } else if (is.list(rows)) {
      df <- tryCatch(
        data.frame(
          region = vapply(rows, function(r) as.character(r$region %||% ""), character(1L)),
          dx_m = vapply(rows, function(r) {
            suppressWarnings(as.numeric(r$dx_m %||% 0))
          }, numeric(1L)),
          dy_m = vapply(rows, function(r) {
            suppressWarnings(as.numeric(r$dy_m %||% 0))
          }, numeric(1L)),
          stringsAsFactors = FALSE
        ),
        error = function(e) NULL
      )
    }

    if (is.null(df)) {
      return(NULL)
    }

    df$region[is.na(df$region)] <- ""
    df$region <- trimws(df$region)
    df$dx_m[!is.finite(df$dx_m)] <- 0
    df$dy_m[!is.finite(df$dy_m)] <- 0

    df <- df[nzchar(df$region), , drop = FALSE]
    df <- df[!duplicated(df$region, fromLast = TRUE), , drop = FALSE]

    rownames(df) <- NULL
    df
  }

  merge_region_offset_rows <- function(incoming) {
    if (is.null(incoming) || !is.data.frame(incoming) || nrow(incoming) == 0L) {
      return(invisible(FALSE))
    }

    existing <- bloom_offset_df()

    # Remove only the regions that the helper just reported.
    # Keep every other parent/child offset untouched.
    existing <- existing[
      !(as.character(existing$region) %in% as.character(incoming$region)),
      ,
      drop = FALSE
    ]

    # Do not store zero rows, but a zero row from incoming still clears that
    # specific region because we removed it from existing above.
    incoming <- incoming[
      incoming$dx_m != 0 | incoming$dy_m != 0,
      ,
      drop = FALSE
    ]

    out <- rbind(existing, incoming)
    out <- out[nzchar(as.character(out$region)), , drop = FALSE]
    out <- out[!duplicated(out$region, fromLast = TRUE), , drop = FALSE]

    state$region_csv_cache <- if (nrow(out) > 0L) csv_text(out) else NULL

    invisible(TRUE)
  }

  apply_helper_offset_rows <- function(rows) {
    incoming <- normalize_offset_rows_from_helper(rows)
    merge_region_offset_rows(incoming)
  }
  # Current region offsets as a clean data frame (possibly empty).
  bloom_offset_df <- function() {
    df <- tryCatch(read_csv_text(state$region_csv_cache), error = function(e) NULL)
    if (is.null(df) || !all(c("region", "dx_m", "dy_m") %in% names(df))) {
      return(data.frame(region = character(), dx_m = numeric(), dy_m = numeric(),
                        stringsAsFactors = FALSE))
    }
    df <- df[, c("region", "dx_m", "dy_m")]
    df$region <- as.character(df$region)
    df$dx_m <- suppressWarnings(as.numeric(df$dx_m))
    df$dy_m <- suppressWarnings(as.numeric(df$dy_m))
    df$dx_m[!is.finite(df$dx_m)] <- 0
    df$dy_m[!is.finite(df$dy_m)] <- 0
    df
  }

  # Single source of truth for expanding/collapsing bloomed parents.
  # Saves the parent offset before expansion, hands it to the children,
  # restores it on collapse, and pushes the new bloom state to the live
  # helper iframe (no rebuild, no loading overlay).
  set_bloom_parents <- function(new_sel, notify = TRUE, path = NULL) {
    new_sel <- unique(as.character(new_sel %||% character()))
    new_sel <- new_sel[!is.na(new_sel) & nzchar(new_sel)]
    if (length(new_sel) > BLOOM_MAX_PARENTS) {
      new_sel <- utils::tail(new_sel, BLOOM_MAX_PARENTS)
    }
    old_sel   <- state$bloom_parents
    child_col <- state$bloom_child_col_active
    if (is.null(child_col) || !nzchar(child_col %||% "")) {
      state$bloom_parents <- character()
      return(invisible(FALSE))
    }
    x <- projected_sf()
    if (!child_col %in% names(x)) {
      state$bloom_parents <- character()
      return(invisible(FALSE))
    }
    path <- as.character(path %||% active_region_path())
    base_keys  <- make_region_path(x, path)
    child_keys <- bloom_child_keys(x, path, child_col)
    new_sel <- new_sel[new_sel %in% base_keys]
    if (identical(sort(new_sel), sort(old_sel))) {
      return(invisible(FALSE))
    }

    added   <- setdiff(new_sel, old_sel)
    removed <- setdiff(old_sel, new_sel)
    df <- bloom_offset_df()

    for (p in removed) {
      # The collapsed parent lands at the mean of its children's current
      # positions, so dragging the whole group then compressing keeps the
      # branch where the user left it.
      kids <- unique(child_keys[base_keys == p])
      kid_rows <- df[df$region %in% kids, , drop = FALSE]
      n_kids <- max(length(kids), 1L)
      pdx <- sum(kid_rows$dx_m) / n_kids
      pdy <- sum(kid_rows$dy_m) / n_kids
      df <- df[!(df$region %in% kids) & df$region != p, , drop = FALSE]
      if (pdx != 0 || pdy != 0) {
        df <- rbind(df, data.frame(region = p, dx_m = pdx, dy_m = pdy,
                                   stringsAsFactors = FALSE))
      }
    }

    for (p in added) {
      # Children inherit the parent's current offset exactly.
      idx <- match(p, df$region)
      pdx <- if (!is.na(idx)) df$dx_m[idx] else 0
      pdy <- if (!is.na(idx)) df$dy_m[idx] else 0
      df <- df[df$region != p, , drop = FALSE]
      kids <- unique(child_keys[base_keys == p])
      if ((pdx != 0 || pdy != 0) && length(kids) > 0L) {
        df <- rbind(df, data.frame(region = kids, dx_m = pdx, dy_m = pdy,
                                   stringsAsFactors = FALSE))
      }
    }

    state$region_csv_cache <- if (nrow(df) > 0L) csv_text(df) else NULL
    state$bloom_parents <- new_sel

    # Layout history belongs to one grouping state.
    state$undo_stack <- list()
    state$redo_stack <- list()
    state$history_armed <- FALSE
    state$history_ignore_until <- NULL

    if (length(added) > 0L) {
      set_status(
        paste0("Bloomed '", paste(clean_region_display(added, include_path = TRUE), collapse = "', '"), "' into '",
               child_col, "'. Use the dotted frame to drag the whole branch; ",
               "click a child to compress it back to the parent."),
        "ok"
      )
    } else if (length(removed) > 0L) {
      set_status(
        paste0("Compressed '", paste(clean_region_display(removed, include_path = TRUE), collapse = "', '"),
               "' back to '", utils::tail(path, 1L), "'."),
        "info"
      )
    }
    if (isTRUE(notify)) {
      state$animation_active <- TRUE
      session$sendCustomMessage("dragmapr-bloom", list(
        expanded              = as.list(new_sel),
        boundary              = isTRUE(input$bloom_boundary %||% TRUE),
        boundaryBehavior      = "drag",
        boundaryLabel         = "Drag to",
        boundaryDragThreshold = 8,
        regionOffsets         = rows_for_message(bloom_offset_df())
      ))
    }
    invisible(TRUE)
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
      set_status(
        paste0(
          "Loaded ", n_feat, " features from ", source_label, ", but no usable attribute columns were found. ",
          "Add a grouping column to the spatial data, then upload it again."
        ),
        "error"
      )
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
      updateNumericInput(session, "static_width", value = metadata$width %||% 10)
      updateNumericInput(session, "static_height", value = metadata$height %||% 8)
      updateNumericInput(session, "static_dpi", value = metadata$dpi %||% 300)
      updateCheckboxInput(session, "show_labels", value = isTRUE(metadata$show_labels %||% TRUE))
      updateCheckboxInput(session, "show_legend", value = isTRUE(metadata$show_legend %||% TRUE))
      updateCheckboxInput(session, "show_connectors", value = isTRUE(metadata$show_connectors %||% FALSE))
      updateCheckboxInput(session, "legend_reflect_bloom", value = isTRUE(metadata$legend_reflects_bloom %||% FALSE))
      updateTextInput(session, "legend_title", value = metadata$legend_title %||% "Region")
      updateSelectInput(session, "legend_position", selected = metadata$legend_position %||% "bottom")
      updateSelectInput(session, "map_background", selected = metadata$map_background %||% "white")
      updateSelectInput(session, "annotation_mode", selected = metadata$annotation_mode %||% "labels")
      updateSelectInput(session, "label_marker_shape", selected = metadata$label_marker_shape %||% "circle")
      updateSliderInput(session, "label_text_size", value = metadata$label_text_size %||% 11)
      updateSliderInput(session, "label_radius", value = metadata$label_radius %||% ((metadata$marker_size %||% (14 / 3)) * 3))
      updateSliderInput(session, "label_width", value = metadata$label_width %||% 64)
      updateSliderInput(session, "label_height", value = metadata$label_height %||% 30)
      updateSliderInput(session, "box_width", value = metadata$box_width %||% 170)
      updateSliderInput(session, "box_height", value = metadata$box_height %||% 76)
      updateCheckboxInput(
        session,
        "show_origin_outlines",
        value = isTRUE(metadata$show_origin_outlines %||% FALSE)
      )
      updateCheckboxInput(
        session,
        "show_movement_connectors",
        value = isTRUE(metadata$show_movement_connectors %||% FALSE)
      )
      updateCheckboxInput(
        session,
        "show_movement_band",
        value = isTRUE(metadata$show_movement_band %||% FALSE)
      )
      updateCheckboxInput(
        session,
        "legend_show_all",
        value = isTRUE(metadata$legend_show_all %||% FALSE)
      )
      update_studio_color_input(session, "connector_color", value = metadata$connector_color %||% "#334155")
      updateSliderInput(session, "connector_linewidth", value = metadata$connector_linewidth %||% 0.45)
      updateSelectInput(session, "connector_type", selected = metadata$connector_type %||% "straight")
      updateSelectInput(session, "connector_linetype", selected = metadata$connector_linetype %||% "solid")
      updateSelectInput(session, "connector_endpoint", selected = metadata$connector_endpoint %||% "none")
      updateCheckboxInput(session, "connector_smart", value = isTRUE(metadata$connector_smart %||% FALSE))
      update_studio_color_input(session, "movement_connector_color", value = metadata$movement_connector_color %||% "#64748b")
      updateSliderInput(session, "movement_connector_opacity", value = metadata$movement_connector_opacity %||% 0.72)
      updateSliderInput(session, "movement_connector_linewidth", value = metadata$movement_connector_linewidth %||% 0.45)
      updateSelectInput(session, "movement_connector_linetype", selected = metadata$movement_connector_linetype %||% "solid")
      updateSelectInput(session, "movement_connector_endpoint", selected = metadata$movement_connector_endpoint %||% "closed")
      state$undo_stack <- list()
      state$redo_stack <- list()
      state$region_csv_cache <- if (!is.null(project$region_offsets)) csv_text(project$region_offsets) else NULL
      state$label_csv_cache <- if (!is.null(project$label_offsets)) csv_text(project$label_offsets) else NULL
      state$pending_region_col <- metadata$region_col %||% NULL
      state$pending_label_col <- metadata$label_col %||% NULL
      state$legend_filter_selection <- if (is.null(metadata$legend_values)) NULL else as.character(metadata$legend_values)
      state$label_filter_selection <- if (is.null(metadata$label_values)) NULL else as.character(metadata$label_values)

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

  active_region_path <- reactive({
    req(region_col())
    path <- state$active_region_path
    x    <- projected_sf()
    if (is.null(path) || length(path) == 0L ||
        !all(path %in% names(x)) ||
        !identical(tail(path, 1L), region_col())) {
      return(region_col())
    }
    path
  })

  # TRUE when a valid bloom child column is set and at least one parent is
  # currently expanded.
  bloom_active <- reactive({
    child_col <- state$bloom_child_col_active
    !is.null(child_col) && nzchar(child_col %||% "") &&
      length(state$bloom_parents) > 0L &&
      child_col %in% names(projected_sf())
  })

  map_sf <- reactive({
    x    <- projected_sf()
    path <- active_region_path()
    if (isTRUE(bloom_active())) {
      # Mixed-level view: expanded parents render as children, the rest of
      # the map keeps the parent grouping.
      base  <- make_region_path(x, path)
      child <- bloom_child_keys(x, path, state$bloom_child_col_active)
      x[[INTERNAL_REGION_COL]] <- ifelse(base %in% state$bloom_parents, child, base)
    } else if (length(path) > 1L) {
      x[[INTERNAL_REGION_COL]] <- make_region_path(x, path)
    }
    x
  })

  effective_region_col <- reactive({
    if (isTRUE(bloom_active()) || length(active_region_path()) > 1L) {
      INTERNAL_REGION_COL
    } else {
      region_col()
    }
  })

  # Groups at the parent level (ignoring any bloom expansion).
  parent_level_groups <- reactive({
    req(region_col())
    stable_unique(make_region_path(projected_sf(), active_region_path()))
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
    req(effective_region_col())
    stable_unique(map_sf()[[effective_region_col()]])
  })

  darken_hex <- function(hex, amount = 0.38) {
    # Note: pmax(0, m) would strip the matrix's dim attribute (pmax copies
    # attributes from its FIRST argument), making m[1, ] fail with
    # "incorrect number of dimensions". Values here are already in [0, 1]
    # and (1 - amount) <= 1, so no clamping is needed at all.
    rgb <- grDevices::col2rgb(hex) / 255
    rgb <- rgb * (1 - max(0, min(1, amount)))
    grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ])
  }

  stable_palette_for <- function(keys, palette = default_palette) {
    keys <- stable_unique(keys)
    stats::setNames(rep(palette, length.out = length(keys)), keys)
  }

  parent_region_palette <- reactive({
    req(region_col())

    parents <- parent_level_groups()
    pal <- stable_palette_for(parents, default_palette)

    override <- state$region_palette_override
    if (!is.null(override)) {
      keep <- intersect(names(override), names(pal))
      pal[keep] <- unname(override[keep])
    }

    pal
  })

  bloom_child_palette <- reactive({
    child_col <- state$bloom_child_col_active

    if (is.null(child_col) || !nzchar(child_col %||% "")) {
      return(character())
    }

    x <- projected_sf()
    if (!child_col %in% names(x)) {
      return(character())
    }

    path <- active_region_path()

    parent_keys <- make_region_path(x, path)
    child_keys  <- bloom_child_keys(x, path, child_col)

    parent_pal <- parent_region_palette()
    child_groups <- stable_unique(child_keys)

    out <- character(length(child_groups))
    names(out) <- child_groups

    for (parent in stable_unique(parent_keys)) {
      kids <- stable_unique(child_keys[parent_keys == parent])
      # `[` (not `[[`) so a missing parent gives NA instead of erroring
      # during data/column transitions.
      parent_color <- unname(parent_pal[parent])

      if (length(parent_color) != 1L || is.na(parent_color)) {
        parent_color <- "#334155"
      }

      amounts <- seq(0.28, 0.52, length.out = length(kids))

      for (i in seq_along(kids)) {
        out[[kids[i]]] <- darken_hex(parent_color, amount = amounts[i])
      }
    }

    out
  })

  region_palette <- reactive({
    c(parent_region_palette(), bloom_child_palette())
  })

  legend_values <- reactive({
    # Parent-only during bloom keeps the legend stable. The optional
    # bloom-aware mode switches to the mixed parent/child key set.
    groups <- if (nzchar(state$bloom_child_col_active %||% "") &&
                  !isTRUE(input$legend_reflect_bloom)) {
      parent_level_groups()
    } else {
      region_groups()
    }
    selected <- state$legend_filter_selection
    if (is.null(selected)) {
      groups
    } else {
      intersect(as.character(selected), groups)
    }
  })

  visible_legend_values <- reactive({
    values <- legend_values()
    if (!isTRUE(input$legend_show_all) && length(values) > LEGEND_THRESHOLD) {
      character()
    } else {
      values
    }
  })

  helper_region_palette <- reactive({
    pal <- region_palette()

    parent_names <- names(parent_region_palette())
    child_names  <- names(bloom_child_palette())

    ordered <- unique(c(
      intersect(parent_names, names(pal)),
      intersect(child_names, names(pal)),
      names(pal)
    ))

    pal[ordered]
  })

  legend_max_keys <- reactive({
    # Keep the dragmapr renderer from suppressing the legend when the user
    # selects fewer legend values than the total number of region groups.
    # The selected values are applied through legend breaks/options instead.
    as.integer(max(length(region_groups()), length(legend_values()), LEGEND_THRESHOLD, 1L))
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
    req(effective_region_col(), label_col())
    labels <- make_region_labels(map_sf(), region_col = effective_region_col(), label_col = label_col())
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

  label_auto_limit_n <- reactive({
    n <- suppressWarnings(as.integer(input$label_auto_limit_n %||% LABEL_AUTO_LIMIT))
    if (!is.finite(n)) n <- LABEL_AUTO_LIMIT
    max(LABEL_AUTO_MIN, min(LABEL_AUTO_MAX, n))
  })

  label_area_scores <- reactive({
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    out <- stats::setNames(rep(0, length(ids)), ids)
    if (length(ids) == 0L) {
      return(out)
    }

    x <- map_sf()
    region_col_now <- effective_region_col()
    if (!region_col_now %in% names(x) || nrow(x) == 0L) {
      return(out)
    }

    groups <- as.character(x[[region_col_now]])
    areas <- suppressWarnings(as.numeric(sf::st_area(sf::st_geometry(x))))
    areas[!is.finite(areas)] <- 0

    summed <- tapply(areas, groups, sum, na.rm = TRUE)
    keep <- intersect(names(summed), ids)
    out[keep] <- as.numeric(summed[keep])
    out
  })

  bloom_child_label_ids <- reactive({
    child_col <- state$bloom_child_col_active
    if (is.null(child_col) || !nzchar(child_col %||% "") ||
        length(state$bloom_parents) == 0L ||
        !child_col %in% names(projected_sf())) {
      return(character())
    }

    x <- projected_sf()
    path <- active_region_path()
    base_keys  <- make_region_path(x, path)
    child_keys <- bloom_child_keys(x, path, child_col)
    unique(as.character(child_keys[base_keys %in% state$bloom_parents]))
  })

  auto_visible_label_ids <- reactive({
    labels <- all_label_table()
    ids <- as.character(labels$label_id)
    if (length(ids) == 0L) {
      return(ids)
    }

    limit <- label_auto_limit_n()
    if (!isTRUE(input$label_auto_limit) || length(ids) <= limit) {
      return(ids)
    }

    areas <- label_area_scores()
    moved <- as.character(bloom_offset_df()$region)
    bloom_kids <- bloom_child_label_ids()

    score <- areas[ids]
    score[!is.finite(score)] <- 0
    score <- score + ifelse(ids %in% moved, max(score, 1) * 10, 0)
    score <- score + ifelse(ids %in% bloom_kids, max(score, 1) * 100, 0)

    # `order()` has one `decreasing` flag, so sort the numeric score by
    # negating it and then use stable text tie-breakers.
    ordered <- ids[order(-score, as.character(labels$label), as.character(labels$region), method = "radix")]
    utils::head(unique(as.character(ordered)), limit)
  })

  visible_label_ids <- reactive({
    ids <- as.character(all_label_table()$label_id)
    selected <- state$label_filter_selection
    if (is.null(selected)) {
      auto_visible_label_ids()
    } else {
      intersect(as.character(selected), ids)
    }
  })

  label_display_summary <- reactive({
    total <- length(as.character(all_label_table()$label_id))
    visible <- length(visible_label_ids())
    selected <- state$label_filter_selection

    if (total == 0L) {
      return("No labels available for the current grouping.")
    }
    if (!isTRUE(input$show_labels)) {
      return(paste0(total, " label", if (total == 1L) "" else "s", " available. Labels are hidden."))
    }
    if (!is.null(selected)) {
      return(paste0("Showing ", visible, " of ", total, " selected label", if (visible == 1L) "" else "s", "."))
    }
    if (isTRUE(input$label_auto_limit) && visible < total) {
      return(paste0("Auto showing ", visible, " of ", total, " labels. Use the picker to choose exact labels or Select all when needed."))
    }
    paste0("Showing all ", total, " label", if (total == 1L) "" else "s", ".")
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
    req(effective_region_col(), label_col())
    x <- map_sf()
    if (nrow(x) == 0L) {
      return(as_drag_labels(data.frame(
        label_id = character(),
        region = character(),
        label = character(),
        x = numeric(),
        y = numeric()
      )))
    }
    labels <- make_studio_labels(
      x,
      region_col     = effective_region_col(),
      label_col      = label_col(),
      mode           = input$annotation_mode %||% "labels",
      width_px       = input$box_width       %||% 170,
      height_px      = input$box_height      %||% 76,
      connector      = isTRUE(input$show_connectors),
      connector_type = input$connector_type  %||% "straight"
    )
    labels <- apply_label_edits(labels, state$label_edits)
    labels$label_color <- unname(label_palette()[as.character(labels$label_id)])
    labels$label_color[is.na(labels$label_color)] <- "#111827"
    labels
  })

  region_csv_raw <- debounce(reactive(input$region_csv), 250)
  label_csv_raw  <- debounce(reactive(input$label_csv),  250)

  # observeEvent(region_csv_raw(), {
  #   incoming <- region_csv_raw()
  #   if (!is.null(incoming) && nzchar(incoming)) {
  #     incoming_df <- tryCatch(read_csv_text(incoming), error = function(e) NULL)
  #     if (!is.null(incoming_df) && "region" %in% names(incoming_df)) {
  #       current_groups <- tryCatch(region_groups(), error = function(e) NULL)
  #       # Reject broadcasts from the old column's iframe: only accept if the
  #       # incoming region names exactly match the current column's groups.
  #       if (is.null(current_groups) ||
  #           setequal(as.character(incoming_df$region), as.character(current_groups))) {
  #         state$region_csv_cache <- incoming
  #       }
  #     }
  #   }
  # }, ignoreInit = TRUE)

  observeEvent(region_csv_raw(), {
    incoming <- region_csv_raw()

    if (is.null(incoming) || !nzchar(incoming)) {
      return()
    }

    incoming_df <- tryCatch(read_csv_text(incoming), error = function(e) NULL)

    if (is.null(incoming_df) || !"region" %in% names(incoming_df)) {
      return()
    }

    incoming_df <- normalize_offset_rows_from_helper(incoming_df)

    if (is.null(incoming_df) || nrow(incoming_df) == 0L) {
      return()
    }

    current_groups <- tryCatch(region_groups(), error = function(e) NULL)

    # Accept partial updates only when every incoming key belongs to the
    # current visible/bloomed key set. This prevents old iframe broadcasts
    # from another grouping from corrupting the state.
    if (!is.null(current_groups)) {
      ok <- all(as.character(incoming_df$region) %in% as.character(current_groups))
      if (!ok) {
        return()
      }
    }

    merge_region_offset_rows(incoming_df)
  }, ignoreInit = TRUE)

  observeEvent(label_csv_raw(), {
    incoming <- label_csv_raw()
    if (!is.null(incoming) && nzchar(incoming)) {
      incoming_df <- tryCatch(read_csv_text(incoming), error = function(e) NULL)
      if (!is.null(incoming_df) && "label_id" %in% names(incoming_df)) {
        current_labels <- tryCatch(label_table(), error = function(e) NULL)
        if (is.null(current_labels) ||
            setequal(as.character(incoming_df$label_id),
                     as.character(current_labels$label_id))) {
          state$label_csv_cache <- incoming
        }
      } else {
        state$label_csv_cache <- incoming
      }
    }
  }, ignoreInit = TRUE)

  region_delta_state <- reactive({
    read_csv_text(state$region_csv_cache) %||%
      read_csv_text(region_csv_raw()) %||%
      empty_region_offsets(region_groups())
  })

  region_base_state <- reactive({
    read_csv_text(state$region_base_csv_cache) %||%
      empty_region_offsets(region_groups())
  })

  region_state <- reactive({
    combine_region_offsets(
      base   = region_base_state(),
      delta  = region_delta_state(),
      groups = region_groups()
    )
  })

  label_state <- reactive({
    read_csv_text(state$label_csv_cache) %||% read_csv_text(label_csv_raw()) %||% empty_label_offsets(label_table())
  })

  observeEvent(list(region_state(), label_state()), {
    push_history(state_snapshot())
  }, ignoreInit = TRUE)

  build_plot <- function(region_offsets, label_offsets) {
    # Keep the renderer's internal legend limit high enough for the full data,
    # then use scale breaks below to control which keys are actually displayed.
    legend_breaks <- visible_legend_values()
    all_legend_values <- region_groups()
    legend_active <- isTRUE(input$show_legend) &&
      !identical(input$legend_position %||% "bottom", "none") &&
      length(legend_breaks) > 0L
    max_keys <- legend_max_keys()
    plot_labels <- if (isTRUE(input$show_labels)) {
      labels <- label_table()
      labels <- labels[as.character(labels$label_id) %in% visible_label_ids(), , drop = FALSE]
      if (isTRUE(input$connector_smart)) {
        labels <- apply_smart_connector_types(labels, label_offsets)
      }
      labels
    } else {
      FALSE
    }
    plot <- render_dragged_map(
      map_sf(),
      region_offsets      = region_offsets,
      region_col          = effective_region_col(),
      labels              = plot_labels,
      label_values        = visible_label_ids(),
      label_offsets       = label_offsets,
      region_palette      = region_palette(),
      legend_values       = visible_legend_values(),
      show_legend         = legend_active,
      max_legend_keys     = max_keys,
      legend_position     = input$legend_position %||% "bottom",
      legend_title        = input$legend_title %||% "Region",
      show_label_marker   = !identical(input$label_marker_shape %||% "circle", "none"),
      label_marker_shape  = input$label_marker_shape %||% "circle",
      marker_size         = (input$label_radius %||% 14) / 3,
      connector_color     = studio_color_value(input$connector_color, "#334155"),
      connector_linewidth = input$connector_linewidth %||% 0.45,
      connector_linetype  = input$connector_linetype %||% "solid",
      connector_endpoint  = input$connector_endpoint %||% "none",
      show_origin_outlines = isTRUE(input$show_origin_outlines),
      show_movement_connectors = isTRUE(input$show_movement_connectors),
      show_movement_band   = isTRUE(input$show_movement_band),
      movement_connector_color = studio_color_value(input$movement_connector_color, "#64748b"),
      movement_connector_opacity = input$movement_connector_opacity %||% 0.72,
      movement_connector_linewidth = input$movement_connector_linewidth %||% 0.45,
      movement_connector_linetype = input$movement_connector_linetype %||% "solid",
      movement_connector_endpoint = input$movement_connector_endpoint %||% "closed",
      label_padding       = 0.12,
      map_background      = input$map_background %||% "white",
      title               = state$static_title
    )
    if (legend_active) {
      key_count <- length(legend_breaks)
      horizontal <- input$legend_position %in% c("top", "bottom")
      legend_cols <- if (horizontal) min(4L, key_count) else 1L

      # Only replace the fill scale when the user has chosen a subset of keys.
      # suppressMessages() prevents ggplot2's harmless "Scale for fill is already
      # present" message from appearing each time the multiselect changes.
      if (!identical(as.character(legend_breaks), as.character(all_legend_values))) {
        plot <- suppressMessages(
          plot + ggplot2::scale_fill_manual(
            values = region_palette(),
            breaks = legend_breaks,
            drop = FALSE
          )
        )
      }

      plot <- plot +
        ggplot2::guides(
          fill = ggplot2::guide_legend(ncol = legend_cols, byrow = TRUE)
        ) +
        ggplot2::theme(
          legend.box = if (horizontal) "horizontal" else "vertical",
          legend.key.size = ggplot2::unit(0.42, "lines"),
          legend.text = ggplot2::element_text(size = 8)
        )
    } else {
      plot <- plot + ggplot2::theme(legend.position = "none")
    }
    plot
  }

  current_plot <- reactive({
    build_plot(region_state(), label_state())
  })

  # ---- Preview refresh logic ----

  preview_token <- reactiveVal(0L)
  preview_snapshot <- reactiveVal(NULL)
  do_refresh <- function(force = FALSE) {
    if (!isTRUE(force) && isTRUE(state$animation_active)) {
      state$preview_refresh_pending <- TRUE
      return(invisible(FALSE))
    }
    preview_snapshot(list(
      region_offsets = isolate(region_state()),
      label_offsets  = isolate(label_state())
    ))
    preview_token(isolate(preview_token()) + 1L)
    state$preview_refresh_pending <- FALSE
    invisible(TRUE)
  }

  observeEvent(input$refresh_preview, do_refresh(force = TRUE), ignoreInit = TRUE)

  observeEvent(input$helper_animation_state, {
    msg <- input$helper_animation_state
    active <- isTRUE(msg$active)
    state$animation_active <- active
    if (!active && isTRUE(state$preview_refresh_pending)) {
      do_refresh(force = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(
    list(projected_sf(), region_col(), label_col(), region_palette(), label_table(),
         input$show_labels, input$show_legend, input$label_marker_shape,
         input$connector_color, input$connector_linewidth, input$legend_show_all,
         input$legend_reflect_bloom, input$legend_filter,
         input$legend_position, input$legend_title, input$connector_linetype, input$connector_endpoint,
         input$connector_smart, input$show_origin_outlines, input$show_movement_connectors, input$show_movement_band,
         input$movement_connector_color, input$movement_connector_opacity,
         input$movement_connector_linewidth, input$movement_connector_linetype,
         input$movement_connector_endpoint,
         input$map_background,
         state$bloom_parents, state$bloom_child_col_active,
         state$static_title),
    do_refresh(),
    ignoreInit = FALSE
  )

  auto_refresh_signal <- debounce(
    reactive(list(r = region_csv_raw(), l = label_csv_raw())),
    500
  )
  observeEvent(auto_refresh_signal(), {
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
    apply_offsets(map_sf(), region_state(), region_col = effective_region_col())
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
    label_offsets <- label_state()
    can_reset_labels <- nrow(label_offsets) > 0L && any(
      is.finite(label_offsets$dx_m) & is.finite(label_offsets$dy_m) &
        (label_offsets$dx_m != 0 | label_offsets$dy_m != 0)
    )
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
        if (can_undo) "Undo the last drag-state change (Ctrl+Z)" else "No drag-state changes to undo (Ctrl+Z)"
      ),
      history_button(
        "redo_layout", "Redo", "btn-sm btn-default",
        can_redo,
        if (can_redo) "Redo the last undone drag-state change (Ctrl+Y)" else "No undone drag-state changes to redo (Ctrl+Y)"
      ),
      history_button(
        "reset_label_positions", "Reset label positions", "btn-sm btn-default",
        can_reset_labels,
        if (can_reset_labels) {
          "Return all dragged labels to their regions without moving regions"
        } else {
          "No dragged label positions to reset"
        }
      ),
      actionButton("reset_layout", "Reset drag layout", class = "btn-sm btn-warning"),
      actionButton("reset_view", "Reset view", class = "btn-sm btn-default", title = "Re-centre the map (Esc)")
    )
  })

  # First nesting child column for a given parent column, or NULL. Used for
  # the "Detected" summary and the recommended-grouping action.
  recommend_bloom_child <- function(x, parent_col) {
    rec <- tryCatch(
      recommend_dragmapr_hierarchy(x, parent_col = parent_col),
      error = function(e) NULL
    )
    rec$child %||% NULL
  }

  # Combined detected/setup block inside Start. Keeping this as the only
  # setup summary avoids asking users to read two competing text cards.
  output$start_summary <- renderUI({
    x <- projected_sf()
    cols <- available_columns()
    if (is.null(x) || !inherits(x, "sf") || length(cols) == 0L) {
      return(tags$p("Upload a spatial file to see what dragmapr detects.",
                    class = "studio-help"))
    }

    geom_types <- unique(as.character(sf::st_geometry_type(x)))
    rec_parent <- default_studio_region_col(x, cols)
    rec_child <- tryCatch(recommend_bloom_child(x, rec_parent),
                          error = function(e) NULL)

    rcol <- region_col() %||% rec_parent %||% "-"
    child <- state$bloom_child_col_active
    expanded <- state$bloom_parents
    n_parents <- length(parent_level_groups())

    labels_txt <- if (isTRUE(input$show_labels %||% TRUE)) {
      if (isTRUE(input$label_auto_limit %||% TRUE)) {
        paste0("Auto, up to ", input$label_auto_limit_n %||% LABEL_AUTO_LIMIT)
      } else {
        "On"
      }
    } else {
      "Off"
    }

    tip <- if (is.null(child) || !nzchar(child %||% "")) {
      "Pick 'Expand into' below, then click a group on the map to bloom it."
    } else if (length(expanded) == 0L) {
      paste0("Click a ", rcol, " on the map to bloom it into ", child, ".")
    } else {
      "Drag the dotted frame to move a branch; click it to collapse."
    }

    crs <- summarise_spatial_crs(x)
    crs_bounds <- if (!is.null(crs$bounds)) {
      paste0(
        format(round(crs$bounds[["xmin"]], 2), trim = TRUE), ", ",
        format(round(crs$bounds[["ymin"]], 2), trim = TRUE), " to ",
        format(round(crs$bounds[["xmax"]], 2), trim = TRUE), ", ",
        format(round(crs$bounds[["ymax"]], 2), trim = TRUE)
      )
    } else {
      "Unavailable"
    }

    tagList(
      tags$div(
        class = "studio-setup-card studio-detected-card",
        tags$div(class = "setup-row",
                 tags$span("Features"), tags$span(format(nrow(x)))),
        tags$div(class = "setup-row",
                 tags$span("Geometry"),
                 tags$span(paste(utils::head(geom_types, 2L), collapse = ", "))),
        tags$div(class = "setup-row",
                 tags$span("Recommended"),
                 tags$span(if (!is.null(rec_child)) {
                   paste0(rec_parent, " -> ", rec_child)
                 } else {
                   rec_parent %||% "-"
                 })),
        tags$div(class = "setup-row",
                 tags$span("Group map by"), tags$span(rcol)),
        tags$div(class = "setup-row",
                 tags$span("Expand into"),
                 tags$span(if (!is.null(child) && nzchar(child %||% "")) child else "Off")),
        tags$div(class = "setup-row",
                 tags$span("Groups"), tags$span(format(n_parents))),
        tags$div(class = "setup-row",
                 tags$span("Expanded"),
                 tags$span(if (length(expanded) > 0L) {
                   paste(clean_region_display(expanded, include_path = TRUE), collapse = ", ")
                 } else {
                   "None"
                 })),
        tags$div(class = "setup-row",
                 tags$span("Labels"), tags$span(labels_txt)),
        tags$div(tip, class = "setup-tip")
      ),
      tags$div(
        class = "studio-crs-card",
        tags$div(
          class = "crs-head",
          tags$span(crs$id),
          tags$span(crs$type)
        ),
        tags$div(class = "crs-row",
                 tags$span("Name"), tags$span(crs$name)),
        tags$div(class = "crs-row",
                 tags$span("Units"), tags$span(crs$units)),
        tags$div(class = "crs-row",
                 tags$span("Bounds"), tags$span(crs_bounds)),
        tags$div(crs$message, class = "crs-meaning")
      ),
      tags$div(class = "studio-action-row",
               actionButton("use_recommended", "Use recommended grouping",
                            class = "btn-sm btn-default"))
    )
  })

  observeEvent(input$use_recommended, {
    x <- projected_sf()
    cols <- available_columns()
    if (length(cols) == 0L) return()
    rec <- default_studio_region_col(x, cols)
    if (is.null(rec) || identical(rec, input$region_col)) {
      set_status(paste0("Already grouping by '", rec %||% "-", "'."), "info")
      return()
    }
    state$pending_region_col <- rec
    state$pending_label_col  <- rec
    state$column_ui_nonce <- state$column_ui_nonce + 1L
    set_status(paste0("Grouping by '", rec, "'. Pick 'Expand into' in the ",
                      "Grouping & bloom section to enable bloom."), "info")
  })

  # Once the recommended grouping has landed in the input, stop forcing it.
  observeEvent(input$region_col, {
    if (!is.null(state$pending_region_col) &&
        identical(input$region_col, state$pending_region_col)) {
      state$pending_region_col <- NULL
      state$pending_label_col  <- NULL
    }
  }, ignoreInit = TRUE)

  output$column_controls <- renderUI({
    state$column_ui_nonce  # re-render when "Use recommended grouping" runs
    cols <- available_columns()
    if (length(cols) == 0L) return(tags$p("No non-geometry columns found.", style = "color:#6b7280; font-size:0.83rem;"))
    default_col <- state$pending_region_col %||% default_studio_region_col(projected_sf(), cols)
    default_label <- state$pending_label_col %||% default_col
    selected_region <- state$pending_region_col %||%
      choose_column(isolate(input$region_col), cols, default_col)
    selected_label <- state$pending_label_col %||%
      choose_column(isolate(input$label_col), cols, default_label)
    tagList(
      studio_select(
        "region_col", "Group map by",
        choices = cols,
        selected = choose_column(selected_region, cols, default_col),
        placeholder = "Choose grouping column"
      ),
      tags$div(class = "studio-field-gap"),
      studio_select(
        "label_col",  "Label names from",
        choices = cols,
        selected = choose_column(selected_label, cols, default_label),
        placeholder = "Choose label column"
      )
    )
  })

  output$bloom_controls <- renderUI({
    cols <- available_columns()
    if (length(cols) == 0L) return(NULL)
    path <- active_region_path()
    child_choices <- setdiff(cols, as.character(path))
    parents <- parent_level_groups()
    active_child <- state$bloom_child_col_active %||% ""
    tagList(
      tags$p(
        paste0(
          "Pick what a parent should bloom into, then click a parent on the ",
          "map (or choose below). At most ", BLOOM_MAX_PARENTS, " parents ",
          "can be expanded at a time; expanding another replaces the oldest."
        ),
        class = "studio-help"
      ),
      studio_select(
        "bloom_child_col", "Expand into (child column)",
        choices  = c(stats::setNames("", "Off"),
                     stats::setNames(child_choices, child_choices)),
        selected = if (nzchar(active_child) && active_child %in% child_choices) {
          active_child
        } else {
          ""
        },
        placeholder = "Choose child column"
      ),
      tags$div(class = "studio-field-gap"),
      studio_multi_select(
        "bloom_parents",
        paste0("Expanded groups (max ", BLOOM_MAX_PARENTS, ")"),
        choices  = clean_region_choices(parents),
        selected = intersect(state$bloom_parents, parents),
        placeholder = "Click the map or choose groups"
      ),
      checkboxInput(
        "bloom_boundary", "Show dotted drag handle",
        value = isTRUE(isolate(input$bloom_boundary) %||% TRUE)
      )
    )
  })

  # Arm or disarm bloom. Changing the target collapses everything first,
  # and the chosen column must actually nest inside the current grouping.
  observeEvent(input$bloom_child_col, {
    sel <- as.character(input$bloom_child_col %||% "")
    cur <- state$bloom_child_col_active %||% ""
    if (identical(sel, cur)) return()
    if (length(state$bloom_parents) > 0L) {
      set_bloom_parents(character())
    }
    if (!nzchar(sel)) {
      state$bloom_child_col_active <- NULL
      if (nzchar(cur)) {
        # Disarm: rebuild once so the helper drops the bloom machinery.
        state$bloom_version <- state$bloom_version + 1L
      }
      return()
    }
    x <- projected_sf()
    if (!sel %in% names(x)) return()
    path <- active_region_path()
    check_x <- x
    check_x[["..dragmapr_parent_check.."]] <- make_region_path(check_x, path)
    bloom_check <- validate_bloom_hierarchy(
      check_x,
      parent_col = "..dragmapr_parent_check..",
      child_col = sel,
      max_child_groups = 600L
    )
    if (!isTRUE(bloom_check$valid)) {
      state$bloom_child_col_active <- NULL
      if (nzchar(cur)) {
        state$bloom_version <- state$bloom_version + 1L
      }
      set_status(
        paste0("'", sel, "' cannot be used for bloom: ", bloom_check$message),
        "error"
      )
      return()
    }
    state$bloom_child_col_active <- sel
    # Arm: one rebuild embeds parent + child keys in the helper; after this,
    # expanding and collapsing is instant (postMessage, no rebuild).
    state$bloom_version <- state$bloom_version + 1L
    set_status(
      paste0("Bloom armed: click a parent region on the map (or pick ",
             "parents in the Bloom panel) to expand it into '", sel, "'."),
      "info"
    )
  }, ignoreInit = TRUE)

  # Boundary visibility is pushed to the live helper, never a rebuild.
  observeEvent(input$bloom_boundary, {
    if (length(state$bloom_parents) == 0L) return()
    session$sendCustomMessage("dragmapr-bloom", list(
      expanded              = as.list(state$bloom_parents),
      boundary              = isTRUE(input$bloom_boundary),
      boundaryBehavior      = "drag",
      boundaryLabel         = "Drag to",
      boundaryDragThreshold = 8,
      regionOffsets         = rows_for_message(bloom_offset_df())
    ))
  }, ignoreInit = TRUE)

  # Debounced so the transient NULL emitted while the picker re-renders does
  # not collapse the current expansion.
  bloom_parents_input <- debounce(reactive(input$bloom_parents), 300)
  observeEvent(bloom_parents_input(), {
    set_bloom_parents(bloom_parents_input())
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Click on the map (relayed by the helper): expand an unexpanded parent,
  # or compress the branch when one of its children is clicked.
  observeEvent(input$helper_region_click, {
    key <- as.character(input$helper_region_click$region %||% "")
    if (!nzchar(key)) return()
    child_col <- state$bloom_child_col_active
    if (is.null(child_col) || !nzchar(child_col %||% "")) return()
    apply_helper_offset_rows(input$helper_region_click$regionOffsets)
    x <- projected_sf()
    if (!child_col %in% names(x)) return()
    path <- active_region_path()
    base_keys  <- make_region_path(x, path)
    child_keys <- bloom_child_keys(x, path, child_col)
    current <- state$bloom_parents
    if (key %in% current) {
      set_bloom_parents(setdiff(current, key))
    } else if (key %in% base_keys) {
      set_bloom_parents(c(current, key))  # FIFO clamp handles the max
    } else {
      hit <- unique(base_keys[child_keys == key & base_keys %in% current])
      if (length(hit) > 0L) {
        set_bloom_parents(setdiff(current, hit))
      }
    }
    do_refresh()
  })

  # Dotted group frame interaction. The helper separates frame drag from
  # frame click before it talks back to Shiny. Drag messages save offsets
  # and keep the branch open; click messages remove the parent from the
  # expanded set after the reversible branch animation starts.
  observeEvent(input$boundary_drag_offsets, {
    parent_key <- as.character(input$boundary_drag_offsets$parent %||% "")
    state$boundary_drag_parent <- parent_key
    state$boundary_drag_ignore_until <- Sys.time() + 0.35
    apply_helper_offset_rows(input$boundary_drag_offsets$regionOffsets)
    do_refresh()
  }, ignoreInit = TRUE)

  observeEvent(input$collapse_branch, {
    parent_key <- as.character(input$collapse_branch$parent %||% "")
    if (!nzchar(parent_key)) return()

    # Keep the latest helper-reported position before compressing, so a branch
    # that was dragged and then clicked returns exactly where the user left
    # it. The helper has already filtered real drag gestures into
    # boundary_drag_offsets, so changed rows here should not block collapse.
    apply_helper_offset_rows(input$collapse_branch$regionOffsets)

    if (parent_key %in% state$bloom_parents) {
      set_bloom_parents(setdiff(state$bloom_parents, parent_key))
    }
    do_refresh()
  }, ignoreInit = TRUE)

  # New data invalidates any expansion state.
  observeEvent(state$source_version, {
    state$bloom_parents <- character()
    state$bloom_child_col_active <- NULL
  }, ignoreInit = TRUE)

  output$color_pickers <- renderUI({
    req(region_col())
    groups <- region_groups()
    pal <- isolate(region_palette())
    selected <- isolate(input$region_color_group)
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
          choices = clean_region_choices(groups),
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
        choices = clean_region_choices(groups),
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

  output$legend_filter_ui <- renderUI({
    groups <- region_groups()
    if (length(groups) == 0L) {
      return(NULL)
    }
    choices <- clean_region_choices(groups)
    selected <- isolate(state$legend_filter_selection)
    if (is.null(selected)) {
      selected <- unname(choices)
    } else {
      selected <- intersect(as.character(selected), unname(choices))
    }

    tagList(
      tags$p(
        "Choose which categories appear in the legend. This does not filter the map itself.",
        class = "studio-help"
      ),
      studio_multi_select(
        "legend_filter",
        "Legend values",
        choices = choices,
        selected = selected,
        placeholder = "Choose legend keys to show",
        all_label = "All legend values"
      )
    )
  })

  output$label_filter_ui <- renderUI({
    labels <- all_label_table()
    choices <- stats::setNames(
      as.character(labels$label_id),
      paste0(as.character(labels$label), " (",
             clean_region_display(as.character(labels$region), include_path = TRUE), ")")
    )
    selected <- isolate(state$label_filter_selection)
    if (is.null(selected)) {
      selected <- intersect(isolate(auto_visible_label_ids()), unname(choices))
    } else {
      selected <- intersect(as.character(selected), unname(choices))
    }

    tagList(
      studio_multi_select(
        "label_filter",
        "Visible labels",
        choices = choices,
        selected = selected,
        placeholder = "Choose labels to show",
        all_label = "All labels"
      ),
      tags$p(label_display_summary(), class = "studio-help"),
      tags$div(
        class = "studio-action-row",
        actionButton("label_use_auto", "Use auto", class = "btn btn-default btn-xs"),
        actionButton("label_show_all", "Show all", class = "btn btn-default btn-xs")
      ),
      tags$div(class = "studio-field-gap")
    )
  })

  observeEvent(input$label_use_auto, {
    state$label_filter_selection <- NULL
    session$sendCustomMessage(
      "dragmapr-label-values",
      list(values = as.list(visible_label_ids()))
    )
    set_status("Label visibility returned to Auto.", "info")
  }, ignoreInit = TRUE)

  observeEvent(input$label_show_all, {
    ids <- as.character(all_label_table()$label_id)
    state$label_filter_selection <- ids
    session$sendCustomMessage(
      "dragmapr-label-values",
      list(values = as.list(ids))
    )
    set_status(paste0("Showing all ", length(ids), " labels."), "info")
  }, ignoreInit = TRUE)

  output$label_editor_ui <- renderUI({
    labels <- base_label_table()
    if (nrow(labels) == 0L) {
      return(NULL)
    }
    choices <- stats::setNames(
      as.character(labels$label_id),
      paste0(as.character(labels$label), " (", as.character(labels$region), ")")
    )
    selected <- isolate(input$edit_label_id)
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
    default_col <- state$pending_region_col %||% default_studio_region_col(projected_sf(), cols)
    default_label <- state$pending_label_col %||% default_col
    selected_region <- choose_column(state$pending_region_col %||% isolate(input$region_col), cols, default_col)
    selected_label <- choose_column(state$pending_label_col %||% isolate(input$label_col), cols, default_label)
    signature <- list(cols = cols, region = selected_region, label = selected_label)
    if (!identical(signature, state$column_select_signature)) {
      updateSelectInput(session, "region_col", choices = cols, selected = selected_region)
      updateSelectInput(session, "label_col", choices = cols, selected = selected_label)
      state$column_select_signature <- signature
    }
    state$pending_region_col <- NULL
    state$pending_label_col <- NULL
  }, ignoreInit = FALSE)

  observeEvent(region_groups(), {
    selected <- state$legend_filter_selection
    if (is.null(selected)) return()
    keep <- intersect(as.character(selected), region_groups())
    if (length(keep) == 0L && length(selected) > 0L) {
      state$legend_filter_selection <- NULL
      set_status("Project legend selection did not match the current groups, so all legend values are visible.", "info")
    } else if (!identical(keep, selected)) {
      state$legend_filter_selection <- keep
    }
  }, ignoreInit = TRUE)

  observeEvent(base_label_table(), {
    selected <- state$label_filter_selection
    if (is.null(selected)) return()
    ids <- as.character(base_label_table()$label_id)
    keep <- intersect(as.character(selected), ids)
    if (length(keep) == 0L && length(selected) > 0L) {
      state$label_filter_selection <- NULL
      set_status("Project label selection did not match the current labels, so label visibility returned to Auto.", "info")
    } else if (!identical(keep, selected)) {
      state$label_filter_selection <- keep
    }
  }, ignoreInit = TRUE)

  observeEvent(input$studio_filter_change, {
    change <- input$studio_filter_change
    values <- as.character(change$values %||% character())
    if (identical(change$id, "legend_filter")) {
      state$legend_filter_selection <- values
    } else if (identical(change$id, "label_filter")) {
      state$label_filter_selection <- values
    }
  }, ignoreInit = TRUE)

  # Rebuild helper HTML only for data, grouping, or label-column changes.
  # Presentation-only changes are sent to the existing iframe so they do not
  # reset drag state or show a map overlay.
  observeEvent(
    list(state$source_version, projected_sf(), region_col(), label_col(),
         state$bloom_version),
    {
      bloom_armed_now <- nzchar(state$bloom_child_col_active %||% "")
      signature <- list(
        source_version = state$source_version,
        region_col = region_col(),
        label_col = label_col(),
        bloom_version = state$bloom_version
      )
      if (identical(signature, state$helper_signature)) {
        return()
      }
      # When only the column changed (same source data), save and restore
      # per-column offset caches so switching columns never loses layout work.
      #   Region offsets -> keyed by region_col (geometries move together)
      #   Label  offsets -> keyed by region_col:label_col (label IDs change)
      old_sig <- state$helper_signature
      if (!is.null(old_sig) &&
          identical(old_sig$source_version, signature$source_version) &&
          (!identical(old_sig$region_col, signature$region_col) ||
           !identical(old_sig$label_col,  signature$label_col))) {

        region_changed <- !identical(old_sig$region_col, signature$region_col)
        label_changed  <- !identical(old_sig$label_col,  signature$label_col)
        old_rkey <- old_sig$region_col
        new_rkey <- signature$region_col
        old_lkey <- paste0(old_sig$region_col,  ":", old_sig$label_col)
        new_lkey <- paste0(signature$region_col, ":", signature$label_col)

        if (region_changed) {
          old_path <- state$active_region_path %||% old_rkey
          if (is.null(old_path) || length(old_path) == 0L ||
              !identical(tail(as.character(old_path), 1L), old_rkey)) {
            old_path <- old_rkey
          }
          old_context_key <- region_context_key(old_path)

          # Compress any bloomed parents first so the saved layout (and any
          # inheritance below) is keyed by the plain parent grouping.
          if (length(state$bloom_parents) > 0L) {
            set_bloom_parents(character(), notify = FALSE, path = old_path)
          }
          state$bloom_child_col_active <- NULL

          # Save the current visible layout for the current grouping path.
          state$column_offset_store[[old_context_key]] <- list(
            region = state$region_csv_cache,
            label  = state$label_csv_cache,
            path   = old_path
          )

          # Data-driven hierarchy detection (column-name agnostic).
          # The new column is treated as a child level only when it actually
          # subdivides the current grouping AND each of its values stays
          # within (approximately) one parent group. Duplicated child names
          # across parents (e.g. the same municipality name in two counties)
          # are allowed; unrelated cross-cutting attributes are not.
          hierarchy_info <- tryCatch({
            x_now <- projected_sf()
            old_groups_v <- make_region_path(x_now, old_path)
            new_alone_v  <- clean_group_value(x_now[[new_rkey]])
            comp_v <- paste(old_groups_v, new_alone_v, sep = "\r")
            list(
              old_n  = length(unique(old_groups_v)),
              new_n  = length(unique(new_alone_v)),
              comp_n = length(unique(comp_v))
            )
          }, error = function(e) list(old_n = 1L, new_n = 1L, comp_n = 1L))
          old_n  <- hierarchy_info$old_n
          new_n  <- hierarchy_info$new_n
          comp_n <- hierarchy_info$comp_n
          going_finer <- new_n > old_n && comp_n <= ceiling(new_n * 1.5)

          if (going_finer) {
            # Parent to child: inherit from the current visible parent layout.
            # Never restore an older child snapshot here.
            new_path <- unique(c(old_path, new_rkey))

            old_off_df <- tryCatch(
              read_csv_text(state$region_csv_cache),
              error = function(e) NULL
            )

            inherited <- tryCatch(
              inherit_offsets_across_paths(
                projected_sf(),
                old_path    = old_path,
                new_path    = new_path,
                old_offsets = old_off_df
              ),
              error = function(e) NULL
            )

            state$active_region_path    <- new_path
            state$region_base_csv_cache <- NULL

            if (!is.null(inherited) && any(inherited$dx_m != 0 | inherited$dy_m != 0)) {
              state$region_csv_cache <- csv_text(inherited)
              set_status(
                paste0("Switched to '", new_rkey,
                       "'. Child groups inherited the current '", old_rkey, "' layout."),
                "info"
              )
            } else {
              state$region_csv_cache <- NULL
              set_status(
                paste0("Switched to '", new_rkey, "'. Drag to build this grouping's layout."),
                "info"
              )
            }
            state$label_csv_cache <- NULL

          } else {
            # Child to parent: save child layout, restore parent snapshot exactly.
            if (new_rkey %in% old_path) {
              new_path <- old_path[seq_len(match(new_rkey, old_path))]
            } else {
              new_path <- new_rkey
            }
            state$active_region_path    <- new_path
            state$region_base_csv_cache <- NULL

            new_context_key <- region_context_key(new_path)
            stored <- state$column_offset_store[[new_context_key]]

            if (!is.null(stored)) {
              state$region_csv_cache <- stored$region
              state$label_csv_cache  <- stored$label
              set_status(paste0("Switched to '", new_rkey, "'. Saved layout restored."), "info")
            } else {
              state$region_csv_cache <- NULL
              state$label_csv_cache  <- NULL
              set_status(paste0("Switched to '", new_rkey, "'. Drag to position this grouping."), "info")
            }
          }
          state$undo_stack <- list()
          state$redo_stack <- list()
          state$history_armed <- FALSE
          state$history_ignore_until <- NULL

        } else if (label_changed) {
          # Region column unchanged - preserve region offsets entirely.
          # Only save/restore the label offsets per label-column key.
          state$column_offset_store[[old_lkey]] <- list(label = state$label_csv_cache)
          stored_l <- state$column_offset_store[[new_lkey]]
          state$label_csv_cache <- if (!is.null(stored_l)) stored_l$label else NULL
          # region_csv_cache stays untouched - regions have not moved
          set_status(paste0("Label column changed to '", signature$label_col, "'."), "info")
        }
      }
      if (is.null(state$active_region_path)) {
        state$active_region_path <- signature$region_col
      }
      next_generation <- state$helper_token + 1L
      state$helper_building <- TRUE
      set_helper_loading(TRUE, generation = next_generation)
      on.exit({
        state$helper_building <- FALSE
      }, add = TRUE)
      tryCatch({
        helper_base <- isolate(region_base_state())
        helper_x    <- map_sf()
        if (nrow(helper_base) > 0L && any(helper_base$dx_m != 0 | helper_base$dy_m != 0)) {
          helper_x <- tryCatch(
            apply_offsets(helper_x, helper_base, region_col = effective_region_col()),
            error = function(e) map_sf()
          )
        }
        helper_labels <- if (isTRUE(input$show_labels)) {
          shift_label_table_by_offsets(label_table(), helper_base)
        } else {
          FALSE
        }
        helper_label_values <- visible_label_ids()
        helper_region_col <- effective_region_col()
        helper_transition <- NULL
        bloom_col <- state$bloom_child_col_active
        if (!is.null(bloom_col) && nzchar(bloom_col %||% "") &&
            bloom_col %in% names(helper_x)) {
          pathb <- active_region_path()
          parent_key <- make_region_path(helper_x, pathb)
          child_key <- bloom_child_keys(helper_x, pathb, bloom_col)
          helper_x[["..dragmapr_bloom_parent_src.."]] <- parent_key

          built <- build_branch_transition_data(
            helper_x,
            parent_col = "..dragmapr_bloom_parent_src..",
            child_col = bloom_col,
            expanded = state$bloom_parents,
            dissolve = TRUE,
            animation = BLOOM_ANIMATION,
            duration_ms = BLOOM_DURATION_MS,
            easing = BLOOM_EASING,
            show_parent_ghost = FALSE,
            parent_ghost_opacity = 0,
            leaf_flip_strength = 0.16,
            leaf_child_scale = 0.86,
            leaf_expand_duration_factor = 0.82,
            leaf_collapse_duration_factor = 0.58,
            boundary = isTRUE(input$bloom_boundary %||% TRUE),
            boundary_behavior = "drag",
            boundary_label = "Drag to",
            boundary_drag_threshold = BLOOM_DRAG_THRESHOLD_PX,
            parent_key_col = "..dragmapr_bloom_parent..",
            child_key_col = "..dragmapr_bloom_child..",
            shell_col = "..dragmapr_bloom_shell.."
          )
          helper_x <- built$sf
          child_rows <- helper_x[["..dragmapr_bloom_shell.."]] == 0L
          helper_x[["..dragmapr_bloom_child.."]][child_rows] <- child_key
          helper_x[["..dragmapr_bloom_child.."]][!child_rows] <-
            helper_x[["..dragmapr_bloom_parent.."]][!child_rows]
          helper_region_col <- built$parent_key_col
          helper_transition <- built$transition

          # DOM-size guard: with both levels mounted in the helper, very
          # detailed child geometry would create too many SVG points.
          # Simplify the DISPLAY geometry only - every export and download
          # is built server-side from the full-resolution data.
          if (nrow(helper_x) > 2000L) {
            simplified <- tryCatch({
              bb <- sf::st_bbox(helper_x)
              tol <- sqrt((bb[["xmax"]] - bb[["xmin"]])^2 +
                            (bb[["ymax"]] - bb[["ymin"]])^2) * 5e-4
              sf::st_simplify(helper_x, dTolerance = tol,
                              preserveTopology = TRUE)
            }, error = function(e) NULL)
            if (!is.null(simplified) &&
                !any(sf::st_is_empty(sf::st_geometry(simplified)))) {
              helper_x <- simplified
              set_status(
                paste0("Large file: the interactive map uses simplified ",
                       "outlines for smooth animation. Exports keep full ",
                       "detail."),
                "info"
              )
            }
          }

          if (isTRUE(input$show_labels)) {
            helper_labels <- tryCatch({
              lab <- make_branch_bloom_labels(
                helper_x,
                parent_key_col = "..dragmapr_bloom_parent..",
                child_key_col = "..dragmapr_bloom_child..",
                shell_col = "..dragmapr_bloom_shell..",
                parent_label_col = label_col(),
                child_label_col = label_col(),
                show_parent_labels = TRUE,
                show_child_labels = FALSE,
                connector = FALSE
              )
              lab$label_id <- sub("^parent::", "", as.character(lab$label_id))
              lab <- apply_label_edits(lab, state$label_edits)
              pal <- label_palette()
              lab$label_color <- unname(pal[as.character(lab$label_id)])
              lab$label_color[is.na(lab$label_color)] <- "#111827"
              shift_label_table_by_offsets(lab, helper_base)
            }, error = function(e) helper_labels)
            if (is.data.frame(helper_labels)) {
              helper_label_values <- as.character(helper_labels$label_id)
            }
          }
        }

        drag_map_prototype(
          helper_x,
          region_col          = helper_region_col,
          label_col           = label_col(),
          labels              = helper_labels,
          label_values        = helper_label_values,
          label_marker        = !identical(input$label_marker_shape %||% "circle", "none"),
          label_marker_shape  = input$label_marker_shape %||% "circle",
          label_text_size     = input$label_text_size %||% 11,
          label_radius        = input$label_radius %||% 14,
          label_width         = input$label_width  %||% 64,
          label_height        = input$label_height %||% 30,
          label_box_width     = input$box_width  %||% 170,
          label_box_height    = input$box_height %||% 76,
          connector_color     = studio_color_value(input$connector_color, "#334155"),
          connector_linewidth = (input$connector_linewidth %||% 0.45) * 3,
          region_offsets      = if (!is.null(helper_transition)) isolate(bloom_offset_df()) else isolate(region_delta_state()),
          label_offsets       = isolate(
            read_csv_text(state$label_csv_cache) %||%
              empty_label_offsets(label_table())
          ),
          region_palette      = helper_region_palette(),
          show_legend         = isTRUE(input$show_legend) && length(visible_legend_values()) > 0L,
          max_legend_keys     = legend_max_keys(),
          legend_position     = input$legend_position %||% "bottom",
          legend_title        = input$legend_title %||% "Region",
          legend_values       = visible_legend_values(),
          legend_reflects_bloom = isTRUE(input$legend_reflect_bloom),
          map_background      = input$map_background %||% "white",
          connector_linetype  = input$connector_linetype %||% "solid",
          connector_endpoint  = input$connector_endpoint %||% "none",
          connector_smart     = isTRUE(input$connector_smart),
          show_origin_outlines = isTRUE(input$show_origin_outlines),
          show_movement_connectors = isTRUE(input$show_movement_connectors),
          show_movement_band   = isTRUE(input$show_movement_band),
          movement_connector_color = studio_color_value(input$movement_connector_color, "#64748b"),
          movement_connector_opacity = input$movement_connector_opacity %||% 0.72,
          movement_connector_linewidth = (input$movement_connector_linewidth %||% 0.45) * 3,
          movement_connector_linetype = input$movement_connector_linetype %||% "solid",
          movement_connector_endpoint = input$movement_connector_endpoint %||% "closed",
          show_drag_trail      = isTRUE(input$show_drag_trail),
          side_panel          = isTRUE(input$show_helper_panel %||% TRUE),
          transition          = helper_transition,
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

  observeEvent(list(input$reset_view, input$reset_view_requested), {
    session$sendCustomMessage("dragmapr-reset-view", list())
  }, ignoreInit = TRUE)

  observeEvent(input$show_labels, {
    session$sendCustomMessage("dragmapr-labels", list(labels = isTRUE(input$show_labels)))
  }, ignoreInit = FALSE)

  observeEvent(visible_legend_values(), {
    session$sendCustomMessage(
      "dragmapr-legend-values",
      list(values = as.list(visible_legend_values()))
    )
  }, ignoreInit = TRUE)

  observeEvent(visible_label_ids(), {
    session$sendCustomMessage(
      "dragmapr-label-values",
      list(values = as.list(visible_label_ids()))
    )
  }, ignoreInit = TRUE)

  observeEvent(list(input$label_auto_limit, input$label_auto_limit_n), {
    # Auto settings only apply when the user has not made an explicit label
    # selection. The multi-select remains the override for exact control.
    if (!is.null(state$label_filter_selection)) {
      return()
    }
    session$sendCustomMessage(
      "dragmapr-label-values",
      list(values = as.list(visible_label_ids()))
    )
  }, ignoreInit = TRUE)

  observeEvent(input$show_helper_panel, {
    session$sendCustomMessage(
      "dragmapr-side-panel",
      list(sidePanel = isTRUE(input$show_helper_panel %||% TRUE))
    )
  }, ignoreInit = TRUE)

  observeEvent(label_table(), {
    if (isTRUE(state$helper_loading) || isTRUE(state$helper_building)) return()
    # While bloom is armed the helper owns the dual-level label cache, so
    # full-table pushes are skipped. Crucially this also stops label_table()
    # (point-on-surface over every group) from being recomputed on every
    # bloom click - the reactive stays lazy because nothing pulls it.
    if (nzchar(state$bloom_child_col_active %||% "")) return()
    session$sendCustomMessage("dragmapr-label-data", list(labels = rows_for_message(label_table())))
  }, ignoreInit = TRUE)

  # Lightweight edit channel while bloom is armed: send only the edited
  # texts; the helper merges them by id into its dual-level cache.
  observeEvent(state$label_edits, {
    if (!nzchar(state$bloom_child_col_active %||% "")) return()
    edits <- state$label_edits
    if (is.null(edits) || length(edits) == 0L) return()
    rows <- lapply(names(edits), function(id) {
      list(label_id = id, label = edits[[id]])
    })
    session$sendCustomMessage("dragmapr-label-data", list(labels = rows))
  }, ignoreInit = TRUE)

  observeEvent(helper_region_palette(), {
    if (isTRUE(state$helper_loading) || isTRUE(state$helper_building)) return()
    session$sendCustomMessage("dragmapr-region-palette", list(
      palette = as.list(helper_region_palette())
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
         input$box_width, input$box_height, input$connector_color, input$connector_linewidth,
         input$connector_linetype, input$connector_endpoint, input$connector_smart,
         input$show_legend, input$legend_show_all, input$legend_reflect_bloom,
         input$legend_filter,
         input$legend_position, input$legend_title, input$show_origin_outlines, input$show_movement_connectors, input$show_movement_band,
         input$movement_connector_color, input$movement_connector_opacity,
         input$movement_connector_linewidth, input$movement_connector_linetype,
         input$movement_connector_endpoint,
         input$show_drag_trail, input$map_background),
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
        connectorColor = studio_color_value(input$connector_color, "#334155"),
        connectorLinewidth = (input$connector_linewidth %||% 0.45) * 3,
        connectorLinetype = input$connector_linetype %||% "solid",
        connectorEndpoint = input$connector_endpoint %||% "none",
        connectorSmart = isTRUE(input$connector_smart),
        showOriginOutlines = isTRUE(input$show_origin_outlines),
        showMovementConnectors = isTRUE(input$show_movement_connectors),
        showMovementBand = isTRUE(input$show_movement_band),
        movementConnectorColor = studio_color_value(input$movement_connector_color, "#64748b"),
        movementConnectorOpacity = input$movement_connector_opacity %||% 0.72,
        movementConnectorLinewidth = (input$movement_connector_linewidth %||% 0.45) * 3,
        movementConnectorLinetype = input$movement_connector_linetype %||% "solid",
        movementConnectorEndpoint = input$movement_connector_endpoint %||% "closed",
        showDragTrail = isTRUE(input$show_drag_trail),
        showLegend = isTRUE(input$show_legend) && length(visible_legend_values()) > 0L,
        maxLegendKeys = legend_max_keys(),
        legendValues = as.list(visible_legend_values()),
        legendReflectsBloom = isTRUE(input$legend_reflect_bloom),
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
    showModal(modalDialog(
      title = "Reset drag layout?",
      tags$p("This will zero all region and label offsets. The current layout will be saved to the undo stack so you can recover it with Undo."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset_layout", "Reset", class = "btn btn-warning")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_reset_layout, {
    removeModal()
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

  observeEvent(input$reset_label_positions, {
    region_offsets <- region_state()
    zero_labels <- empty_label_offsets(all_label_table())
    state$label_csv_cache <- csv_text(zero_labels)
    session$sendCustomMessage("dragmapr-state", list(
      regionOffsets = rows_for_message(region_offsets),
      labelOffsets = rows_for_message(zero_labels)
    ))
    do_refresh()
    set_status("Reset label positions without moving regions.", "ok")
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
      show_connectors     = isTRUE(input$show_connectors),
      legend_show_all     = isTRUE(input$legend_show_all),
      legend_reflects_bloom = isTRUE(input$legend_reflect_bloom),
      legend_values       = legend_values(),
      label_values        = visible_label_ids(),
      legend_position     = input$legend_position %||% "bottom",
      legend_title        = input$legend_title %||% "Region",
      map_background      = input$map_background %||% "white",
      annotation_mode     = input$annotation_mode %||% "labels",
      label_marker_shape  = input$label_marker_shape %||% "circle",
      label_text_size     = input$label_text_size %||% 11,
      label_radius        = input$label_radius %||% 14,
      label_width         = input$label_width %||% 64,
      label_height        = input$label_height %||% 30,
      box_width           = input$box_width %||% 170,
      box_height          = input$box_height %||% 76,
      marker_size         = (input$label_radius %||% 14) / 3,
      connector_color     = studio_color_value(input$connector_color, "#334155"),
      connector_linewidth = input$connector_linewidth %||% 0.45,
      connector_type      = input$connector_type %||% "straight",
      connector_linetype  = input$connector_linetype %||% "solid",
      connector_endpoint  = input$connector_endpoint %||% "none",
      connector_smart     = isTRUE(input$connector_smart),
      show_origin_outlines = isTRUE(input$show_origin_outlines),
      show_movement_connectors = isTRUE(input$show_movement_connectors),
      show_movement_band   = isTRUE(input$show_movement_band),
      movement_connector_color = studio_color_value(input$movement_connector_color, "#64748b"),
      movement_connector_opacity = input$movement_connector_opacity %||% 0.72,
      movement_connector_linewidth = input$movement_connector_linewidth %||% 0.45,
      movement_connector_linetype = input$movement_connector_linetype %||% "solid",
      movement_connector_endpoint = input$movement_connector_endpoint %||% "closed",
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
    content     = function(file) {
      req(file.exists(helper_file))
      file.copy(helper_file, file, overwrite = TRUE)
    },
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
  offsets <- normalize_studio_label_offsets(label_offsets)
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

if (interactive()) {
  shinyApp(ui, server)
}
