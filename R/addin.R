#' RStudio addin for the interactive drag-map prototype
#'
#' Launches a compact Shiny gadget that lets you pick a projected `sf` object
#' from an environment, choose the region and label columns, and
#' interact with the D3 drag-map prototype inside the IDE viewer pane. When you
#' click **Done**, the current region and label offsets are assigned as
#' `region_offsets` and `label_offsets` in the same environment, ready to pass to
#' [render_dragged_map()].
#'
#' The addin appears under **Addins > Launch dragmapr** in RStudio once the
#' package is installed.
#'
#' @param env Environment to scan for `sf` objects and receive exported offset
#'   tables. Defaults to `.GlobalEnv`, which is what RStudio uses for addins.
#'
#' @return Invisibly returns a list with elements `region_offsets` and
#'   `label_offsets` (both data frames). The same data frames are also assigned
#'   into `env`.
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

  serve_prototype <- function(x, region_col, label_col) {
    tmp_dir <- tempfile("dragmapr_addin_")
    dir.create(tmp_dir, recursive = TRUE)
    tmp_file <- file.path(tmp_dir, "index.html")
    drag_map_prototype(
      x = x,
      region_col = region_col,
      label_col = label_col,
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
      shiny::tags$style(shiny::HTML("
        .dragmapr-sidebar {
          padding: 10px 12px;
          border-right: 1px solid #d8dee8;
          background: #f7f8fb;
          display: flex;
          flex-direction: column;
          gap: 10px;
          min-width: 200px;
          max-width: 220px;
        }
        .dragmapr-sidebar label { font-size: 12px; font-weight: 600; color: #334155; }
        .dragmapr-sidebar select { width: 100%; font-size: 12px; }
        .dragmapr-iframe-wrap { flex: 1; overflow: hidden; }
        .dragmapr-iframe-wrap iframe { width: 100%; height: 100%; border: none; }
        .dragmapr-layout {
          display: flex;
          height: calc(100vh - 42px);
        }
        .dragmapr-status {
          font-size: 11px;
          color: #64748b;
          margin-top: 4px;
          min-height: 16px;
        }
        .dragmapr-status.ok  { color: #16a34a; }
        .dragmapr-status.err { color: #dc2626; }
      "))
    ),
    shiny::div(
      class = "dragmapr-layout",
      shiny::div(
        class = "dragmapr-sidebar",
        shiny::div(
          shiny::tags$label("sf object"),
          shiny::selectInput(
            "sf_name", NULL,
            choices = initial_sf_choices,
            selected = initial_sf,
            width = "100%"
          )
        ),
        shiny::actionButton(
          "refresh_objects", "Refresh objects",
          style = "width: 100%; font-size: 12px;"
        ),
        shiny::div(
          shiny::tags$label("Region column"),
          shiny::selectInput("region_col", NULL, choices = character(0), width = "100%")
        ),
        shiny::div(
          shiny::tags$label("Label column"),
          shiny::selectInput("label_col", NULL, choices = character(0), width = "100%")
        ),
        shiny::actionButton(
          "render_btn", "Render prototype",
          style = "width: 100%; font-size: 12px;"
        ),
        shiny::uiOutput("status_msg")
      ),
      shiny::div(
        class = "dragmapr-iframe-wrap",
        shiny::uiOutput("prototype_frame")
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
      status = "",
      status_class = "info"
    )

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
        serve_prototype(x, region_col, label_col),
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

    output$prototype_frame <- shiny::renderUI({
      src <- rv$frame_src
      if (is.null(src)) {
        return(shiny::div(
          style = "display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;font-size:13px;",
          "Select an sf object and click 'Render prototype'."
        ))
      }
      shiny::tags$iframe(
        class = "dragmapr-helper-frame",
        src = src,
        style = "width:100%;height:100%;border:none;"
      )
    })

    output$status_msg <- shiny::renderUI({
      shiny::div(class = paste("dragmapr-status", rv$status_class), rv$status)
    })

    shiny::observeEvent(input$done, {
      region_offsets <- rv$region_offsets
      label_offsets <- rv$label_offsets
      assign("region_offsets", region_offsets, envir = env)
      assign("label_offsets", label_offsets, envir = env)
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
        label_offsets = label_offsets
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
