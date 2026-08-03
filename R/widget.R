#' dragmapr widget options
#'
#' These constructors separate options that affect source geometry from options
#' that can be updated live in the browser.
#'
#' @param width,height Widget canvas dimensions in CSS pixels.
#' @param region_palette Optional named color vector.
#' @param show_origin_outlines,show_movement_connectors,show_drag_trail Logical
#'   display toggles.
#' @param map_background One of `"white"`, `"transparent"`, `"light_grid"`, or
#'   `"dark"`.
#' @param connector_color Browser label-connector color.
#' @param connector_linewidth Browser label-connector width.
#' @param draggable_regions,draggable_labels Enable region and label dragging.
#'
#' @return A named list of options.
#' @export
dragmapr_geometry_options <- function(width = 7200, height = 4800) {
  width <- positive_scalar(width, "`width`")
  height <- positive_scalar(height, "`height`")
  list(width = width, height = height)
}

#' @rdname dragmapr_geometry_options
#' @export
dragmapr_display_options <- function(region_palette = NULL,
                                     show_origin_outlines = FALSE,
                                     show_movement_connectors = FALSE,
                                     show_drag_trail = FALSE,
                                     map_background = c("white", "transparent", "light_grid", "dark"),
                                     connector_color = "#334155",
                                     connector_linewidth = 1.3) {
  map_background <- match.arg(map_background)
  if (!is.null(region_palette) && (is.null(names(region_palette)) || any(names(region_palette) == ""))) {
    stop("`region_palette` must be a named vector when supplied.", call. = FALSE)
  }
  list(
    regionPalette = if (is.null(region_palette)) NULL else as.list(region_palette),
    showOriginOutlines = isTRUE(flag_scalar(show_origin_outlines, "`show_origin_outlines`")),
    showMovementConnectors = isTRUE(flag_scalar(show_movement_connectors, "`show_movement_connectors`")),
    showDragTrail = isTRUE(flag_scalar(show_drag_trail, "`show_drag_trail`")),
    mapBackground = map_background,
    connectorColor = color_scalar(connector_color, "`connector_color`"),
    connectorLinewidth = positive_scalar(connector_linewidth, "`connector_linewidth`")
  )
}

#' @rdname dragmapr_geometry_options
#' @export
dragmapr_interaction_options <- function(draggable_regions = TRUE,
                                         draggable_labels = TRUE) {
  list(
    draggableRegions = isTRUE(flag_scalar(draggable_regions, "`draggable_regions`")),
    draggableLabels = isTRUE(flag_scalar(draggable_labels, "`draggable_labels`"))
  )
}

#' Create a native dragmapr htmlwidget
#'
#' @param x An `sf` object in a projected CRS.
#' @param region_col Column defining draggable groups.
#' @param label_col Column used for default region-label text.
#' @param labels Show draggable labels, omit labels, or pass a label table.
#' @param state Optional `dragmapr_state`.
#' @param region_palette Optional named colour vector passed to
#'   [dragmapr_display_options()]. This is a convenience alias for users who
#'   want widget, proxy, and static-render palettes to share one argument name.
#' @param geometry_options Options from [dragmapr_geometry_options()].
#' @param display_options Options from [dragmapr_display_options()].
#' @param interaction_options Options from [dragmapr_interaction_options()].
#' @param width,height htmlwidget container size.
#' @param elementId Optional widget element id.
#'
#' @return An htmlwidget.
#' @export
dragmapr_widget <- function(x,
                            region_col,
                            label_col = region_col,
                            labels = TRUE,
                            state = NULL,
                            region_palette = NULL,
                            geometry_options = dragmapr_geometry_options(),
                            display_options = dragmapr_display_options(),
                            interaction_options = dragmapr_interaction_options(),
                            width = "100%",
                            height = "650px",
                            elementId = NULL) {
  if (!is.null(region_palette)) {
    if (is.null(names(region_palette)) || any(names(region_palette) == "")) {
      stop("`region_palette` must be a named vector when supplied.", call. = FALSE)
    }
    display_options$regionPalette <- as.list(region_palette)
  }
  payload <- dragmapr_widget_payload(
    x = x,
    region_col = region_col,
    label_col = label_col,
    labels = labels,
    state = state,
    geometry_options = geometry_options,
    display_options = display_options,
    interaction_options = interaction_options
  )

  htmlwidgets::createWidget(
    name = "dragmapr",
    x = payload,
    width = width,
    height = height,
    package = "dragmapr",
    elementId = elementId,
    dependencies = dragmapr_widget_dependencies()
  )
}

#' Open the interactive dragmapr editor
#'
#' A friendly front-door to the native draggable editor and the composition step
#' of the state-first workflow: compute a layout, edit it, render the edited
#' state. It accepts the objects produced upstream -- a projected `sf`, an
#' explodemap `grouped_exploded_map`, or a `dragmapr_layout` -- together with an
#' optional [dragmapr_state()], and returns a configured [dragmapr_widget()]
#' ready to embed in Shiny, R Markdown, or the viewer.
#'
#' For an explodemap layout, pass the composed state explicitly so the editor
#' opens on the exploded composition rather than the bare geometry:
#'
#' ```r
#' layout <- explodemap::explode_grouped(x, region_col = "region")
#' state  <- explodemap::as_dragmapr_state(layout)
#' dragmapr_edit(layout, state = state)
#' ```
#'
#' To capture edits back into a `dragmapr_state` (to persist, merge, or
#' re-render them), read the widget's state input in Shiny with
#' [dragmapr_widget_state()]:
#'
#' ```r
#' output$map <- renderDragmapr(dragmapr_edit(layout, state = state))
#' observeEvent(input$map_state, {
#'   state <- dragmapr_widget_state(input$map_state)
#' })
#' ```
#'
#' @param x A projected `sf` object, an explodemap `grouped_exploded_map`, or a
#'   `dragmapr_layout`. Layout objects supply their own draggable geometry and
#'   region column.
#' @param region_col Column defining draggable groups. Required when `x` is a
#'   raw `sf`; inferred from layout objects when omitted.
#' @param state Optional [dragmapr_state()] giving the initial composition.
#' @param ... Further arguments passed to [dragmapr_widget()], for example
#'   `label_col`, `display_options`, `width`, or `height`.
#'
#' @return A `dragmapr` htmlwidget (the interactive editor).
#' @seealso [dragmapr_widget()], [dragmapr_widget_state()].
#' @export
dragmapr_edit <- function(x, region_col = NULL, state = NULL, ...) {
  editable <- dragmapr_editable_geometry(x, region_col)
  dragmapr_widget(
    editable$sf,
    region_col = editable$region_col,
    state = state,
    ...
  )
}

# Duck-type the supported inputs into draggable geometry + a region column,
# without taking a hard dependency on explodemap's classes.
dragmapr_editable_geometry <- function(x, region_col) {
  if (inherits(x, "sf")) {
    if (is.null(region_col) || !nzchar(region_col)) {
      stop("`region_col` is required when `x` is an sf object.", call. = FALSE)
    }
    return(list(sf = x, region_col = region_col))
  }
  # explodemap dragmapr_layout: list with $sf and $region_col.
  if (is.list(x) && inherits(x[["sf"]], "sf") && !is.null(x[["region_col"]])) {
    return(list(sf = x[["sf"]], region_col = region_col %||% x[["region_col"]]))
  }
  # explodemap grouped_exploded_map: $sf_grouped and $diagnostics$region_col.
  if (is.list(x) && inherits(x[["sf_grouped"]], "sf")) {
    rc <- region_col %||% x[["diagnostics"]][["region_col"]]
    if (is.null(rc) || !nzchar(rc)) {
      stop("Could not infer `region_col` from the layout; supply it explicitly.",
           call. = FALSE)
    }
    return(list(sf = x[["sf_grouped"]], region_col = rc))
  }
  stop(
    "`x` must be a projected sf object, an explodemap grouped_exploded_map, ",
    "or a dragmapr_layout.",
    call. = FALSE
  )
}

#' Shiny bindings for dragmapr widgets
#'
#' @param outputId Shiny output id.
#' @param width,height Output dimensions.
#' @param expr Expression returning [dragmapr_widget()].
#' @param env Evaluation environment.
#' @param quoted Is `expr` quoted?
#'
#' @return Shiny output/render functions.
#' @export
dragmaprOutput <- function(outputId, width = "100%", height = "650px") {
  htmlwidgets::shinyWidgetOutput(outputId, "dragmapr", width, height, package = "dragmapr")
}

#' @rdname dragmaprOutput
#' @export
renderDragmapr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, dragmaprOutput, env, quoted = TRUE)
}

#' Update a dragmapr widget in place
#'
#' @param session Shiny session.
#' @param outputId Widget output id.
#' @param ... Live updates to apply. Display options use the R API, for example
#'   `show_origin_outlines` or `map_background`. The composition field
#'   `selected_feature` can also be updated; pass `NULL` or `""` to clear the
#'   current selection. Use `remove_features = c(...)` to remove one or more
#'   feature ids from the live widget, or `delete_selected = TRUE` to remove
#'   the current selection.
#' @param generation Optional widget generation token. When supplied, the
#'   browser ignores the update unless it targets the active render generation.
#' @param revision Optional server-side revision token to echo in the widget
#'   acknowledgement input.
#'
#' @return Invisibly returns the sent message.
#' @export
updateDragmapr <- function(session, outputId, ..., generation = NULL, revision = NULL) {
  if (missing(session) || is.null(session)) {
    stop("`session` is required.", call. = FALSE)
  }
  if (!is.character(outputId) || length(outputId) != 1L || is.na(outputId) || !nzchar(outputId)) {
    stop("`outputId` must be a single non-empty string.", call. = FALSE)
  }
  dots <- list(...)
  message <- dragmapr_update_message(dots)
  if (!is.null(generation)) {
    message$generation <- generation_scalar(generation, "`generation`")
  }
  if (!is.null(revision)) {
    message$serverRevision <- generation_scalar(revision, "`revision`")
  }
  session$sendCustomMessage("dragmapr-update", c(list(id = session$ns(outputId)), message))
  invisible(message)
}

#' Reconstruct a dragmapr state from a browser state event
#'
#' The native widget reports edits to Shiny through an input named
#' `paste0(outputId, "_state")`. This function turns that value back into a
#' [dragmapr_state()] so server code can persist, merge, or re-render the user's
#' live composition. It is the inbound half of the widget bridge that
#' [dragmapr_widget()] / [updateDragmapr()] form on the outbound side.
#'
#' The browser's monotonic `revision` becomes the state `version` unchanged --
#' the client owns the live counter, so the server records the edit faithfully
#' rather than bumping it again. The empty-string selection sentinel is mapped
#' back to `NULL`.
#'
#' @param value The list received from `input[[paste0(outputId, "_state")]]`.
#'   `NULL` (no event yet) returns `NULL`.
#'
#' @return A `dragmapr_state`, or `NULL` if `value` is `NULL`.
#' @export
#' @examples
#' \dontrun{
#' shiny::observeEvent(input$map_state, {
#'   state <- dragmapr_widget_state(input$map_state)
#'   write_dragmapr_state(state, "composition.json")
#' })
#' }
dragmapr_widget_state <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.list(value)) {
    stop("`value` must be the list sent by the dragmapr widget state input.",
         call. = FALSE)
  }
  selected <- value$selected_feature
  if (!is.null(selected) &&
      (length(selected) != 1L || is.na(selected) || !nzchar(selected))) {
    selected <- NULL
  }
  expanded <- value$expanded_groups
  expanded <- if (is.null(expanded)) character() else as.character(unlist(expanded))

  dragmapr_state(
    level = value$level %||% "region",
    region_col = value$region_col %||% value$binding$region_col %||% NULL,
    label_id_col = value$label_id_col %||% value$binding$label_id_col %||% NULL,
    region_offsets = widget_rows_to_df(value$region_offsets),
    label_offsets = widget_rows_to_df(value$label_offsets),
    expanded_groups = expanded,
    view = value$view,
    version = value$revision %||% 0L,
    crs = value$crs %||% NULL,
    geometry_id = value$geometry_id %||% NULL,
    selected_feature = selected,
    binding = value$binding %||% NULL,
    schema_version = value$schema_version %||% "1.1.0",
    package_version = value$package_version %||% "0.0.0"
  )
}

# Coerce the offset rows sent by the browser (a list of per-row lists, or a
# data frame) into a data frame. Returns NULL for an empty/absent table so the
# dragmapr_state() constructor supplies the correct empty schema.
widget_rows_to_df <- function(rows) {
  if (is.null(rows) || length(rows) == 0L) {
    return(NULL)
  }
  if (is.data.frame(rows)) {
    return(rows)
  }
  do.call(
    rbind,
    lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  )
}

dragmapr_widget_payload <- function(x, region_col, label_col, labels, state,
                                    geometry_options, display_options,
                                    interaction_options) {
  validate_dragmapr_sf(x)
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found.", call. = FALSE)
  }
  if (!identical(labels, FALSE) && !is.data.frame(labels) && !label_col %in% names(x)) {
    stop("label_col '", label_col, "' not found.", call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop(
      "Project `x` before using dragmapr_widget(). ",
      "Use prepare_dragmapr_sf(x) or sf::st_transform(x, crs = 3857).",
      call. = FALSE
    )
  }

  export <- x
  export$drag_region <- as.character(export[[region_col]])
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(export, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
               layer_options = "RFC7946=NO")
  geojson <- jsonlite::read_json(tmp, simplifyVector = FALSE)

  label_data <- if (isTRUE(labels)) {
    make_region_labels(x, region_col = region_col, label_col = label_col)
  } else if (is.data.frame(labels)) {
    as_drag_labels(labels)
  } else {
    data.frame(label_id = character(), region = character(), label = character(), x = numeric(), y = numeric())
  }

  state <- if (is.null(state)) {
    dragmapr_state(
      region_col = region_col,
      label_id_col = "label_id",
      region_offsets = data.frame(region = character(), dx_m = numeric(), dy_m = numeric()),
      label_offsets = data.frame(label_id = character(), region = character(), dx_m = numeric(), dy_m = numeric())
    )
  } else {
    validate_dragmapr_state(state)
  }
  if (!identical(state$region_col, region_col)) {
    state <- dragmapr_state(
      level = state$level,
      region_col = region_col,
      label_id_col = state$label_id_col,
      region_offsets = state$region_offsets,
      label_offsets = state$label_offsets,
      expanded_groups = state$expanded_groups,
      view = state$view,
      version = state$version,
      crs = state$crs,
      geometry_id = state$geometry_id,
      selected_feature = state$selected_feature,
      schema_version = state$schema_version,
      package_version = state$package_version
    )
  }

  state_payload <- snapshot_dragmapr_state(state)
  state_payload$region_offsets <- dataframe_records(state_payload$region_offsets)
  state_payload$label_offsets <- dataframe_records(state_payload$label_offsets)

  list(
    widgetId = NULL,
    generation = as.integer(stats::runif(1, min = 1, max = .Machine$integer.max)),
    revision = unname(state$version),
    crs = state$crs,
    geometryId = state$geometry_id,
    selectedFeature = state$selected_feature %||% "",
    regionCol = state$region_col,
    labelIdCol = state$label_id_col,
    geojson = geojson,
    labels = dataframe_records(label_data),
    state = state_payload,
    geometry = geometry_options,
    display = display_options,
    interaction = interaction_options
  )
}

dragmapr_widget_dependencies <- function() {
  list(
    htmltools::htmlDependency(
      name = "d3",
      version = "7.9.0",
      src = "prototype",
      script = "d3.v7.min.js",
      package = "dragmapr"
    )
  )
}

dragmapr_update_message <- function(dots) {
  if (length(dots) == 0L) {
    return(list(display = list()))
  }
  display_allowed <- c(
    show_origin_outlines = "showOriginOutlines",
    show_movement_connectors = "showMovementConnectors",
    show_drag_trail = "showDragTrail",
    map_background = "mapBackground",
    connector_color = "connectorColor",
    connector_linewidth = "connectorLinewidth",
    region_palette = "regionPalette"
  )
  composition_allowed <- c(
    selected_feature = "selectedFeature",
    remove_features = "removeFeatures",
    delete_selected = "deleteSelected"
  )
  unknown <- setdiff(names(dots), c(names(display_allowed), names(composition_allowed)))
  if (length(unknown) > 0L) {
    stop("Unsupported live update option(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  message <- list()
  out <- list()
  for (nm in names(dots)) {
    value <- dots[[nm]]
    if (nm %in% names(composition_allowed)) {
      if (nm == "selected_feature") {
        # An empty string clears the selection; the browser treats "" as none.
        value <- selection_scalar(value)
      } else if (nm == "remove_features") {
        value <- remove_features_vector(value)
      } else if (nm == "delete_selected") {
        value <- isTRUE(flag_scalar(value, "`delete_selected`"))
      }
      message[[composition_allowed[[nm]]]] <- value
      next
    }
    if (nm %in% c("show_origin_outlines", "show_movement_connectors", "show_drag_trail")) {
      value <- isTRUE(flag_scalar(value, paste0("`", nm, "`")))
    } else if (nm == "map_background") {
      value <- match.arg(value, c("white", "transparent", "light_grid", "dark"))
    } else if (nm == "connector_color") {
      value <- color_scalar(value, "`connector_color`")
    } else if (nm == "connector_linewidth") {
      value <- positive_scalar(value, "`connector_linewidth`")
    } else if (nm == "region_palette" && !is.null(value)) {
      if (is.null(names(value)) || any(names(value) == "")) {
        stop("`region_palette` must be a named vector when supplied.", call. = FALSE)
      }
      value <- as.list(value)
    }
    out[[display_allowed[[nm]]]] <- value
  }
  message$display <- out
  message
}

# Coerce a selected_feature update to a JSON-safe scalar. NULL, NA or "" all
# mean "clear" and are sent as an empty string.
selection_scalar <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return("")
  }
  x <- as.character(x)
  if (length(x) != 1L) {
    stop("`selected_feature` must be NULL or a single string.", call. = FALSE)
  }
  x
}

remove_features_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

positive_scalar <- function(x, name) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x) || x <= 0) {
    stop(name, " must be a positive number.", call. = FALSE)
  }
  unname(x)
}

flag_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

color_scalar <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(name, " must be a single color string.", call. = FALSE)
  }
  unname(x)
}

generation_scalar <- function(x, name) {
  if (is.character(x)) {
    if (length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop(name, " must be a single non-empty string or finite number.", call. = FALSE)
    }
    return(unname(x))
  }
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x)) {
    stop(name, " must be a single non-empty string or finite number.", call. = FALSE)
  }
  unname(x)
}

dataframe_records <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }
  jsonlite::fromJSON(
    jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE),
    simplifyVector = FALSE
  )
}
