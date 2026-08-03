#' Render a static map from a Spatial Studio project bundle
#'
#' `render_dragmapr_project()` turns a Spatial Studio project ZIP, or an
#' extracted project directory, into the same `ggplot2` image produced by
#' [render_dragged_map()]. It reads the saved source geometry, region offsets,
#' label offsets, label table, palette, and metadata, then validates that the
#' pieces still match before rendering.
#'
#' @param project Path to a `dragmapr-project.zip` file or an extracted project
#'   directory created by Spatial Studio.
#' @param file Optional output image path. When supplied, the rendered plot is
#'   saved with [ggplot2::ggsave()].
#' @param width,height,dpi Output settings used when `file` is supplied.
#' @param title Optional plot title. Defaults to the title saved in project
#'   metadata, when available.
#' @param quiet Use `TRUE` to suppress validation messages about zero-filled or
#'   extra offset rows.
#' @param ... Additional arguments passed to [render_dragged_map()], such as
#'   `legend_position`, `show_legend`, or `label_marker_shape`.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' if(interactive()){
#' render_dragmapr_project("dragmapr-project.zip", file = "map.png")
#' }
render_dragmapr_project <- function(project,
                                    file = NULL,
                                    width = 10,
                                    height = 8,
                                    dpi = 300,
                                    title = NULL,
                                    quiet = FALSE,
                                    ...) {
  bundle <- read_dragmapr_project(project)
  metadata <- bundle$metadata
  region_col <- project_default(metadata$region_col, NULL)
  if (is.null(region_col) || !nzchar(region_col)) {
    stop(
      "Project metadata is missing `region_col`. ",
      "Re-export the project from Spatial Studio, or add region_col to metadata.json.",
      call. = FALSE
    )
  }
  if (!region_col %in% names(bundle$source)) {
    stop(
      "Project metadata uses region_col = '", region_col, "', but that column ",
      "is not present in source.gpkg. Available columns: ",
      paste(setdiff(names(bundle$source), attr(bundle$source, "sf_column")), collapse = ", "),
      call. = FALSE
    )
  }
  label_col <- project_default(metadata$label_col, region_col)
  if (!label_col %in% names(bundle$source)) {
    label_col <- region_col
  }
  plot_title <- project_default(title, project_default(metadata$title, NULL))
  validate_dragmapr_project(bundle, region_col = region_col, quiet = quiet)
  project_labels <- bundle$labels
  if (isTRUE(project_default(metadata$connector_smart, FALSE)) && !is.null(project_labels)) {
    project_labels <- project_smart_connector_labels(project_labels, bundle$label_offsets)
  }

  defaults <- list(
    show_legend = project_default(metadata$show_legend, TRUE),
    legend_position = project_default(metadata$legend_position, "bottom"),
    legend_title = project_default(metadata$legend_title, "Region"),
    legend_values = project_default(metadata$legend_values, NULL),
    map_background = project_default(metadata$map_background, "white"),
    labels = if (isFALSE(project_default(metadata$show_labels, TRUE))) FALSE else project_labels,
    label_values = project_default(metadata$label_values, NULL),
    label_marker_shape = project_default(metadata$label_marker_shape, "circle"),
    show_label_marker = !identical(project_default(metadata$label_marker_shape, "circle"), "none"),
    marker_size = project_default(metadata$marker_size, 3.2),
    connector_color = project_default(metadata$connector_color, "#334155"),
    connector_linewidth = project_default(metadata$connector_linewidth, 0.35),
    connector_linetype = project_default(metadata$connector_linetype, "solid"),
    connector_endpoint = project_default(metadata$connector_endpoint, "none"),
    show_origin_outlines = isTRUE(project_default(metadata$show_origin_outlines, FALSE)),
    show_movement_connectors = isTRUE(project_default(metadata$show_movement_connectors, FALSE)),
    movement_connector_color = project_default(metadata$movement_connector_color, "#64748b"),
    movement_connector_opacity = project_default(metadata$movement_connector_opacity, 0.72),
    movement_connector_linewidth = project_default(metadata$movement_connector_linewidth, 0.45),
    movement_connector_linetype = project_default(metadata$movement_connector_linetype, "solid"),
    movement_connector_endpoint = project_default(metadata$movement_connector_endpoint, "closed"),
    label_padding = project_default(metadata$label_padding, 0.12)
  )
  dots <- list(...)
  defaults[names(dots)] <- dots

  args <- c(
    list(
      x = bundle$source,
      region_offsets = bundle$region_offsets,
      region_col = region_col,
      label_col = label_col,
      label_offsets = bundle$label_offsets,
      region_palette = bundle$region_palette,
      title = plot_title,
      file = file,
      width = width,
      height = height,
      dpi = dpi
    ),
    defaults
  )
  do.call(render_dragged_map, args)
}

#' Read a dragmapr project bundle
#'
#' Reads a Spatial Studio project ZIP or extracted project directory and
#' returns its components as a named list. This is the low-level companion to
#' [render_dragmapr_project()]: use it when you want to access the raw source
#' geometry, offsets, labels, or palette programmatically before rendering.
#'
#' @param project Path to a `dragmapr-project.zip` file or an extracted project
#'   directory created by Spatial Studio.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`source`}{The source `sf` object read from `source.gpkg`.}
#'     \item{`region_offsets`}{Data frame from `drag_region_offsets.csv`, or
#'       `NULL` if not present.}
#'     \item{`label_offsets`}{Data frame from `drag_label_offsets.csv`, or
#'       `NULL` if not present.}
#'     \item{`labels`}{Label table from `labels.csv` (as returned by
#'       [as_drag_labels()]), or `NULL`.}
#'     \item{`region_palette`}{Named character vector of colors from
#'       `palette.csv`, or `NULL`.}
#'     \item{`metadata`}{Named list parsed from `metadata.json`.}
#'     \item{`path`}{Path to the extracted project directory.}
#'   }
#' @export
#' @seealso [render_dragmapr_project()] to render a project bundle directly;
#'   [write_dragmapr_project()] to create a project bundle from R objects.
#' @examples
#' if(interactive()){
#' bundle <- read_dragmapr_project("dragmapr-project.zip")
#' names(bundle)
#' nrow(bundle$source)
#' }
read_dragmapr_project <- function(project) {
  if (!is.character(project) || length(project) != 1L || !nzchar(project)) {
    stop("`project` must be a path to a Spatial Studio ZIP file or extracted directory.", call. = FALSE)
  }
  if (!file.exists(project)) {
    stop("Project path does not exist: ", project, call. = FALSE)
  }
  project_dir <- if (dir.exists(project)) {
    project
  } else {
    exdir <- tempfile("dragmapr_project_")
    dir.create(exdir, recursive = TRUE)
    unzip_dragmapr_project(project, exdir)
    exdir
  }

  source_file <- file.path(project_dir, "source.gpkg")
  if (!file.exists(source_file)) {
    stop(
      "Project bundle is missing source.gpkg. ",
      "Download a fresh Bundle ZIP from Spatial Studio and try again.",
      call. = FALSE
    )
  }

  metadata <- read_project_metadata(file.path(project_dir, "metadata.json"))
  labels <- read_optional_project_csv(project_dir, "labels.csv")
  if (is.null(labels)) {
    labels <- read_optional_project_csv(project_dir, "drag_labels.csv")
  }
  palette <- read_optional_project_csv(project_dir, "palette.csv")
  state_file <- file.path(project_dir, "state.json")
  state <- if (file.exists(state_file)) {
    read_dragmapr_state(state_file)
  } else {
    NULL
  }

  list(
    source = sf::st_read(source_file, quiet = TRUE),
    region_offsets = read_optional_project_csv(project_dir, "drag_region_offsets.csv"),
    label_offsets = read_optional_project_csv(project_dir, "drag_label_offsets.csv"),
    state = state,
    labels = if (is.null(labels)) NULL else as_drag_labels(labels),
    region_palette = project_palette_vector(palette),
    metadata = metadata,
    path = project_dir
  )
}

unzip_dragmapr_project <- function(zip_file, exdir) {
  listing <- tryCatch(
    utils::unzip(zip_file, list = TRUE),
    error = function(e) {
      stop(
        "Could not read the project ZIP. ",
        "Check that it is a valid Spatial Studio project bundle.",
        call. = FALSE
      )
    }
  )
  names_in_zip <- as.character(listing$Name)
  unsafe <- grepl("(^|/|\\\\)\\.\\.(/|\\\\|$)", names_in_zip) |
    grepl("^([A-Za-z]:|/|\\\\)", names_in_zip)
  if (any(unsafe)) {
    stop(
      "Project ZIP contains unsafe file paths. ",
      "For safety, dragmapr will not extract this archive.",
      call. = FALSE
    )
  }
  ok <- tryCatch(
    {
      utils::unzip(zip_file, exdir = exdir)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!isTRUE(ok)) {
    stop(
      "Could not unzip the project bundle. ",
      "Check that it is a valid .zip file and try again.",
      call. = FALSE
    )
  }
  invisible(exdir)
}

read_optional_project_csv <- function(project_dir, name) {
  path <- file.path(project_dir, name)
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      stop("Could not read ", name, ": ", conditionMessage(e), call. = FALSE)
    }
  )
}

read_project_metadata <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) {
      stop("Could not read metadata.json: ", conditionMessage(e), call. = FALSE)
    }
  )
}

project_palette_vector <- function(palette) {
  if (is.null(palette)) {
    return(NULL)
  }
  names(palette) <- tolower(names(palette))
  if (!all(c("region", "color") %in% names(palette))) {
    stop("palette.csv must contain `region` and `color` columns.", call. = FALSE)
  }
  out <- stats::setNames(as.character(palette$color), as.character(palette$region))
  bad <- is.na(names(out)) | !nzchar(names(out)) | is.na(out) | !nzchar(out)
  if (any(bad)) {
    stop("palette.csv contains empty region or color values.", call. = FALSE)
  }
  out
}

validate_dragmapr_project <- function(bundle, region_col, quiet = FALSE) {
  regions <- natural_sort(unique(as.character(bundle$source[[region_col]])))

  if (is.null(bundle$region_offsets)) {
    if (!quiet) {
      message("dragmapr: project has no drag_region_offsets.csv; using zero movement for all regions.")
    }
  } else {
    region_offsets <- normalize_offsets(bundle$region_offsets, source = "drag_region_offsets.csv")
    missing_regions <- setdiff(regions, region_offsets$region)
    extra_regions <- setdiff(region_offsets$region, regions)
    if (length(missing_regions) > 0L && !quiet) {
      message(
        "dragmapr: drag_region_offsets.csv has no row for ",
        length(missing_regions), " region(s); using zero movement for: ",
        paste(utils::head(missing_regions, 8), collapse = ", "),
        if (length(missing_regions) > 8L) ", ..." else ""
      )
    }
    if (length(extra_regions) > 0L && !quiet) {
      message(
        "dragmapr: drag_region_offsets.csv contains region(s) not found in source.gpkg: ",
        paste(utils::head(extra_regions, 8), collapse = ", "),
        if (length(extra_regions) > 8L) ", ..." else ""
      )
    }
    bundle$region_offsets <- region_offsets
  }

  if (!is.null(bundle$labels)) {
    label_regions <- setdiff(unique(as.character(bundle$labels$region)), regions)
    if (length(label_regions) > 0L) {
      stop(
        "labels.csv refers to region(s) not found in source.gpkg: ",
        paste(utils::head(label_regions, 8), collapse = ", "),
        if (length(label_regions) > 8L) ", ..." else "",
        ". Check that metadata.json uses the correct region_col.",
        call. = FALSE
      )
    }
  }

  if (!is.null(bundle$label_offsets) && !is.null(bundle$labels)) {
    label_offsets <- normalize_label_state(bundle$label_offsets, source = "drag_label_offsets.csv")
    missing_labels <- setdiff(bundle$labels$label_id, label_offsets$label_id)
    extra_labels <- setdiff(label_offsets$label_id, bundle$labels$label_id)
    if (length(missing_labels) > 0L && !quiet) {
      message(
        "dragmapr: drag_label_offsets.csv has no row for ",
        length(missing_labels), " label(s); using anchor position for: ",
        paste(utils::head(missing_labels, 8), collapse = ", "),
        if (length(missing_labels) > 8L) ", ..." else ""
      )
    }
    if (length(extra_labels) > 0L && !quiet) {
      message(
        "dragmapr: drag_label_offsets.csv contains label_id(s) not found in labels.csv: ",
        paste(utils::head(extra_labels, 8), collapse = ", "),
        if (length(extra_labels) > 8L) ", ..." else ""
      )
    }
  } else if (is.null(bundle$label_offsets) && !quiet) {
    message("dragmapr: project has no drag_label_offsets.csv; using anchor positions for labels.")
  }

  invisible(TRUE)
}

project_default <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (is.character(x) && !nzchar(x[1]))) y else x
}

project_smart_connector_labels <- function(labels, label_offsets) {
  labels <- as_drag_labels(labels)
  if (is.null(label_offsets) || nrow(labels) == 0L) {
    return(labels)
  }
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

#' Write a dragmapr project bundle
#'
#' Packages a source `sf` object together with region offsets, label offsets,
#' labels, a palette, and metadata into a ZIP file that can be reopened in
#' Spatial Studio or rendered with [render_dragmapr_project()]. This lets you
#' create and share reproducible project bundles entirely from R, without
#' opening the Shiny app.
#'
#' @param x An `sf` object in a projected CRS — the source geometry for the
#'   project.
#' @param region_col Column in `x` defining draggable groups.
#' @param file Output path for the ZIP file. Should end in `.zip`. When
#'   `NULL`, a temporary file is created and its path returned invisibly.
#' @param region_offsets Optional data frame with `region`, `dx_m`, and `dy_m`
#'   columns, or a path to such a CSV. When `NULL`, all regions are placed at
#'   their original positions.
#' @param label_offsets Optional data frame with `label_id`, `region`, `dx_m`,
#'   and `dy_m` columns, or a path to such a CSV.
#' @param labels Optional label table as returned by [make_region_labels()] or
#'   [as_drag_labels()].
#' @param region_palette Optional named character vector of fill colors (names
#'   are region values, values are hex strings).
#' @param label_col Column used for default label text. Defaults to
#'   `region_col`.
#' @param title Optional project title stored in metadata.
#' @param ... Additional named metadata fields written to `metadata.json`.
#'
#' @return Invisibly returns `file` (the path to the written ZIP).
#' @export
#' @seealso [read_dragmapr_project()] to read a project back into R;
#'   [render_dragmapr_project()] to render a project bundle directly.
#' @examples
#' if(interactive()){
#' poly <- sf::st_sf(
#'   region = c("A", "B"),
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(0,1e5),c(1e5,1e5),c(1e5,2e5),c(0,2e5),c(0,1e5)))),
#'     sf::st_polygon(list(rbind(c(0,0),c(1e5,0),c(1e5,1e5),c(0,1e5),c(0,0)))),
#'     crs = 3857
#'   )
#' )
#' offsets <- data.frame(region = c("A", "B"), dx_m = c(50000, -50000), dy_m = 0)
#' zip_path <- write_dragmapr_project(
#'   poly,
#'   region_col     = "region",
#'   region_offsets = offsets,
#'   title          = "Demo project",
#'   file           = tempfile(fileext = ".zip")
#' )
#' file.exists(zip_path)
#' }
write_dragmapr_project <- function(x,
                                   region_col,
                                   file           = NULL,
                                   region_offsets = NULL,
                                   label_offsets  = NULL,
                                   labels         = NULL,
                                   region_palette = NULL,
                                   label_col      = region_col,
                                   title          = NULL,
                                   ...) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!region_col %in% names(x)) {
    stop("region_col '", region_col, "' not found in `x`.", call. = FALSE)
  }
  if (is.null(file)) {
    file <- tempfile("dragmapr-project-", fileext = ".zip")
  }
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    stop("`file` must be a single non-empty file path.", call. = FALSE)
  }

  # Validate offsets if supplied
  if (!is.null(region_offsets)) {
    if (is.character(region_offsets) && length(region_offsets) == 1L) {
      region_offsets <- read_offsets(region_offsets)
    }
    region_offsets <- normalize_offsets(region_offsets, source = "`region_offsets`")
  }

  proj_dir <- tempfile("dragmapr_write_")
  dir.create(proj_dir, recursive = TRUE)
  on.exit(unlink(proj_dir, recursive = TRUE), add = TRUE)

  # source.gpkg
  sf::st_write(x, file.path(proj_dir, "source.gpkg"), quiet = TRUE, delete_dsn = TRUE)

  # drag_region_offsets.csv
  if (!is.null(region_offsets)) {
    utils::write.csv(region_offsets, file.path(proj_dir, "drag_region_offsets.csv"),
                     row.names = FALSE)
  }

  # drag_label_offsets.csv
  if (!is.null(label_offsets)) {
    utils::write.csv(label_offsets, file.path(proj_dir, "drag_label_offsets.csv"),
                     row.names = FALSE)
  }

  # labels.csv
  if (!is.null(labels)) {
    label_df <- tryCatch(as_drag_labels(labels), error = function(e) as.data.frame(labels))
    utils::write.csv(label_df, file.path(proj_dir, "labels.csv"), row.names = FALSE)
  }

  # palette.csv
  if (!is.null(region_palette)) {
    if (!is.character(region_palette) || is.null(names(region_palette))) {
      stop("`region_palette` must be a named character vector of hex colors.", call. = FALSE)
    }
    pal_df <- data.frame(region = names(region_palette), color = unname(region_palette),
                         stringsAsFactors = FALSE)
    utils::write.csv(pal_df, file.path(proj_dir, "palette.csv"), row.names = FALSE)
  }

  # metadata.json
  extra_meta <- list(...)
  metadata <- c(
    list(
      region_col  = region_col,
      label_col   = label_col,
      title       = if (is.null(title)) "" else as.character(title),
      created_by  = "write_dragmapr_project",
      dragmapr_version = as.character(utils::packageVersion("dragmapr"))
    ),
    extra_meta
  )
  jsonlite::write_json(metadata, file.path(proj_dir, "metadata.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  # Create ZIP — change into the project dir so zip paths are relative
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(proj_dir)
  files_to_zip <- list.files(proj_dir, full.names = FALSE)
  utils::zip(normalizePath(file, mustWork = FALSE), files_to_zip, flags = "-r9Xq")

  invisible(file)
}
