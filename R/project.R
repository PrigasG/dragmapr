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
#' \dontrun{
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

  defaults <- list(
    show_legend = project_default(metadata$show_legend, TRUE),
    legend_position = project_default(metadata$legend_position, "bottom"),
    labels = if (isFALSE(project_default(metadata$show_labels, TRUE))) FALSE else bundle$labels,
    label_marker_shape = project_default(metadata$label_marker_shape, "circle"),
    show_label_marker = !identical(project_default(metadata$label_marker_shape, "circle"), "none"),
    marker_size = project_default(metadata$marker_size, 3.2),
    connector_linewidth = project_default(metadata$connector_linewidth, 0.35),
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

  list(
    source = sf::st_read(source_file, quiet = TRUE),
    region_offsets = read_optional_project_csv(project_dir, "drag_region_offsets.csv"),
    label_offsets = read_optional_project_csv(project_dir, "drag_label_offsets.csv"),
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
