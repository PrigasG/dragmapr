#' Read an sf object from a Shiny file upload
#'
#' Handles the data frame returned by [shiny::fileInput()], including
#' multi-file uploads of shapefile sidecars (`.shp`, `.dbf`, `.shx`, `.prj`)
#' and zipped archives containing any supported format.  When `upload` is
#' `NULL` or empty the function returns `NULL` so callers can fall back to
#' demo data.
#'
#' @param upload The value of `input$<id>` from a [shiny::fileInput()] widget.
#'   Each row is one uploaded file with `name` and `datapath` columns.  When
#'   `NULL` or a zero-row data frame, returns `NULL`.
#'
#' @return An `sf` object, or `NULL` if `upload` is `NULL` or empty.
#' @export
#' @examples
#' # Typical Shiny server usage:
#' \dontrun{
#' observeEvent(input$spatial_upload, {
#'   x <- read_dragmapr_sf_upload(input$spatial_upload)
#'   if (!is.null(x)) {
#'     state$source <- x
#'   }
#' })
#' }
read_dragmapr_sf_upload <- function(upload) {
  if (is.null(upload) || nrow(upload) == 0L) {
    return(NULL)
  }
  upload_dir <- tempfile("dragmapr_upload_")
  dir.create(upload_dir, recursive = TRUE)
  for (i in seq_len(nrow(upload))) {
    file.copy(upload$datapath[i],
              file.path(upload_dir, upload$name[i]),
              overwrite = TRUE)
  }
  zip_files <- file.path(
    upload_dir,
    upload$name[tolower(tools::file_ext(upload$name)) == "zip"]
  )
  if (length(zip_files) > 0L) {
    utils::unzip(zip_files[1], exdir = upload_dir)
  }
  spatial_file <- .detect_spatial_file(upload_dir)
  x <- sf::st_read(spatial_file, quiet = TRUE)
  if (!inherits(x, "sf")) {
    stop("The uploaded file could not be read as an sf object.", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    stop("The uploaded spatial file contains no features.", call. = FALSE)
  }
  x
}

#' Download and read an sf object from a URL
#'
#' Downloads a spatial file from `url` into a temporary directory and reads it
#' with [sf::st_read()].  Supported direct formats are `.geojson`, `.json`,
#' and `.gpkg`.  A `.zip` URL is extracted first and the first supported file
#' inside is read.  For ambiguous extensions the function assumes a zip archive.
#'
#' @param url A non-empty character string pointing to a downloadable spatial
#'   file.
#' @param timeout Download timeout in seconds.  Defaults to `60`.
#'
#' @return An `sf` object.
#' @export
#' @examples
#' \dontrun{
#' x <- read_dragmapr_sf_url("https://example.com/regions.geojson")
#' }
read_dragmapr_sf_url <- function(url, timeout = 60) {
  if (!is.character(url) || length(url) != 1L || !nzchar(trimws(url))) {
    stop("`url` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      !is.finite(timeout) || timeout <= 0) {
    stop("`timeout` must be a positive number.", call. = FALSE)
  }
  url <- trimws(url)
  url_path <- sub("[?#].*$", "", url)
  ext <- tolower(tools::file_ext(url_path))
  if (!ext %in% c("zip", "geojson", "json", "gpkg", "shp")) {
    ext <- "zip"
  }
  dest <- tempfile(fileext = paste0(".", ext))
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)
  tryCatch(
    utils::download.file(url, dest, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop("Could not download '", url, "': ", conditionMessage(e), call. = FALSE)
    }
  )
  if (ext == "zip") {
    extract_dir <- tempfile("dragmapr_url_")
    dir.create(extract_dir, recursive = TRUE)
    utils::unzip(dest, exdir = extract_dir)
    spatial_file <- .detect_spatial_file(extract_dir)
  } else {
    spatial_file <- dest
  }
  x <- sf::st_read(spatial_file, quiet = TRUE)
  if (!inherits(x, "sf")) {
    stop("The downloaded file could not be read as an sf object.", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    stop("The downloaded spatial file contains no features.", call. = FALSE)
  }
  x
}

#' Prepare an sf object for use with dragmapr
#'
#' Repairs invalid geometry, filters to polygon types, assigns a fallback CRS
#' when none is present, and reprojects geographic (longitude/latitude) data to
#' a projected CRS so metre offsets work correctly.
#'
#' @param x An `sf` object.
#' @param target_crs EPSG code for the projected CRS to use when the input is
#'   geographic.  Defaults to `3857` (Web Mercator).
#'
#' @return A projected `sf` object containing only polygon or multipolygon
#'   features with valid geometry.
#' @export
#' @examples
#' poly <- sf::st_sf(
#'   region = "A",
#'   geometry = sf::st_sfc(
#'     sf::st_polygon(list(rbind(c(-1,0),c(1,0),c(1,1),c(-1,1),c(-1,0)))),
#'     crs = 4326
#'   )
#' )
#' out <- prepare_dragmapr_sf(poly)
#' sf::st_is_longlat(out)  # FALSE
prepare_dragmapr_sf <- function(x, target_crs = 3857) {
  if (!inherits(x, "sf")) {
    stop("`x` must be an sf object.", call. = FALSE)
  }
  if (!is.numeric(target_crs) || length(target_crs) != 1L || !is.finite(target_crs)) {
    stop("`target_crs` must be a single numeric EPSG code.", call. = FALSE)
  }
  x <- sf::st_make_valid(x)
  geom_type <- as.character(sf::st_geometry_type(x))
  keep <- grepl("POLYGON|MULTIPOLYGON", geom_type)
  if (!any(keep)) {
    stop(
      "dragmapr expects polygon or multipolygon geometry. Found: ",
      paste(unique(geom_type), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(!keep)) {
    message("dragmapr: dropping ", sum(!keep), " non-polygon feature(s).")
    x <- x[keep, , drop = FALSE]
  }
  if (is.na(sf::st_crs(x))) {
    message("dragmapr: no CRS found. Assuming EPSG:", target_crs, ".")
    sf::st_crs(x) <- target_crs
  }
  if (sf::st_is_longlat(x)) {
    message("dragmapr: transforming from geographic to EPSG:", target_crs,
            " for metre offsets.")
    x <- sf::st_transform(x, target_crs)
  }
  x
}

#' Build the Shiny iframe bridge JavaScript for a draggable helper
#'
#' Returns the JavaScript needed to pass region and label offset state from the
#' draggable helper iframe back into Shiny inputs via `postMessage`, and to
#' poll the iframe until state arrives.  The string is suitable for wrapping
#' in `tags$head(tags$script(HTML(...)))`.
#'
#' @param region_input Name of the Shiny input that receives the region-offset
#'   CSV text.  Defaults to `"region_csv"`.
#' @param label_input Name of the Shiny input that receives the label-offset
#'   CSV text.  Defaults to `"label_csv"`.
#' @param slow_poll_ms Polling interval in milliseconds once initial state has
#'   been received.  Defaults to `2000`.
#' @param fast_poll_ms Polling interval in milliseconds before initial state
#'   arrives.  Defaults to `500`.
#'
#' @return A character string of JavaScript ready for
#'   `tags$head(tags$script(HTML(dragmapr_iframe_bridge())))`.
#' @export
#' @examples
#' # In a Shiny UI:
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   tags$head(tags$script(HTML(dragmapr_iframe_bridge()))),
#'   uiOutput("helper")
#' )
#' }
dragmapr_iframe_bridge <- function(region_input = "region_csv",
                                   label_input  = "label_csv",
                                   slow_poll_ms = 2000L,
                                   fast_poll_ms = 500L) {
  region_input <- as.character(region_input[1L])
  label_input  <- as.character(label_input[1L])
  slow_poll_ms <- as.integer(slow_poll_ms[1L])
  fast_poll_ms <- as.integer(fast_poll_ms[1L])

  sprintf(
    '
var _dragmaprBridgeReceived = false;
window.addEventListener("message", function(event) {
  if (!event.data || event.data.type !== "dragmapr-offsets") return;
  _dragmaprBridgeReceived = true;
  Shiny.setInputValue(%s, event.data.regionCsv, {priority: "event"});
  Shiny.setInputValue(%s, event.data.labelCsv, {priority: "event"});
});
function _dragmaprBridgePoll() {
  var iframe = document.querySelector("iframe");
  if (iframe && iframe.contentWindow) {
    iframe.contentWindow.postMessage({type: "dragmapr-request-state"}, "*");
  }
  setTimeout(_dragmaprBridgePoll, _dragmaprBridgeReceived ? %d : %d);
}
$(document).on("shiny:connected", function() {
  setTimeout(_dragmaprBridgePoll, 100);
});
',
    jsonlite::toJSON(region_input, auto_unbox = TRUE),
    jsonlite::toJSON(label_input,  auto_unbox = TRUE),
    slow_poll_ms,
    fast_poll_ms
  )
}

# Internal: find the first readable spatial file in a directory tree.
.detect_spatial_file <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  supported <- files[
    tolower(tools::file_ext(files)) %in% c("shp", "gpkg", "geojson", "json")
  ]
  if (length(supported) == 0L) {
    stop(
      "No .shp, .gpkg, .geojson, or .json file found in '", dir, "'.",
      call. = FALSE
    )
  }
  shp <- supported[tolower(tools::file_ext(supported)) == "shp"]
  if (length(shp) > 0L) return(shp[1L])
  supported[1L]
}
