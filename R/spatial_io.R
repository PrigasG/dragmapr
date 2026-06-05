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
  required <- c("name", "datapath")
  missing <- setdiff(required, names(upload))
  if (length(missing) > 0L) {
    stop(
      "`upload` must be a Shiny fileInput() value with column(s): ",
      paste(required, collapse = ", "), ". Missing: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  safe_names <- sanitize_upload_names(upload$name)
  upload_dir <- tempfile("dragmapr_upload_")
  dir.create(upload_dir, recursive = TRUE)
  for (i in seq_len(nrow(upload))) {
    ok <- file.copy(
      upload$datapath[i],
      file.path(upload_dir, safe_names[i]),
      overwrite = TRUE
    )
    if (!isTRUE(ok)) {
      stop(
        "Could not copy uploaded file '", safe_names[i], "'. ",
        "Please try uploading the file again.",
        call. = FALSE
      )
    }
  }
  zip_files <- file.path(
    upload_dir,
    safe_names[tolower(tools::file_ext(safe_names)) == "zip"]
  )
  if (length(zip_files) > 0L) {
    .unzip_spatial_archive(zip_files[1], upload_dir, source = "uploaded zip file")
  }
  spatial_file <- .detect_spatial_file(upload_dir)
  .read_spatial_file(spatial_file, source = "uploaded spatial file")
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
    .unzip_spatial_archive(dest, extract_dir, source = "downloaded zip file")
    spatial_file <- .detect_spatial_file(extract_dir)
  } else {
    spatial_file <- dest
  }
  .read_spatial_file(spatial_file, source = "downloaded spatial file")
}

#' Prepare an sf object for use with dragmapr
#'
#' Repairs invalid geometry, filters to polygon types, assigns a fallback CRS
#' when none is present, and reprojects geographic (longitude/latitude) data to
#' a projected CRS so metre offsets work correctly. CRS-less inputs trigger a
#' warning because the fallback is an assumption; pass `target_crs` explicitly
#' when your data uses a different projected CRS.
#'
#' @param x An `sf` object.
#' @param target_crs EPSG code for the projected CRS to use when the input is
#'   geographic.  Defaults to `3857` (Web Mercator).
#'
#' @return A projected `sf` object containing only polygon or multipolygon
#'   features with valid geometry.
#' @seealso [read_dragmapr_sf_upload()] and [read_dragmapr_sf_url()] to read
#'   spatial files before passing them to this function; [drag_map_prototype()]
#'   which requires a projected sf object.
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
    warning("dragmapr: no CRS found. Assuming EPSG:", target_crs, ".", call. = FALSE)
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
#' @param allowed_origin Origin allowed to send drag state messages. Use
#'   `"same-origin"` to accept the current Shiny app origin, `"*"` to accept
#'   any origin, or a specific origin such as `"https://example.com"`.
#' @param iframe_selector CSS selector used to find the drag-map helper iframe
#'   in the parent document.  Defaults to `"iframe"` (first iframe found), but
#'   a more specific selector such as `"#my-helper-frame"` or
#'   `"iframe.studio-helper-frame"` is recommended when the page may contain
#'   other iframes (e.g. Shiny download handlers).
#'
#' @return A character string of JavaScript ready for
#'   `tags$head(tags$script(HTML(dragmapr_iframe_bridge())))`.
#' @export
#' @examples
#' # In a Shiny UI:
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   tags$head(tags$script(HTML(
#'     dragmapr_iframe_bridge(iframe_selector = "#my-helper-frame")
#'   ))),
#'   uiOutput("helper")  # render tags$iframe(id = "my-helper-frame", ...)
#' )
#' }
dragmapr_iframe_bridge <- function(region_input = "region_csv",
                                   label_input  = "label_csv",
                                   slow_poll_ms = 2000L,
                                   fast_poll_ms = 500L,
                                   allowed_origin = "same-origin",
                                   iframe_selector = "iframe") {
  region_input    <- as.character(region_input[1L])
  label_input     <- as.character(label_input[1L])
  slow_poll_ms    <- as.integer(slow_poll_ms[1L])
  fast_poll_ms    <- as.integer(fast_poll_ms[1L])
  allowed_origin  <- as.character(allowed_origin[1L])
  iframe_selector <- as.character(iframe_selector[1L])
  if (!nzchar(allowed_origin)) {
    stop("`allowed_origin` must be a non-empty character string.", call. = FALSE)
  }
  if (!nzchar(iframe_selector)) {
    stop("`iframe_selector` must be a non-empty CSS selector string.", call. = FALSE)
  }

  sprintf(
    '
var _dragmaprBridgeReceived = false;
var _dragmaprAllowedOrigin = %s;
var _dragmaprIframeSelector = %s;
var _dragmaprBridgeTimer = null;
var _dragmaprBridgeActive = false;
function _dragmaprOriginAllowed(event) {
  if (_dragmaprAllowedOrigin === "*") return true;
  if (_dragmaprAllowedOrigin === "same-origin") {
    return event.origin === window.location.origin || event.origin === "null";
  }
  return event.origin === _dragmaprAllowedOrigin;
}
function _dragmaprTargetOrigin() {
  return _dragmaprAllowedOrigin === "same-origin" ? window.location.origin : _dragmaprAllowedOrigin;
}
window.addEventListener("message", function(event) {
  if (!event.data || event.data.type !== "dragmapr-offsets") return;
  if (!_dragmaprOriginAllowed(event)) return;
  _dragmaprBridgeReceived = true;
  Shiny.setInputValue(%s, event.data.regionCsv, {priority: "event"});
  Shiny.setInputValue(%s, event.data.labelCsv, {priority: "event"});
});
function _dragmaprBridgePoll() {
  if (!_dragmaprBridgeActive) return;
  var iframe = document.querySelector(_dragmaprIframeSelector);
  if (iframe && iframe.contentWindow) {
    iframe.contentWindow.postMessage({type: "dragmapr-request-state"}, _dragmaprTargetOrigin());
  }
  _dragmaprBridgeTimer = setTimeout(_dragmaprBridgePoll, _dragmaprBridgeReceived ? %d : %d);
}
function _dragmaprBridgeStop() {
  _dragmaprBridgeActive = false;
  if (_dragmaprBridgeTimer) {
    clearTimeout(_dragmaprBridgeTimer);
    _dragmaprBridgeTimer = null;
  }
}
$(document).on("shiny:connected", function() {
  if (_dragmaprBridgeActive) return;
  _dragmaprBridgeActive = true;
  _dragmaprBridgeTimer = setTimeout(_dragmaprBridgePoll, 100);
});
$(document).on("shiny:disconnected", _dragmaprBridgeStop);
window.addEventListener("beforeunload", _dragmaprBridgeStop);
',
    jsonlite::toJSON(allowed_origin,  auto_unbox = TRUE),
    jsonlite::toJSON(iframe_selector, auto_unbox = TRUE),
    jsonlite::toJSON(region_input,    auto_unbox = TRUE),
    jsonlite::toJSON(label_input,     auto_unbox = TRUE),
    slow_poll_ms,
    fast_poll_ms
  )
}

sanitize_upload_names <- function(names) {
  names <- basename(gsub("\\\\", "/", as.character(names)))
  bad <- is.na(names) | !nzchar(names) | names %in% c(".", "..")
  if (any(bad)) {
    stop("Uploaded files must have non-empty base names.", call. = FALSE)
  }
  make.unique(names, sep = "_")
}

# Internal: find the first readable spatial file in a directory tree.
.detect_spatial_file <- function(dir) {
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("(^|[/\\\\])(__MACOSX|\\.DS_Store)", files)]
  supported <- files[
    tolower(tools::file_ext(files)) %in% c("shp", "gpkg", "geojson", "json")
  ]
  if (length(supported) == 0L) {
    stop(
      "No supported spatial file found. Upload a .zip containing a shapefile, ",
      "or provide a .shp, .gpkg, .geojson, or .json file.",
      call. = FALSE
    )
  }
  shp <- supported[tolower(tools::file_ext(supported)) == "shp"]
  if (length(shp) > 0L) return(shp[1L])
  supported[1L]
}

.unzip_spatial_archive <- function(zip_file, exdir, source) {
  listing <- tryCatch(
    utils::unzip(zip_file, list = TRUE),
    error = function(e) {
      stop(
        "Could not inspect the ", source, ". ",
        "Check that it is a valid .zip archive and try again.",
        call. = FALSE
      )
    }
  )
  names <- gsub("\\\\", "/", listing$Name)
  unsafe <- grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", names)
  if (any(unsafe)) {
    stop(
      "The ", source, " contains unsafe paths. ",
      "Please re-create the archive with ordinary relative file names.",
      call. = FALSE
    )
  }
  tryCatch(
    utils::unzip(zip_file, exdir = exdir),
    error = function(e) {
      stop(
        "Could not unzip the ", source, ". ",
        "Check that it is a valid .zip archive and try again.",
        call. = FALSE
      )
    },
    warning = function(w) {
      stop(
        "Could not unzip the ", source, ". ",
        "Check that it is a valid .zip archive and try again.",
        call. = FALSE
      )
    }
  )
}

.read_spatial_file <- function(file, source) {
  x <- tryCatch(
    sf::st_read(file, quiet = TRUE),
    error = function(e) {
      stop(
        "Could not read the ", source, " as spatial data. ",
        "Supported formats are zipped shapefiles, GeoPackage, GeoJSON, and JSON. ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!inherits(x, "sf")) {
    stop("The ", source, " did not produce an sf object.", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    stop("The ", source, " contains no features.", call. = FALSE)
  }
  x
}
