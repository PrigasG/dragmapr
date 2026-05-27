#' Build a small explodemap-style HHS example layout
#'
#' This copies the HHS region membership, names, colors, and published display
#' offsets used by the explodemap paper workflow into a lightweight fixture that
#' does not depend on the explodemap package or its generated paper outputs.
#'
#' @return A list with `states`, `labels`, `region_offsets`, `label_offsets`,
#'   `region_names`, and `region_colors`.
#' @export
#' @examples
#' hhs <- example_hhs_layout()
#' names(hhs)
example_hhs_layout <- function() {
  hhs_lookup <- data.frame(
    STUSPS = c(
      "ME", "NH", "VT", "MA", "RI", "CT",
      "NY", "NJ", "PR", "VI",
      "PA", "DE", "MD", "DC", "VA", "WV",
      "NC", "SC", "GA", "FL", "AL", "MS", "TN", "KY",
      "MN", "WI", "IL", "IN", "MI", "OH",
      "AR", "LA", "NM", "OK", "TX",
      "IA", "KS", "MO", "NE",
      "CO", "MT", "ND", "SD", "UT", "WY",
      "AZ", "CA", "HI", "NV", "MP", "AS", "GU",
      "AK", "ID", "OR", "WA"
    ),
    hhs_region = as.character(c(
      rep(1, 6), rep(2, 4), rep(3, 6), rep(4, 8),
      rep(5, 6), rep(6, 5), rep(7, 4), rep(8, 6),
      rep(9, 7), rep(10, 4)
    )),
    stringsAsFactors = FALSE
  )

  region_names <- stats::setNames(
    paste0(
      as.character(1:10),
      " - ",
      c(
        "Boston", "New York", "Philadelphia", "Atlanta", "Chicago",
        "Dallas", "Kansas City", "Denver", "San Francisco", "Seattle"
      )
    ),
    as.character(1:10)
  )
  region_colors <- stats::setNames(
    c(
      "#A89A83", "#C764A6", "#2B4970", "#DF514F", "#309396",
      "#70A255", "#F2BE42", "#8459A0", "#872722", "#3579B0"
    ),
    as.character(1:10)
  )

  region_offsets <- read_offsets(system.file(
    "extdata",
    "explodemap_hhs_region_offsets.csv",
    package = "dragmapr",
    mustWork = TRUE
  ))
  label_offsets <- read_label_offsets(system.file(
    "extdata",
    "explodemap_hhs_label_offsets.csv",
    package = "dragmapr",
    mustWork = TRUE
  ))

  width <- 85000
  height <- 65000
  gap <- 18000
  region_gap_x <- 520000
  region_gap_y <- 390000

  geoms <- vector("list", nrow(hhs_lookup))
  for (i in seq_len(nrow(hhs_lookup))) {
    region <- as.integer(hhs_lookup$hhs_region[i])
    local_i <- sum(hhs_lookup$hhs_region[seq_len(i)] == hhs_lookup$hhs_region[i]) - 1L
    region_col <- (region - 1L) %% 5L
    region_row <- (region - 1L) %/% 5L
    local_col <- local_i %% 3L
    local_row <- local_i %/% 3L
    x0 <- region_col * region_gap_x + local_col * (width + gap)
    y0 <- -region_row * region_gap_y - local_row * (height + gap)
    coords <- rbind(
      c(x0, y0),
      c(x0 + width, y0),
      c(x0 + width, y0 + height),
      c(x0, y0 + height),
      c(x0, y0)
    )
    geoms[[i]] <- sf::st_polygon(list(coords))
  }

  states <- sf::st_sf(
    STUSPS = hhs_lookup$STUSPS,
    NAME = hhs_lookup$STUSPS,
    hhs_region = hhs_lookup$hhs_region,
    hhs_region_name = unname(region_names[hhs_lookup$hhs_region]),
    geometry = sf::st_sfc(geoms, crs = 5070)
  )

  labels <- make_region_labels(states, region_col = "hhs_region", label_col = "hhs_region")
  labels$label <- labels$region

  list(
    states = states,
    labels = labels,
    region_offsets = region_offsets,
    label_offsets = label_offsets,
    region_names = region_names,
    region_colors = region_colors
  )
}

#' Build a non-map panel layout example
#'
#' This creates simple projected rectangles that behave like plot panels,
#' dashboard tiles, or diagram cards. It is useful for checking that dragmapr's
#' offset workflow is not tied to administrative map boundaries.
#'
#' @return A list with `panels`, `labels`, `region_offsets`, `label_offsets`,
#'   `region_names`, and `region_colors`.
#' @export
#' @examples
#' panels <- example_panel_layout()
#' names(panels)
example_panel_layout <- function() {
  panels <- data.frame(
    panel = c("Input", "Model", "Review", "Publish", "Archive"),
    group = c("A", "B", "C", "D", "E"),
    x = c(0, 180000, 360000, 540000, 720000),
    y = c(0, 90000, 0, 90000, 0),
    width = c(120000, 130000, 120000, 130000, 120000),
    height = c(70000, 70000, 70000, 70000, 70000),
    stringsAsFactors = FALSE
  )

  geoms <- lapply(seq_len(nrow(panels)), function(i) {
    x0 <- panels$x[i]
    y0 <- panels$y[i]
    x1 <- x0 + panels$width[i]
    y1 <- y0 + panels$height[i]
    sf::st_polygon(list(rbind(
      c(x0, y0),
      c(x1, y0),
      c(x1, y1),
      c(x0, y1),
      c(x0, y0)
    )))
  })

  sf_panels <- sf::st_sf(
    panel = panels$panel,
    group = panels$group,
    geometry = sf::st_sfc(geoms, crs = 3857)
  )
  labels <- make_region_labels(sf_panels, region_col = "group", label_col = "panel")

  region_offsets <- data.frame(
    region = panels$group,
    dx_m = c(0, -50000, 20000, -35000, 0),
    dy_m = c(0, -60000, 50000, -90000, 0),
    stringsAsFactors = FALSE
  )
  label_offsets <- data.frame(
    label_id = panels$group,
    region = panels$group,
    dx_m = c(0, 0, 0, 0, 0),
    dy_m = c(0, 20000, 0, 15000, 0),
    stringsAsFactors = FALSE
  )
  region_colors <- stats::setNames(
    c("#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756"),
    panels$group
  )
  region_names <- stats::setNames(panels$panel, panels$group)

  list(
    panels = sf_panels,
    labels = labels,
    region_offsets = region_offsets,
    label_offsets = label_offsets,
    region_names = region_names,
    region_colors = region_colors
  )
}
