# Local elastic hierarchy transitions

rect <- function(x0, x1, y0, y1) {
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0))))
}

make_parents <- function() {
  sf::st_sf(
    county = c("A", "B"),
    geometry = sf::st_sfc(rect(0, 3, 0, 3), rect(4, 7, 0, 3), crs = 3857)
  )
}

make_children <- function() {
  sf::st_sf(
    county = c("A", "A", "B", "B"),
    mun    = c("A1", "A2", "B1", "B2"),
    geometry = sf::st_sfc(
      rect(-1, 1, 0, 3), rect(2, 4, 0, 3),
      rect(3, 5, 0, 3), rect(6, 8, 0, 3),
      crs = 3857
    )
  )
}

# ---- transition_options ------------------------------------------------------

test_that("transition_options returns validated defaults", {
  opts <- transition_options()
  expect_s3_class(opts, "dragmapr_transition_options")
  expect_identical(opts$mode, "local_elastic")
  expect_equal(opts$duration_ms, 550)
  expect_true(opts$preserve_skeleton)
  expect_false(opts$global_relayout)
  expect_true(opts$reset_boundary)
  expect_identical(opts$n_frames, 30L)
})

test_that("transition_options rejects global relayout and bad input", {
  expect_error(
    transition_options(global_relayout = TRUE),
    "mental-map stability"
  )
  expect_error(
    transition_options(preserve_skeleton = FALSE),
    "preserve_skeleton"
  )
  expect_error(transition_options(duration_ms = -5), "duration_ms")
  expect_error(transition_options(duration_ms = c(300, 900)), "duration_ms")
  expect_error(transition_options(n_frames = 1), "n_frames")
  expect_error(transition_options(n_frames = 10.5), "whole number")
  expect_error(transition_options(max_stretch = 0), "max_stretch")
  expect_error(transition_options(boundary_padding = 0), "boundary_padding")
  expect_error(transition_options(show_ghost = "yes"), "show_ghost")
  expect_error(transition_options(mode = "dagre"))
})

# ---- build_elastic_transition ------------------------------------------------

test_that("build_elastic_transition returns all components", {
  tr <- build_elastic_transition(
    make_children(), make_parents(),
    parent_col = "county",
    options    = transition_options(n_frames = 5)
  )

  expect_s3_class(tr, "dragmapr_transition")
  expect_named(tr, c("anchored", "frames", "boundaries", "options"))

  anchored <- tr$anchored
  expect_s3_class(anchored, "sf")
  expect_true(all(c(
    "anchor_x", "anchor_y", "final_x", "final_y",
    "move_dist", "move_ratio", "stretch_strength", "duration_ms"
  ) %in% names(anchored)))
  expect_equal(nrow(anchored), 4L)
  expect_true(all(anchored$duration_ms == 550))
  expect_true(all(anchored$stretch_strength <= 0.35))

  # Children of county A anchor at A's centroid (1.5, 1.5)
  expect_equal(anchored$anchor_x[anchored$county == "A"], c(1.5, 1.5))
  expect_equal(anchored$anchor_y[anchored$county == "A"], c(1.5, 1.5))
})

test_that("frames start at the parent anchor and end at final geometry", {
  children <- make_children()
  tr <- build_elastic_transition(
    children, make_parents(),
    parent_col = "county",
    options    = transition_options(n_frames = 4)
  )

  frames <- tr$frames
  expect_s3_class(frames, "sf")
  expect_equal(nrow(frames), 4L * nrow(children))
  expect_equal(sort(unique(frames$frame_id)), 1:4)

  # First frame: centroids sit at the parent anchors
  first <- frames[frames$frame_id == 1L, ]
  first_xy <- suppressWarnings(
    sf::st_coordinates(sf::st_centroid(sf::st_geometry(first)))
  )
  expect_equal(unname(first_xy[, 1]), first$anchor_x, tolerance = 1e-8)
  expect_equal(unname(first_xy[, 2]), first$anchor_y, tolerance = 1e-8)

  # Last frame: geometry equals the supplied (final) child geometry
  last <- frames[frames$frame_id == 4L, ]
  expect_equal(last$frame_progress, rep(1, nrow(children)))
  for (j in seq_len(nrow(children))) {
    expect_equal(
      sf::st_coordinates(sf::st_geometry(last)[[j]]),
      sf::st_coordinates(sf::st_geometry(children)[[j]]),
      tolerance = 1e-8
    )
  }
})

test_that("unmatched parents warn and fall back to the child centroid", {
  children <- make_children()
  children$county[4] <- "Z"

  expect_warning(
    tr <- build_elastic_transition(
      children, make_parents(),
      parent_col = "county",
      options    = transition_options(n_frames = 3)
    ),
    "No parent found"
  )
  orphan <- tr$anchored[tr$anchored$county == "Z", ]
  expect_equal(orphan$anchor_x, orphan$final_x)
  expect_equal(orphan$anchor_y, orphan$final_y)
})

test_that("build_elastic_transition validates inputs with clear messages", {
  children <- make_children()
  parents  <- make_parents()

  expect_error(
    build_elastic_transition(data.frame(a = 1), parents, parent_col = "county"),
    "must be an sf object"
  )
  expect_error(
    build_elastic_transition(children, parents, parent_col = "nope"),
    "Available columns"
  )
  expect_error(
    build_elastic_transition(children, parents, parent_col = "county",
                             options = list(n_frames = 5)),
    "transition_options"
  )

  parents_4326 <- sf::st_transform(parents, 4326)
  expect_error(
    build_elastic_transition(children, parents_4326, parent_col = "county"),
    "coordinate reference"
  )
})

test_that("reset_boundary = FALSE skips boundary generation", {
  tr <- build_elastic_transition(
    make_children(), make_parents(),
    parent_col = "county",
    options    = transition_options(n_frames = 3, reset_boundary = FALSE)
  )
  expect_null(tr$boundaries)
})

# ---- make_group_boundaries ---------------------------------------------------

test_that("make_group_boundaries returns one padded rectangle per group", {
  children <- make_children()
  bounds <- make_group_boundaries(children, group_col = "county", padding = 0.5)

  expect_s3_class(bounds, "sf")
  expect_equal(nrow(bounds), 2L)
  expect_setequal(bounds$group_id, c("A", "B"))

  a <- bounds[bounds$group_id == "A", ]
  expect_equal(a$xmin, -1.5)
  expect_equal(a$xmax, 4.5)
  expect_equal(a$ymin, -0.5)
  expect_equal(a$ymax, 3.5)

  # Boundary contains its children
  a_children <- children[children$county == "A", ]
  expect_true(all(sf::st_covers(
    sf::st_geometry(a),
    sf::st_geometry(a_children),
    sparse = FALSE
  )))
})

test_that("make_group_boundaries defaults padding to 2.5% of the diagonal", {
  children <- make_children()
  bb <- sf::st_bbox(children)
  diag <- sqrt(
    (bb[["xmax"]] - bb[["xmin"]])^2 + (bb[["ymax"]] - bb[["ymin"]])^2
  )
  bounds <- make_group_boundaries(children, group_col = "county")
  a <- bounds[bounds$group_id == "A", ]
  expect_equal(a$xmin, -1 - diag * 0.025)
})

test_that("make_group_boundaries validates input", {
  expect_error(
    make_group_boundaries(data.frame(a = 1), group_col = "a"),
    "must be an sf object"
  )
  expect_error(
    make_group_boundaries(make_children(), group_col = "nope"),
    "Available columns"
  )
  expect_error(
    make_group_boundaries(make_children(), group_col = "county", padding = -1),
    "padding"
  )
})

# ---- layout_metrics ----------------------------------------------------------

test_that("layout_metrics reports zero drift for identical layouts", {
  children <- make_children()
  m <- layout_metrics(children, children, id_col = "mun")
  expect_equal(m$mean_drift, 0)
  expect_equal(m$max_drift, 0)
  expect_equal(m$stability, 100)
  expect_equal(m$n_matched, 4L)
})

test_that("layout_metrics measures a known shift", {
  children <- make_children()
  shifted <- children
  geom <- sf::st_geometry(shifted)
  geom[1] <- geom[[1]] + c(3, 4)  # distance 5
  sf::st_geometry(shifted) <- geom

  m <- layout_metrics(children, shifted, id_col = "mun")
  expect_equal(m$max_drift, 5, tolerance = 1e-8)
  expect_equal(m$mean_drift, 5 / 4, tolerance = 1e-8)
  expect_lt(m$stability, 100)
})

test_that("layout_metrics warns about regions missing from after", {
  children <- make_children()
  expect_warning(
    m <- layout_metrics(children, children[1:3, ], id_col = "mun"),
    "not present in `after`"
  )
  expect_equal(m$n_matched, 3L)

  expect_error(
    suppressWarnings(
      layout_metrics(children, make_parents(), id_col = "county")
    ),
    NA
  )
  expect_error(
    suppressWarnings({
      other <- children
      other$mun <- paste0("X", seq_len(4))
      layout_metrics(children, other, id_col = "mun")
    }),
    "No regions"
  )
})
