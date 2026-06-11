# Transition test app
#
# Mental model for dragmapr's local elastic hierarchy transitions, driven
# entirely by the exported package API:
#   transition_options()       - validated animation settings
#   build_elastic_transition() - anchors, frames, reset boundaries
#   layout_metrics()           - drift / stability readout
#
# Click "Expand to children" to play the bloom; the dotted local-return
# boundary appears around each expanded group. "Collapse to parents" plays
# the same frames in reverse, like pointer-down on the boundary in the D3
# prototype.

library(shiny)
library(sf)
library(ggplot2)
library(dragmapr)

# -----------------------------
# Demo geometry (projected CRS, like real dragmapr inputs)
# -----------------------------

make_rect <- function(xmin, xmax, ymin, ymax) {
  st_polygon(list(cbind(
    c(xmin, xmax, xmax, xmin, xmin),
    c(ymin, ymin, ymax, ymax, ymin)
  )))
}

make_parent_sf <- function() {
  st_sf(
    parent_id = c("A", "B", "C"),
    label = c("Parent A", "Parent B", "Parent C"),
    geometry = st_sfc(
      make_rect(0, 3, 0, 3),
      make_rect(4, 7, 0, 3),
      make_rect(8, 11, 0, 3),
      crs = 3857
    )
  )
}

make_child_sf <- function(parent_sf) {
  children <- lapply(seq_len(nrow(parent_sf)), function(i) {
    p <- parent_sf[i, ]
    bb <- st_bbox(p)
    parent_id <- p$parent_id

    child_boxes <- st_sfc(
      make_rect(bb[["xmin"]], bb[["xmin"]] + 1.35, bb[["ymin"]], bb[["ymin"]] + 1.35),
      make_rect(bb[["xmin"]] + 1.65, bb[["xmax"]], bb[["ymin"]], bb[["ymin"]] + 1.35),
      make_rect(bb[["xmin"]], bb[["xmin"]] + 1.35, bb[["ymin"]] + 1.65, bb[["ymax"]]),
      make_rect(bb[["xmin"]] + 1.65, bb[["xmax"]], bb[["ymin"]] + 1.65, bb[["ymax"]]),
      crs = 3857
    )

    st_sf(
      parent_id = parent_id,
      child_id = paste0(parent_id, "_", seq_len(4)),
      geometry = child_boxes
    )
  })
  do.call(rbind, children)
}

# Final (exploded) child layout: push children outward from the parent centre
explode_children <- function(child_sf, parent_sf, stretch = 1.35) {
  child_xy <- st_coordinates(st_centroid(st_geometry(child_sf)))
  parent_xy <- st_coordinates(st_centroid(st_geometry(parent_sf)))
  idx <- match(child_sf$parent_id, parent_sf$parent_id)

  vx <- child_xy[, 1] - parent_xy[idx, 1]
  vy <- child_xy[, 2] - parent_xy[idx, 2]
  norm <- pmax(sqrt(vx^2 + vy^2), 1e-9)

  geom <- st_geometry(child_sf)
  for (i in seq_along(geom)) {
    geom[i] <- geom[[i]] + c(vx[i] / norm[i], vy[i] / norm[i]) * stretch
  }
  st_geometry(child_sf) <- geom
  child_sf
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f8fb; }
      .control-card {
        background: white;
        border-radius: 14px;
        padding: 16px;
        box-shadow: 0 6px 24px rgba(0,0,0,0.08);
        margin-bottom: 14px;
      }
      .title { font-weight: 700; font-size: 22px; margin-bottom: 4px; }
      .subtitle { color: #666; margin-bottom: 16px; }
      .metrics { background: #f8fafc; border: 1px solid #e5e7eb;
                 border-radius: 10px; padding: 10px; line-height: 1.8; }
      .btn { border-radius: 10px; }
    "))
  ),

  fluidRow(
    column(
      3,
      div(
        class = "control-card",
        div(class = "title", "Elastic hierarchy test"),
        div(
          class = "subtitle",
          paste(
            "Children bloom from the parent centroid; the dotted",
            "local-return boundary belongs to the expanded group."
          )
        ),
        actionButton("expand", "Expand to children", class = "btn-primary"),
        actionButton("collapse", "Collapse to parents"),
        br(), br(),
        sliderInput("speed", "Frame interval (ms)",
                    min = 15, max = 80, value = 30, step = 5),
        sliderInput("frames", "Frames",
                    min = 20, max = 80, value = 45, step = 5),
        sliderInput("overshoot", "Overshoot",
                    min = 0, max = 2.4, value = 1.70158, step = 0.05),
        br(),
        div(class = "metrics", htmlOutput("metrics"))
      )
    ),
    column(9, plotOutput("map", height = "680px"))
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  parent_sf <- make_parent_sf()
  child_sf <- make_child_sf(parent_sf)
  child_final_sf <- explode_children(child_sf, parent_sf)

  # The whole engine lives in the package: anchors + frames + boundaries.
  transition <- reactive({
    build_elastic_transition(
      child_sf   = child_final_sf,
      parent_sf  = parent_sf,
      parent_col = "parent_id",
      options    = transition_options(
        n_frames  = input$frames,
        overshoot = input$overshoot
      )
    )
  })

  mode <- reactiveVal("parent")     # parent | child | expanding | collapsing
  frame_i <- reactiveVal(1L)

  observeEvent(input$expand, {
    frame_i(1L)
    mode("expanding")
  })

  observeEvent(input$collapse, {
    frame_i(input$frames)
    mode("collapsing")
  })

  observe({
    m <- mode()
    if (!m %in% c("expanding", "collapsing")) return()
    invalidateLater(input$speed, session)

    isolate({
      i <- frame_i()
      if (m == "expanding") {
        if (i >= input$frames) mode("child") else frame_i(i + 1L)
      } else {
        if (i <= 1L) mode("parent") else frame_i(i - 1L)
      }
    })
  })

  output$metrics <- renderUI({
    # The parent skeleton never moves during a local elastic insert, so
    # drift across the toggle should be zero and stability 100.
    m <- layout_metrics(parent_sf, parent_sf, id_col = "parent_id")
    HTML(paste0(
      "<strong>Skeleton drift:</strong> ", round(m$mean_drift, 2), " m<br/>",
      "<strong>Max drift:</strong> ", round(m$max_drift, 2), " m<br/>",
      "<strong>Stability:</strong> ", m$stability
    ))
  })

  output$map <- renderPlot({
    tr <- transition()
    current_mode <- mode()

    base <- ggplot() +
      coord_sf(expand = FALSE) +
      theme_void() +
      theme(legend.position = "none")

    if (current_mode == "parent") {
      return(
        base +
          geom_sf(data = parent_sf, aes(fill = parent_id),
                  color = "white", linewidth = 1.2) +
          geom_sf_text(data = parent_sf, aes(label = label),
                       size = 5, fontface = "bold")
      )
    }

    # Stable skeleton stays as a ghost behind the branch
    ghost <- geom_sf(data = parent_sf, fill = "grey85", color = "white",
                     linewidth = 1, alpha = 0.35)

    if (current_mode == "child") {
      return(
        base + ghost +
          geom_sf(data = child_final_sf, aes(fill = parent_id),
                  color = "white", linewidth = 1) +
          geom_sf_text(data = child_final_sf, aes(label = child_id),
                       size = 4, fontface = "bold") +
          geom_sf(data = tr$boundaries, fill = NA, color = "#7c3aed",
                  linewidth = 0.7, linetype = "dotted")
      )
    }

    frame_sf <- tr$frames[tr$frames$frame_id == frame_i(), ]
    base + ghost +
      geom_sf(data = frame_sf, aes(fill = parent_id),
              color = "white", linewidth = 1) +
      geom_sf_text(data = frame_sf, aes(label = child_id),
                   size = 4, fontface = "bold")
  })
}

shinyApp(ui, server)
