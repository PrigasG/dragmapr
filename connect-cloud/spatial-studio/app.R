# Posit Connect Cloud entrypoint for dragmapr Spatial Studio.
#
# Connect Cloud deploys from GitHub using a primary app.R plus manifest.json.
# This wrapper keeps the deployed app on the same installed package code that
# powers the Hugging Face Space and the package example.

options(
  shiny.maxRequestSize = max(
    getOption("shiny.maxRequestSize", 5 * 1024^2),
    100 * 1024^2
  )
)

# Keep these explicit so rsconnect::writeManifest() records every package used
# by the sourced Spatial Studio app.
library(shiny)
library(sf)
library(ggplot2)
library(jsonlite)
library(htmltools)
library(htmlwidgets)
library(shinyWidgets)
library(glasstabs)
library(dragmapr)

source(
  system.file("examples", "shiny_spatial_studio.R", package = "dragmapr", mustWork = TRUE),
  local = globalenv()
)$value
