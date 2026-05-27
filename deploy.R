# deploy.R — deploy the dragmapr Spatial Studio to shinyapps.io
#
# Run once interactively from the package root:
#   source("deploy.R")
#
# Prerequisites:
#   install.packages(c("rsconnect", "shiny", "shinyWidgets", "sf",
#                      "ggplot2", "jsonlite", "dragmapr"))
#
# First-time setup (do this once, then never again):
#   rsconnect::setAccountInfo(
#     name   = "prigasg",        # your shinyapps.io account name
#     token  = "<YOUR TOKEN>",   # from shinyapps.io > Account > Tokens
#     secret = "<YOUR SECRET>"
#   )

app_file <- system.file("examples", "shiny_spatial_studio.R", package = "dragmapr")
if (!nzchar(app_file)) stop("Build and install dragmapr first: devtools::install()")

rsconnect::deployApp(
  appDir     = dirname(app_file),
  appFiles   = basename(app_file),
  appName    = "dragmapr-spatial-studio",
  appTitle   = "dragmapr Spatial Studio",
  account    = "prigasg",
  forceUpdate = TRUE
)
