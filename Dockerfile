# Dockerfile for the dragmapr Spatial Studio on HuggingFace Spaces.
#
# Build and run locally:
#   docker build -t dragmapr-studio .
#   docker run --rm -p 7860:7860 dragmapr-studio
#   open http://localhost:7860
#
# HuggingFace Spaces picks this file up automatically when the Space SDK
# is set to "Docker".  Push the package repo to the Space remote and the
# platform builds and serves the container.
#
#   git remote add space https://huggingface.co/spaces/Prigas89/dragmapr-spatial-studio
#   git push space main

FROM rocker/geospatial:4.4.2

# ---- System-level Shiny dependencies ----------------------------------------
# rocker/geospatial already ships R, sf, GDAL, GEOS, PROJ, and tidyverse.
# Add the Shiny stack and the optional widgets used by Spatial Studio.
RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny \
    shinyWidgets \
    && R --no-save -e "install.packages('glasstabs', repos='https://cloud.r-project.org')" \
    && rm -rf /tmp/downloaded_packages /var/lib/apt/lists/*

# ---- Install dragmapr from the repo source ----------------------------------
# The full package source is copied in so the installed package matches
# exactly what is in this commit, including inst/examples/shiny_spatial_studio.R.
WORKDIR /pkg
COPY . /pkg

RUN R --no-save -e " \
  install.packages('remotes', repos='https://cloud.r-project.org', quiet=TRUE); \
  remotes::install_local('/pkg', upgrade='never', dependencies=FALSE, quiet=TRUE) \
"

# ---- Runtime ----------------------------------------------------------------
EXPOSE 7860

# shiny.maxRequestSize is also set inside the app, but we set it here too
# so it applies before the app script is sourced.
ENV SHINY_MAX_REQUEST_SIZE=104857600

CMD ["R", "--no-save", "-e", \
  "options(shiny.maxRequestSize=104857600); \
   shiny::runApp( \
     system.file('examples','shiny_spatial_studio.R',package='dragmapr'), \
     host='0.0.0.0', port=7860, launch.browser=FALSE \
   )"]
