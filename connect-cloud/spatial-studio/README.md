# dragmapr Spatial Studio on Posit Connect Cloud

This directory is the Git-backed Posit Connect Cloud entrypoint for the same
Spatial Studio app that is deployed to Hugging Face Spaces.

In Connect Cloud:

1. Publish from GitHub.
2. Select the `master` branch.
3. Use `connect-cloud/spatial-studio/app.R` as the primary file.
4. Keep `connect-cloud/spatial-studio/manifest.json` committed and refresh it
   with `rsconnect::writeManifest()` after dependency changes.

The app code itself remains in `inst/examples/shiny_spatial_studio.R`; this
wrapper exists so Connect Cloud can install the package and run the same app.
