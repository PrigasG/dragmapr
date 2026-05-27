# Run all bundled examples in a temporary directory.

example_dir <- system.file("examples", package = "dragmapr", mustWork = TRUE)
scripts <- c(
  "basic_draggable_map.R",
  "explodemap_hhs_labels.R",
  "label_nudging.R",
  "non_map_panels.R",
  "roundtrip_csv.R"
)

old <- setwd(tempdir())
on.exit(setwd(old), add = TRUE)

for (script in scripts) {
  message("Running ", script)
  source(file.path(example_dir, script), local = new.env(parent = globalenv()))
}

message("Wrote smoke outputs to ", normalizePath(tempdir(), winslash = "/"))
