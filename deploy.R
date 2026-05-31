# deploy.R — push the dragmapr Spatial Studio to HuggingFace Spaces
#
# This file is excluded from the CRAN build (see .Rbuildignore).
# Run the shell commands below from the package root, not from R.
#
# ── One-time setup ────────────────────────────────────────────────────────────
#
#  1. Create the Space on huggingface.co:
#       https://huggingface.co/new-space
#       Owner  : prigasg
#       Name   : dragmapr-spatial-studio
#       SDK    : Docker          <- important
#       License: MIT
#
#  2. Add the Space as a second git remote:
#       git remote add space https://huggingface.co/spaces/Prigas89/dragmapr-spatial-studio
#
#  3. (First push only) authenticate — either log in with the HF CLI:
#       pip install huggingface_hub
#       huggingface-cli login
#     or use a personal access token as the git password when prompted.
#
# ── Deploy / update ───────────────────────────────────────────────────────────
#
#  Push the current branch to the Space remote.  HuggingFace will build the
#  Dockerfile automatically and restart the container.
#
#       git push space main
#
#  Force-push after a rebase:
#       git push space main --force
#
# ── Live URLs ─────────────────────────────────────────────────────────────────
#
#  Space page : https://huggingface.co/spaces/Prigas89/dragmapr-spatial-studio
#  Running app: https://prigasg-dragmapr-spatial-studio.hf.space
#
# ── Local Docker test (optional) ─────────────────────────────────────────────
#
#  Build and run locally before pushing:
#       docker build -t dragmapr-studio .
#       docker run --rm -p 7860:7860 dragmapr-studio
#       # then open http://localhost:7860
#
# ── Notes ─────────────────────────────────────────────────────────────────────
#
#  * rocker/geospatial:4.4.2 ships GDAL, GEOS, PROJ, and sf pre-built, so the
#    Docker build should complete in ~3-5 minutes on HuggingFace's build runners.
#  * The container installs dragmapr from the repo source at build time, so
#    every push reflects the latest commit exactly.
#  * HuggingFace free CPU Spaces have 2 vCPUs and 16 GB RAM — enough for
#    reasonably large shapefiles without hitting memory limits.
#  * The Space sleeps after ~15 minutes of inactivity (free tier) and wakes on
#    the next request within ~10 seconds.
#
# ── README.md frontmatter ─────────────────────────────────────────────────────
#
#  HuggingFace Spaces requires a YAML block at the very top of README.md.
#  It can be accidentally lost when editing the file.  Before every push to the
#  space remote, run source("deploy.R") from the package root to check and
#  restore the block automatically, then commit if README.md changed:
#
#       source("deploy.R")
#       git add README.md
#       git commit -m "Restore HF YAML" --allow-empty
#       git push space master:main
#
# ── Helper function ───────────────────────────────────────────────────────────

#' Ensure README.md begins with the HuggingFace Space YAML frontmatter.
#'
#' Prepends the required block when it is missing; does nothing when it is
#' already present.  Call this from the package root before every HF push.
ensure_hf_readme <- function(readme = "README.md") {
  hf_yaml <- c(
    "---",
    "title: dragmapr Spatial Studio",
    "emoji: \U0001F5FA\UFE0F",
    "colorFrom: blue",
    "colorTo: green",
    "sdk: docker",
    "pinned: false",
    "---",
    ""
  )
  lines <- readLines(readme, warn = FALSE)
  if (length(lines) > 0L && trimws(lines[1L]) == "---") {
    message("HuggingFace YAML frontmatter is already present in ", readme, ".")
    return(invisible(NULL))
  }
  writeLines(c(hf_yaml, lines), readme)
  message("HuggingFace YAML frontmatter added to ", readme, ".")
  message("Run: git add README.md && git commit -m 'Restore HF YAML'")
}

ensure_hf_readme()
