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
