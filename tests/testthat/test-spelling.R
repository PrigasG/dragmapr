test_that("spelling", {
  skip_if_not_installed("spelling")
  spelling::spell_check_test(vignettes = TRUE, error = FALSE,
                             skip_on_cran = TRUE)
})
