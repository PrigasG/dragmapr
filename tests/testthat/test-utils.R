test_that("%||% returns fallback only for NULL", {
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(FALSE %||% "fallback", FALSE)
})
