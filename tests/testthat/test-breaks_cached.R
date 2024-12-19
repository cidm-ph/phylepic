test_that("cached breaks cache breaks", {
  breaks <- scales::breaks_extended()
  cached <- breaks_cached(breaks)

  # wrapping doesn't change the underlying calculation...
  expect_equal(cached(c(0, 23)), breaks(c(0, 23)))
  # ... but it freezes the result, ignoring new limits
  expect_equal(cached(c(0, 50)), breaks(c(0, 23)))
})
