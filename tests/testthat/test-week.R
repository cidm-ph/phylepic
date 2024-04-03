test_that("week start accepts different forms", {
  expect_equal(as_week_start(NULL), 5L)
  expect_equal(as_week_start("monday"), 5L)
  expect_equal(as_week_start("Monday"), 5L)
  expect_equal(as_week_start(5), 5L)
  expect_error(as_week_start("NOT_A_DAY"), "not a valid `week_start`")
})
