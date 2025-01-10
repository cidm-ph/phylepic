run_stat_bin_location <- function(data, ..., .x.scale = list()) {
  plot <-
    ggplot2::ggplot(data, aes(x, y)) +
    rlang::inject(ggplot2::scale_x_date(!!!.x.scale)) +
    stat_bin_location(...)

  ggplot2::layer_data(plot)
}

s2d <- function(x) as.numeric(as.Date(x))

test_that("overflow bins work", {
  computed <-
    run_stat_bin_location(
      data.frame(
        x = as.Date(c(
          "2020-02-01", "2020-12-31", # inside
          "2000-01-01", "2021-02-01", # oob
          "2020-01-01", "2020-07-01", "2021-01-01" # boundaries
        )),
        y = 1
      ),
      overflow = list(x = TRUE, y = FALSE),
      breaks = list(x = as.Date(c("2020-01-01", "2020-07-01", "2021-01-01")), y = 1:5),
      closed = list(x = "right", y = "right"),
    )

  expect_equal(computed$xmin, c(
    s2d("2020-01-01"), s2d("2020-07-01"),
    -Inf, s2d("2021-01-01"),
    s2d("2020-01-01"), s2d("2020-01-01"), s2d("2020-07-01")
  ))
  expect_equal(computed$xmax, c(
    s2d("2020-07-01"), s2d("2021-01-01"),
    s2d("2020-01-01"), Inf,
    s2d("2020-07-01"), s2d("2020-07-01"), s2d("2021-01-01")
  ))
})

test_that("overflow bins work with left-closed bins", {
  computed <-
    run_stat_bin_location(
      data.frame(
        x = as.Date(c(
          "2020-02-01", "2020-12-31", # inside
          "2000-01-01", "2021-02-01", # oob
          "2020-01-01", "2020-07-01", "2021-01-01" # boundaries
        )),
        y = 1
      ),
      overflow = list(x = TRUE, y = FALSE),
      breaks = list(x = as.Date(c("2020-01-01", "2020-07-01", "2021-01-01")), y = 1:5),
      closed = list(x = "left", y = "right"),
      .x.scale = list(oob = scales::oob_censor)
    )

  expect_equal(computed$xmin, c(
    s2d("2020-01-01"), s2d("2020-07-01"),
    -Inf, s2d("2021-01-01"),
    s2d("2020-01-01"), s2d("2020-07-01"), s2d("2020-07-01")
  ))
  expect_equal(computed$xmax, c(
    s2d("2020-07-01"), s2d("2021-01-01"),
    s2d("2020-01-01"), Inf,
    s2d("2020-07-01"), s2d("2021-01-01"), s2d("2021-01-01")
  ))
})
