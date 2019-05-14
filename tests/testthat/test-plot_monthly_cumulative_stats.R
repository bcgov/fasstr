context("Plot monthly cumulative stats")

test_that("a list of plots is created", {
  skip_on_cran()
  skip_on_travis()
  plots <- plot_monthly_cumulative_stats(station_number = "08NM116", start_year = 1980)
  expect_true("list" %in% class(plots) & "gg" %in% sapply(plots, class))
})

test_that("multiple plots are created with multiple groups", {
  skip_on_cran()
  skip_on_travis()
  plots <- plot_monthly_cumulative_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(plots) == 2)
})
