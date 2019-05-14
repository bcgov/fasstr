context("Plot annual stats")

test_that("a list of plots is created", {
  skip_on_cran()
  skip_on_travis()
  plots <- plot_annual_stats(station_number = "08NM116",
                             start_year = 1981,
                             end_year = 2010,
                             water_year_start = 10)
  expect_true("list" %in% class(plots) & "gg" %in% sapply(plots, class))
})

test_that("multiple plots are created with multiple groups", {
  skip_on_cran()
  skip_on_travis()
  plots <- plot_annual_stats(station_number = c("08NM116","08NM242"),
                             ignore_missing = TRUE)
  expect_true(length(plots) == 2)
})
