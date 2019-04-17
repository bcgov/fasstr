context("Compute frequency quantile")

test_that("creates a list with the proper objects", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressMessages(suppressWarnings(
    compute_frequency_quantile(station_number = "08NM116", start_year = 1980,
                                     roll_days = 7, return_period = 10)))
  expect_true(length(data) == 1 & is.numeric(data))
})

test_that("the quantile is correctly extracted from the results", {
  skip_on_cran()
  skip_on_travis()
  quant <- suppressMessages(suppressWarnings(
    compute_frequency_quantile(station_number = "08NM116", start_year = 1980,
                                     roll_days = 7, return_period = 10)))
  data <- suppressMessages(suppressWarnings(
    compute_annual_frequencies(station_number = "08NM116", start_year = 1980,
                                     roll_days = 7)))[[5]]
  expect_true(quant == as.numeric(data$`7-Day`[3]))
})



