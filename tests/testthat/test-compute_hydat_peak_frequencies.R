context("Compute hydat peak frequencies")

test_that("creates a list with the proper objects", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressMessages(suppressWarnings(
    compute_hydat_peak_frequencies(station_number = "08NM116", 
                                         start_year = 1980, use_max = TRUE)))
  expect_true("list" %in% class(data) &
                all(c("Freq_Analysis_Data","Freq_Plot_Data","Freq_Plot","Freq_Fitting","Freq_Fitted_Quantiles") %in% names(data)))
})

test_that("creates proper object classes", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressMessages(suppressWarnings(
    compute_hydat_peak_frequencies(station_number = "08NM116", 
                                         start_year = 1980, use_max = TRUE)))
  expect_true("data.frame" %in% class(data$Freq_Analysis_Data))
  expect_true("data.frame" %in% class(data$Freq_Plot_Data))
  expect_true("gg" %in% class(data$Freq_Plot))
  expect_true("list" %in% class(data$Freq_Fitting))
  expect_true("data.frame" %in% class(data$Freq_Fitted_Quantiles))
})