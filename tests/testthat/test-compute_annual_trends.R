context("Compute annual trends")

test_that("creates a list with the proper objects", {
  skip_on_cran()
  skip_on_travis()
  data <- compute_annual_trends(station_number = "08NM116", start_year = 1980,
                                zyp_method = "yuepilon")
  expect_true("list" %in% class(data) &
                all(c("Annual_Trends_Data","Annual_Trends_Results","Annual_Maximum") %in% names(data)))
})

test_that("creates two dataframes of data", {
  skip_on_cran()
  skip_on_travis()
  data <- compute_annual_trends(station_number = "08NM116", start_year = 1980,
                                zyp_method = "yuepilon",
                                include_plots = FALSE)
  expect_true("data.frame" %in% sapply(data, class))
})

test_that("creates plots after the two data frames", {
  skip_on_cran()
  skip_on_travis()
  data <- compute_annual_trends(station_number = "08NM116", start_year = 1980,
                                zyp_method = "yuepilon")
  expect_true("gg" %in% sapply(data[-(1:2)], class))
})

test_that("proper data is produced", {
  skip_on_cran()
  skip_on_travis()
  data <- compute_annual_trends(station_number = "08NM116", start_year = 1980,
                                zyp_method = "yuepilon",
                                include_plots = TRUE)
  data_data <- data$Annual_Trends_Data
  data_results <- data$Annual_Trends_Results
  
  annual_data <- calc_all_annual_stats(station_number = "08NM116",start_year = 1980,
                                       transpose = TRUE)
  expect_equal(data_data, annual_data)
  
  expect_true(all(c("Statistic","lbound","sig","min_year","mean") %in% names(data_results)))
  expect_equal(nrow(data_data), nrow(annual_data))
  
  plots <- data[-(1:2)]
  expect_equal(nrow(data_data), length(plots))
})