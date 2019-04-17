context("Screen flow data")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- screen_flow_data(station_number = "08NM116")
  expect_true(is.data.frame(data) &
                ncol(data) == 22 &
                all(c("Year","n_days","n_Q","n_missing_Q","Minimum","Jan_missing_Q") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- screen_flow_data(station_number = c("08NM116","08HB048"))
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("data is filtered by years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- screen_flow_data(station_number = "08NM116",
                           start_year = 1981,
                           end_year = 2010)
  expect_identical(1981, min(data$Year))
  expect_identical(2010, max(data$Year))
})

test_that("data is summarized by water years properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- add_date_variables(station_number = "08NM116",
                                  water_year_start = 10)
  
  test_data <- dplyr::filter(flow_data, WaterYear == 1981)
  test_data <- dplyr::summarise(test_data,
                                Mean = mean(Value),
                                Median = median(Value),
                                Maximum = max(Value),
                                Minimum = min(Value))
  
  data <- screen_flow_data(data = flow_data,
                           start_year = 1981,
                           water_year_start = 10)
  data <- dplyr::filter(data, Year == 1981)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
})

test_that("missing dates are calculated properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- fill_missing_dates(station_number = "08NM116")
  flow_data <- add_date_variables(flow_data)
  test_data <- dplyr::summarise(dplyr::group_by(flow_data, WaterYear, MonthName),
                                sum = sum(is.na(Value)))
  test_data <- tidyr::spread(test_data, MonthName, sum)

  data <- screen_flow_data(station_number = "08NM116")
  
  expect_equal(test_data$Jan, data$Jan_missing_Q)
})

test_that("data is filtered by months properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- add_date_variables(station_number = "08NM116")
  
  test_data <- dplyr::filter(flow_data, 
                             WaterYear == 1981,
                             Month %in% 7:9)
  test_data <- dplyr::summarise(test_data,
                                Mean = mean(Value),
                                Median = median(Value),
                                Maximum = max(Value),
                                Minimum = min(Value))
  
  data <- screen_flow_data(data = flow_data,
                           start_year = 1981,
                           months = 7:9)
  data_test <- dplyr::filter(data, Year == 1981)
  data_test <- dplyr::select(data_test, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data_test)
  expect_true(ncol(data) == 13)
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- screen_flow_data(station_number = "08NM116",
                           transpose = TRUE)
  expect_true(all(c("n_days","n_Q","n_missing_Q","Minimum","Jan_missing_Q") %in% data$Statistic))
})

