context("Calc annual stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                all(c("Year","Mean","Median","Maximum","Minimum","P10","P90") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = "08NM116", start_year = 1980,
                            percentiles = c(25,75))
  expect_true(all(c("P25","P75") %in% colnames(data)))
})

test_that("produces NA if there is missing data and warning is produced", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressWarnings(calc_annual_stats(station_number = "08NM116",
                            ignore_missing = FALSE))
  expect_true(any(is.na(data)))
  expect_warning(calc_annual_stats(station_number = "08NM116",
                                   ignore_missing = FALSE))
})

test_that("ignore_missing calculates all data", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = "08NM116",
                            ignore_missing = TRUE)
  expect_true(any(!is.na(data)))
})

test_that("data is filtered by years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = "08NM116",
                            start_year = 1981,
                            end_year = 2010,
                            exclude_years = c(1991,1993:1995))
  expect_identical(1981, min(data$Year))
  expect_identical(2010, max(data$Year))
  
  data_exclude <- dplyr::filter(data, Year %in% c(1991,1993:1995))
  expect_true(all(is.na(data_exclude[,3:8])))  
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
  
  data <- calc_annual_stats(data = flow_data,
                            start_year = 1981, end_year = 1990,
                            water_year_start = 10)
  data <- dplyr::filter(data, Year == 1981)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
})

test_that("rolling days are applied properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- add_rolling_means(flow_data,
                                 roll_days = 7)
  
  test_data <- dplyr::filter(flow_data, WaterYear == 1981)
  test_data <- dplyr::summarise(test_data,
                                Mean = mean(Q7Day),
                                Median = median(Q7Day),
                                Maximum = max(Q7Day),
                                Minimum = min(Q7Day))
  
  data <- calc_annual_stats(data = flow_data,
                            start_year = 1981,
                            roll_days = 7)
  data <- dplyr::filter(data, Year == 1981)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
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
  
  data <- calc_annual_stats(data = flow_data,
                            start_year = 1981,
                            months = 7:9)
  data <- dplyr::filter(data, Year == 1981)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_stats(station_number = "08NM116", start_year = 1980,
                            transpose = TRUE)
  expect_true(all(c("Mean","Median","Maximum","Minimum","P10","P90") %in% data$Statistic))
})

