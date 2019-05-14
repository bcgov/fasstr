context("Calc all annual stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_all_annual_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 109 &
                all(c("Year","Annual_Mean","Annual_Median","Annual_Maximum",
                      "Annual_Minimum","Annual_P10","Annual_P90") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_all_annual_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_all_annual_stats(station_number = "08NM116", start_year = 1980,
                                annual_percentiles = c(25,75))
  expect_true(all(c("Annual_P25","Annual_P75") %in% colnames(data)))
})

test_that("data is filtered by years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_all_annual_stats(station_number = "08NM116",
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
                                Annual_Mean = mean(Value),
                                Annual_Median = median(Value),
                                Annual_Maximum = max(Value),
                                Annual_Minimum = min(Value))
  
  data <- calc_all_annual_stats(data = flow_data,
                                start_year = 1981, end_year = 2000,
                                water_year_start = 10)
  data <- dplyr::filter(data, Year == 1981)
  data <- dplyr::select(data, Annual_Mean, Annual_Median, Annual_Maximum, Annual_Minimum)
  
  expect_equal(test_data, data)
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_all_annual_stats(station_number = "08NM116",start_year = 1981,
                                transpose = TRUE)
  expect_true(all(c("Annual_Mean","Annual_Median","Annual_Maximum","Annual_Minimum") %in% data$Statistic))
})


