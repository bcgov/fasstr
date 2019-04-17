context("Calc daily cumulative stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 11 &
                all(c("Date","DayofYear","Mean","Median","Maximum","Minimum") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_cumulative_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom percentiles", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1980,
                                      percentiles = c(25,75))
  expect_true(is.data.frame(data) &
                ncol(data) == 9 &
                all(c("P25","P75") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  flow_data <- dplyr::mutate(dplyr::group_by(flow_data, WaterYear),
                             cumsum = cumsum(Value) * 86400)
  flow_data <- dplyr::ungroup(flow_data)
  test_data <- dplyr::summarise(dplyr::group_by(flow_data, DayofYear),
                                Mean = mean(cumsum),
                                Median = median(cumsum),
                                Maximum = max(cumsum),
                                Minimum = min(cumsum))
  test_data <- dplyr::filter(test_data, DayofYear != 366)
  
  
  data <- calc_daily_cumulative_stats(data = flow_data,
                          start_year = 1980, end_year = 1990)
  data <- dplyr::select(data, DayofYear, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
})

test_that("it is calculated correctly for yield (math and extracting basin_area)", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  flow_data <- dplyr::mutate(dplyr::group_by(flow_data, WaterYear),
                             cumsum = cumsum(Value) * 86400 / 1000 / 795)
  flow_data <- dplyr::ungroup(flow_data)
  test_data <- dplyr::summarise(dplyr::group_by(flow_data, DayofYear),
                                Mean = mean(cumsum),
                                Median = median(cumsum),
                                Maximum = max(cumsum),
                                Minimum = min(cumsum))
  test_data <- dplyr::filter(test_data, DayofYear != 366)
  
  
  data <- calc_daily_cumulative_stats(data = flow_data, use_yield = TRUE,
                                      start_year = 1980, end_year = 1990)
  data <- dplyr::select(data, DayofYear, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data$Mean, data$Mean)
})