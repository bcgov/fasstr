context("Calc monthly cumulative stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_monthly_cumulative_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 10 &
                all(c("Month","Mean","Median","Maximum","Minimum") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_monthly_cumulative_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom percentiles", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_monthly_cumulative_stats(station_number = "08NM116", start_year = 1980,
                                      percentiles = c(26,76))
  expect_true(is.data.frame(data) &
                ncol(data) == 8 &
                all(c("P26","P76") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  flow_data <- dplyr::mutate(flow_data,
                             total = Value * 86400)
  test_data <- dplyr::summarise(dplyr::group_by(flow_data, WaterYear, MonthName),
                                Tote_Flow = sum(total))
  test_data <- dplyr::ungroup(test_data)
  test_data <- dplyr::mutate(dplyr::group_by(test_data, WaterYear),
                             cumsum = cumsum(Tote_Flow))
  test_data <- dplyr::ungroup(test_data)
  test_data <- dplyr::summarise(dplyr::group_by(test_data, MonthName),
                                Mean = mean(cumsum),
                                Median = median(cumsum),
                                Maximum = max(cumsum),
                                Minimum = min(cumsum))
  
  data <- calc_monthly_cumulative_stats(data = flow_data,
                                        start_year = 1980, end_year = 1990)
  data <- dplyr::select(data, MonthName = Month, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data$Mean, data$Mean)
  expect_equal(test_data$Median, data$Median)
  expect_equal(test_data$Maximum, data$Maximum)
  expect_equal(test_data$Minimum, data$Minimum)
})

test_that("it is calculated correctly for yield (math and extracting basin_area)", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  flow_data <- dplyr::mutate(flow_data,
                             total = Value * 86400 / 1000 / 795)
  test_data <- dplyr::summarise(dplyr::group_by(flow_data, WaterYear, MonthName),
                                Tote_Flow = sum(total))
  test_data <- dplyr::ungroup(test_data)
  test_data <- dplyr::mutate(dplyr::group_by(test_data, WaterYear),
                             cumsum = cumsum(Tote_Flow))
  test_data <- dplyr::ungroup(test_data)
  test_data <- dplyr::summarise(dplyr::group_by(test_data, MonthName),
                                Mean = mean(cumsum),
                                Median = median(cumsum),
                                Maximum = max(cumsum),
                                Minimum = min(cumsum))
  
  data <- calc_monthly_cumulative_stats(data = flow_data, use_yield = TRUE,
                                        start_year = 1980, end_year = 1990)
  data <- dplyr::select(data, MonthName = Month, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data$Mean, data$Mean)
  expect_equal(test_data$Median, data$Median)
  expect_equal(test_data$Maximum, data$Maximum)
  expect_equal(test_data$Minimum, data$Minimum)
  })