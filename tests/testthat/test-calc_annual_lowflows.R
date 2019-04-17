context("Calc annual lowflows")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_lowflows(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 14 &
                all(c("Min_1_Day","Min_1_Day_DoY","Min_1_Day_Date") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_lowflows(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with seasons", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_lowflows(station_number = "08NM116", start_year = 1980,
                               roll_days = 5)
  expect_true(is.data.frame(data) &
                ncol(data) == 5 &
                all(c("Min_5_Day","Min_5_Day_DoY","Min_5_Day_Date") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  data <- calc_annual_lowflows(station_number = "08NM116", 
                               start_year = 1980, end_year = 1980)
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- add_rolling_means(flow_data)
  flow_data <- dplyr::filter(flow_data, WaterYear == 1980)
  expect_true(round(data[[1,9]],1) == round(min(flow_data$Q7Day),1))
  
  data_test <- dplyr::filter(flow_data, Q7Day == min(flow_data$Q7Day))
  expect_true(data[[1,10]] == min(data_test$DayofYear))
  expect_true(data[[1,11]] == min(data_test$Date))
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_lowflows(station_number = "08NM116", start_year = 1980,
                               transpose = TRUE)
  expect_true(all(c("Min_1_Day","Min_1_Day_DoY") %in% data$Statistic))
})