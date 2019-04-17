context("Calc annual flow timing")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_flow_timing(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 10 &
                all(c("DoY_25pct_TotalQ","Date_25pct_TotalQ","DoY_33.3pct_TotalQ","Date_33.3pct_TotalQ",
                      "DoY_50pct_TotalQ","Date_50pct_TotalQ","DoY_75pct_TotalQ","Date_75pct_TotalQ") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_flow_timing(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_flow_timing(station_number = "08NM116", start_year = 1980,
                                  percent_total = 51)
  expect_true(all(c("DoY_51pct_TotalQ","Date_51pct_TotalQ") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  percent = 51
  data <- calc_annual_flow_timing(station_number = "08NM116", start_year = 1980, end_year = 1980,
                                  percent_total = percent)
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear == 1980)
  flow_data <- dplyr::mutate(flow_data, cumsum = cumsum(Value))
  half_flow <- sum(flow_data$Value) * percent/100
  data_test <- dplyr::filter(flow_data, cumsum >= half_flow)
  
  expect_true(data[[1,3]] == data_test$DayofYear[1])
  expect_true(data[[1,4]] == data_test$Date[1])
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_flow_timing(station_number = "08NM116", start_year = 1980,
                                  transpose = TRUE)
  expect_true(all(c("DoY_25pct_TotalQ") %in% data$Statistic))
})
