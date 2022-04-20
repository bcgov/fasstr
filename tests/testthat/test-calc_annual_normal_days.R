context("Calc annual outside normal")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_ci()
  data <- calc_annual_normal_days(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 5 &
                all(c("Year","Below_Normal_Days","Above_Normal_Days","Normal_Days") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_ci()
  data <- calc_annual_normal_days(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_ci()
  upper <- 50
  lower <- 45
  
  data <- calc_annual_normal_days(station_number = "08NM116", start_year = 1980, end_year = 2010,
                                     normal_percentiles = c(lower, upper))
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear >= 1980, WaterYear <=2010)
  data_test <- dplyr::group_by(flow_data, DayofYear)
  data_test <- dplyr::summarise(data_test,
                                Upper_ptile = stats::quantile(Value, upper/100),
                                Lower_ptile = stats::quantile(Value, lower/100))
  data_test <- dplyr::left_join(flow_data, data_test, by = "DayofYear")
  data_test <- dplyr::filter(data_test, WaterYear == 1980)
  data_test <- dplyr::mutate(data_test,
                             normal = ifelse(Value < Lower_ptile, "low",
                                             ifelse(Value > Upper_ptile, "high", "norm")))
  data_test <- dplyr::summarise(dplyr::group_by(data_test, normal), counts = dplyr::n())
  high <- as.numeric(data_test$counts[1])
  low <- as.numeric(data_test$counts[2])
  
  expect_true(data[[1,4]] == low)
  expect_true(data[[1,5]] == high)
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_ci()
  data <- calc_annual_normal_days(station_number = "08NM116", start_year = 1980,
                                     transpose = TRUE)
  expect_true(all(c("Below_Normal_Days","Above_Normal_Days","Normal_Days") %in% data$Statistic))
})
