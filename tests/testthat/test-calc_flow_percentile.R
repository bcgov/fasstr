context("Calc flow percentile")

test_that("`calc_flow_percentile()` works", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_flow_percentile(station_number = "08NM116", start_year = 1980,
                               flow_value = 10)
  expect_true(is.data.frame(data) &
                ncol(data) == 2 &
                all(c("Percentile") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_flow_percentile(station_number = c("08NM116","08HB048"), start_year = 1980,
                               flow_value = 10)
  expect_true(length(unique(data$STATION_NUMBER)) &
                ncol(data) == 2 &
                all(c("Percentile") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  ptile <- round(stats::ecdf(flow_data$Value)(5.00), 5) * 100
  
  data <- calc_flow_percentile(data = flow_data,
                                      start_year = 1980, end_year = 1990,
                               flow_value = 5.00)

  expect_true(ptile == data[[1,2]])
})
