context("Calc longterm percentile")

test_that("`calc_longterm_percentile()` works", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_percentile(station_number = "08NM116", start_year = 1980,
                                   percentiles = c(5,25))
  expect_true(is.data.frame(data) &
                ncol(data) == 3 &
                all(c("P5","P25") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_percentile(station_number = c("08NM116","08HB048"), start_year = 1980,
                                   percentiles = c(5,25))
  expect_true(length(unique(data$STATION_NUMBER)) &
                ncol(data) == 3 &
                all(c("P5","P25") %in% colnames(data)))
})


test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  P5 <- round(stats::quantile(flow_data$Value, 0.05, names = FALSE),5)
  P25 <- round(stats::quantile(flow_data$Value, 0.25, names = FALSE),5)
  
  data <- calc_longterm_percentile(data = flow_data,
                                   start_year = 1980, end_year = 1990,
                                   percentiles = c(5,25))
  
  expect_true(P5 == round(data[[1,2]],5))
  expect_true(P25 == round(data[[1,3]],5))
})