context("Calc longterm mad")

test_that("`calc_longterm_mad()` works", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_mad(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 2 &
                all(c("LTMAD") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_mad(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) &
                ncol(data) == 2 &
                all(c("LTMAD") %in% colnames(data)))
})

test_that("percent mad is added correctly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_mad(station_number = "08NM116", start_year = 1980, 
                            percent_MAD = 25)
  expect_true(length(unique(data$STATION_NUMBER)) &
                ncol(data) == 3 &
                all(c("LTMAD","25%MAD") %in% colnames(data)))
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear %in% 1980:1990)
  longterm_mean <- round(mean(flow_data$Value),5)
  ptile_mean <- round(longterm_mean * .25,5)
  
  data <- calc_longterm_mad(data = flow_data,
                            start_year = 1980, end_year = 1990,
                            percent_MAD = 25)
  
  expect_true(longterm_mean == round(data[[1,2]],5))
  expect_true(ptile_mean == round(data[[1,3]],5))
})