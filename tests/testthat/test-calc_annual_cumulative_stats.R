context("Calc annual cumulative stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_cumulative_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                ncol(data) == 3 &
                all(c("Year","Total_Volume_m3") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_cumulative_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with seasons", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_cumulative_stats(station_number = "08NM116", start_year = 1980,
                                       include_seasons = TRUE)
  expect_true(is.data.frame(data) &
                ncol(data) == 9)
})

test_that("it is calculated correctly", {
  skip_on_cran()
  skip_on_travis()
  
  data <- calc_annual_cumulative_stats(station_number = "08NM116", 
                                       start_year = 1980, end_year = 1980)
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear == 1980)
  flow_data <- dplyr::mutate(flow_data, cumsum = cumsum(Value) * 86400)
  
  expect_true(round(data[[1,3]],0) == round(max(flow_data$cumsum),0))
})

test_that("it is calculated correctly for yield (math and extracting basin_area)", {
  skip_on_cran()
  skip_on_travis()
  
  data <- calc_annual_cumulative_stats(station_number = "08NM116", 
                                       start_year = 1980, end_year = 1980,
                                       use_yield = TRUE)
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- dplyr::filter(flow_data, WaterYear == 1980)
  flow_data <- dplyr::mutate(flow_data, cumsum = cumsum(Value) * 86400 / 1000 / 795)
  expect_true(round(data[[1,3]],2) == round(max(flow_data$cumsum),2))
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_annual_cumulative_stats(station_number = "08NM116", start_year = 1980,
                                       transpose = TRUE)
  expect_true(all(c("Total_Volume_m3") %in% data$Statistic))
})
