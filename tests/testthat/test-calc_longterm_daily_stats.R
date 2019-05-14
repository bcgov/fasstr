context("Calc longterm daily stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                all(c("Month","Mean","Median","Maximum","Minimum") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116", start_year = 1980,
                                    percentiles = c(25,75))
  expect_true(all(c("P25","P75") %in% colnames(data)))
})

test_that("produces NA if there is missing data and warning is produced", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressWarnings(calc_longterm_daily_stats(station_number = "08NM116",
                                                     ignore_missing = FALSE))
  expect_true(any(is.na(data)))
  expect_warning(calc_longterm_daily_stats(station_number = "08NM116",
                                           ignore_missing = FALSE))
})

test_that("ignore_missing calculates all data", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116",
                                    ignore_missing = TRUE)
  expect_true(any(!is.na(data)))
})

test_that("data is filtered by years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116",
                                    start_year = 1981,
                                    end_year = 2010,
                                    exclude_years = c(1991,1993:1995))
  data <- dplyr::filter(data, Month == "Jan")
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  data_test <- add_date_variables(station_number = "08NM116")
  data_test <- dplyr::filter(data_test, 
                             WaterYear %in% c(1981:1990, 1992, 1996:2010),
                             MonthName == "Jan")
  data_test <- dplyr::summarise(data_test,
                                Mean = mean(Value),
                                Median = median(Value),
                                Maximum = max(Value),
                                Minimum = min(Value))
  
  expect_equal(data, data_test)
})

test_that("data is summarized by water years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116",
                                    start_year = 1981,
                                    end_year = 2010,
                                    water_year_start = 10)
  expect_true(data$Month[1] == "Oct")
})

test_that("rolling days are applied properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- add_rolling_means(flow_data,
                                 roll_days = 7)
  
  data_test <- dplyr::filter(flow_data, MonthName == "Jan")
  data_test <- dplyr::summarise(data_test,
                                Mean = mean(Q7Day),
                                Median = median(Q7Day),
                                Maximum = max(Q7Day),
                                Minimum = min(Q7Day))
  
  data <- calc_longterm_daily_stats(data = flow_data,
                                    roll_days = 7, ignore_missing = TRUE)
  data <- dplyr::filter(data, Month == "Jan")
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  expect_equal(data_test, data)
})

test_that("data is filtered by months properly, and include longterm is removed", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116",
                                    start_year = 1981,
                                    months = 7:9,
                                    include_longterm = FALSE)
  expect_true(all(data$Month %in% c("Jul","Aug","Sep")))
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_daily_stats(station_number = "08NM116",
                                    transpose = TRUE,
                                    complete_years = TRUE)
  expect_true(all(c("Mean","Median","Maximum","Minimum") %in% data$Statistic))
})

