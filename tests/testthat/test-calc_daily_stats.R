context("Calc daily stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                all(c("Date","DayofYear","Mean","Median","Maximum","Minimum") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116", start_year = 1980,
                           percentiles = c(25,75))
  expect_true(all(c("P25","P75") %in% colnames(data)))
})

test_that("produces NA if there is missing data and warning is produced", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressWarnings(calc_daily_stats(station_number = "08NM116",
                           ignore_missing = FALSE))
  expect_true(any(is.na(data)))
  expect_warning(calc_daily_stats(station_number = "08NM116",
                                  ignore_missing = FALSE))
})

test_that("ignore_missing calculates all data", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116",
                           ignore_missing = TRUE)
  expect_true(any(!is.na(data)))
})

test_that("data is filtered by years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116",
                           start_year = 1981,
                           end_year = 2010,
                           exclude_years = c(1991,1993:1995))
  data <- dplyr::filter(data, DayofYear == 1)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  data_test <- add_date_variables(station_number = "08NM116")
  data_test <- dplyr::filter(data_test, 
                             WaterYear %in% c(1981:1990, 1992, 1996:2010),
                             DayofYear == 1)
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
  data <- calc_daily_stats(station_number = "08NM116",
                           start_year = 1981,
                           end_year = 2010,
                           water_year_start = 10)
  expect_true(data$Date[1] == "Oct-01")
})

test_that("rolling days are applied properly", {
  skip_on_cran()
  skip_on_travis()
  flow_data <- add_date_variables(station_number = "08NM116")
  flow_data <- add_rolling_means(flow_data,
                                 roll_days = 7)
  
  test_data <- dplyr::filter(flow_data, DayofYear == 1)
  test_data <- dplyr::summarise(test_data,
                                Mean = mean(Q7Day),
                                Median = median(Q7Day),
                                Maximum = max(Q7Day),
                                Minimum = min(Q7Day))
  
  data <- calc_daily_stats(data = flow_data,
                           roll_days = 7, ignore_missing = TRUE)
  data <- dplyr::filter(data, DayofYear == 1)
  data <- dplyr::select(data, Mean, Median, Maximum, Minimum)
  
  expect_equal(test_data, data)
})

test_that("data is filtered by months properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116",
                           start_year = 1981,
                           months = 7:9)
  
  expect_true(all(c(182:273) %in% data$DayofYear))
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_daily_stats(station_number = "08NM116",
                           transpose = TRUE,
                           complete_years = TRUE)
  expect_true(all(c("Mean","Median","Maximum","Minimum") %in% data$Statistic))
})

