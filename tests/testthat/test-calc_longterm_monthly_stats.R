context("Calc longterm monthly stats")

test_that("creates a dataframe with the proper columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116", start_year = 1980)
  expect_true(is.data.frame(data) &
                all(c("Month","Mean","Median","Maximum","Minimum") %in% colnames(data)))
})

test_that("outputs data for two stations", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = c("08NM116","08HB048"), start_year = 1980)
  expect_true(length(unique(data$STATION_NUMBER)) == 2)
})

test_that("creates a dataframe with custom columns", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116", start_year = 1980,
                                      percentiles = c(25,75))
  expect_true(all(c("P25","P75") %in% colnames(data)))
})

test_that("produces NA if there is missing data and warning is produced", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressWarnings(calc_longterm_monthly_stats(station_number = "08NM116",
                                                       ignore_missing = FALSE))
  expect_true(any(is.na(data)))
  expect_warning(calc_longterm_monthly_stats(station_number = "08NM116",
                                             ignore_missing = FALSE))
})

test_that("ignore_missing calculates all data", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116",
                                      ignore_missing = TRUE)
  expect_true(any(!is.na(data)))
})

test_that("data calculated properly", {
  skip_on_cran()
  skip_on_travis()
  ignore_missing = TRUE
  data <- calc_longterm_monthly_stats(station_number = "08NM116", start_year = 1980, 
                                      ignore_missing = ignore_missing)
  data <- dplyr::select(data, -STATION_NUMBER)
  
  annual <- calc_annual_stats(station_number = "08NM116", start_year = 1980, 
                              ignore_missing = ignore_missing)
  annual <- dplyr::rename(annual, Annual_Mean = Mean)
  annual <- dplyr::mutate(annual, Month = "Annual") 
  annual <- dplyr::group_by(annual, Month)
  annual <- dplyr::summarise(annual, 
                             Mean = mean(Annual_Mean, na.rm = ignore_missing),
                             Median = median(Annual_Mean, na.rm = ignore_missing),
                             Maximum = max(Annual_Mean, na.rm = ignore_missing),
                             Minimum = min(Annual_Mean, na.rm = ignore_missing),
                             P10 = ifelse(!is.na(Mean), quantile(Annual_Mean, 0.1, na.rm = ignore_missing), NA),
                             P90 = ifelse(!is.na(Mean), quantile(Annual_Mean, 0.9, na.rm = ignore_missing), NA))
  month <- calc_monthly_stats(station_number = "08NM116", start_year = 1980, 
                              ignore_missing = ignore_missing)
  month <- dplyr::rename(month, Month_Mean = Mean)
  month <- dplyr::group_by(month, Month)
  month <- dplyr::summarise(month, 
                            Mean = mean(Month_Mean, na.rm = ignore_missing),
                            Median = median(Month_Mean, na.rm = ignore_missing),
                            Maximum = max(Month_Mean, na.rm = ignore_missing),
                            Minimum = min(Month_Mean, na.rm = ignore_missing),
                            P10 = ifelse(!is.na(Mean), quantile(Month_Mean, 0.1, na.rm = ignore_missing), NA),
                            P90 = ifelse(!is.na(Mean), quantile(Month_Mean, 0.9, na.rm = ignore_missing), NA))
  month <- rbind(month, annual)
  
  expect_equal(data, month)
})


test_that("data calculated properly when ignore_missing", {
  skip_on_cran()
  skip_on_travis()
  ignore_missing = FALSE
  data <- suppressWarnings(calc_longterm_monthly_stats(station_number = "08HB048", 
                                                       ignore_missing = ignore_missing))
  data <- dplyr::select(data, -STATION_NUMBER)
  
  annual <- suppressWarnings(calc_annual_stats(station_number = "08HB048", 
                                               ignore_missing = ignore_missing))
  annual <- dplyr::rename(annual, Annual_Mean = Mean)
  annual <- dplyr::mutate(annual, Month = "Annual") 
  annual <- dplyr::group_by(annual, Month)
  annual <- dplyr::summarise(annual, 
                             Mean = mean(Annual_Mean, na.rm = ignore_missing),
                             Median = median(Annual_Mean, na.rm = ignore_missing),
                             Maximum = max(Annual_Mean, na.rm = ignore_missing),
                             Minimum = min(Annual_Mean, na.rm = ignore_missing),
                             P10 = ifelse(!is.na(Mean), quantile(Annual_Mean, 0.1, na.rm = ignore_missing), NA),
                             P90 = ifelse(!is.na(Mean), quantile(Annual_Mean, 0.9, na.rm = ignore_missing), NA))
  month <- suppressWarnings(calc_monthly_stats(station_number = "08HB048", 
                                               ignore_missing = ignore_missing))
  month <- dplyr::rename(month, Month_Mean = Mean)
  month <- dplyr::group_by(month, Month)
  month <- dplyr::summarise(month, 
                            Mean = mean(Month_Mean, na.rm = ignore_missing),
                            Median = median(Month_Mean, na.rm = ignore_missing),
                            Maximum = max(Month_Mean, na.rm = ignore_missing),
                            Minimum = min(Month_Mean, na.rm = ignore_missing),
                            P10 = ifelse(!is.na(Mean), quantile(Month_Mean, 0.1, na.rm = ignore_missing), NA),
                            P90 = ifelse(!is.na(Mean), quantile(Month_Mean, 0.9, na.rm = ignore_missing), NA))
  month <- rbind(month, annual)
  
  expect_equal(data, month)
})

test_that("data is summarized by water years properly", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116",
                                      start_year = 1981,
                                      end_year = 2010,
                                      water_year_start = 10)
  expect_true(data$Month[1] == "Oct")
})

test_that("data is filtered by months properly, and include longterm is removed", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116",
                                      start_year = 1981,
                                      months = 7:9,
                                      include_annual = FALSE)
  expect_true(all(data$Month %in% c("Jul","Aug","Sep")))
})

test_that("transpose properly transposed the results", {
  skip_on_cran()
  skip_on_travis()
  data <- calc_longterm_monthly_stats(station_number = "08NM116",
                                      transpose = TRUE,
                                      complete_years = TRUE)
  expect_true(all(c("Mean","Median","Maximum","Minimum") %in% data$Statistic))
})

