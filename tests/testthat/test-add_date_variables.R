context("Add date variables")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date)
  flowdata <- add_date_variables(flowdata, dates = Dates)
  expect_true(all(c("Dates") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_date_variables(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_date_variables(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_date_variables actually adds proper columns",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data <- add_date_variables(station_number = stns)
  expect_true(all(c("CalendarYear","Month","MonthName","WaterYear","DayofYear") %in% names(data)))
})

# Function modifiers

test_that("water years start on first day of selected months",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  yr_str <- 3
  data <- fill_missing_dates(station_number = stns, water_year_start = yr_str) %>% 
    add_date_variables(water_year_start = yr_str) %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::top_n(n = 1, dplyr::desc(Date))
  lubridate::year(data$Date) <- 0
  expect_true(all(data$Date == paste0("0000-",yr_str,"-01")))
})

test_that("first day of years start on first day of selected months",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  yr_str <- 4
  data <- fill_missing_dates(station_number = stns, water_year_start = yr_str) %>% 
    add_date_variables(water_year_start = yr_str) %>% 
    dplyr::top_n(n = -1, DayofYear)
  lubridate::year(data$Date) <- 0
  expect_true(all(data$Date == paste0("0000-",yr_str,"-01")))
})

