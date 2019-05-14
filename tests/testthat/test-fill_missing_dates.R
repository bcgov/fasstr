context("Fill missing dates")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date, Flows = Value, Stations = STATION_NUMBER)
  flowdata <- fill_missing_dates(flowdata, dates = Dates, values = Flows, groups = Stations)
  expect_true(all(c("Dates", "Flows", "Stations") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- fill_missing_dates(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- fill_missing_dates(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("all dates are added between start and end years of original data",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data_missing <- tidyhydat::hy_daily_flows(stns)
  data_filled <- fill_missing_dates(station_number = stns)
  expect_true(all(seq(as.Date(paste0(lubridate::year(min(data_missing$Date)),"-01-01")), 
                              as.Date(paste0(lubridate::year(max(data_missing$Date)),"-12-31")),
                      by = "1 day") %in% data_filled$Date))
})

test_that("gaps of missing dates are filled with NA",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data_missing <- tidyhydat::hy_daily_flows(stns)
  data_filled <- fill_missing_dates(station_number = stns)
  # is the total NAs in the unfilled data the same as the number of added without with the new NAs added
  expect_true(sum(is.na(data_missing$Value)) == 
                -(nrow(data_filled) - nrow(data_missing) - sum(is.na(data_filled$Value))))
})

# Function modifiers

test_that("record starts in the month specified by water_year_start",{
  skip_on_cran()
  skip_on_travis()
  month_to_start_water_year <- 3
  stns <- "08NM003"
  data <- fill_missing_dates(station_number = stns,  
                             water_year_start = month_to_start_water_year)
  expect_true(lubridate::month(min(data$Date)) == month_to_start_water_year)
})

