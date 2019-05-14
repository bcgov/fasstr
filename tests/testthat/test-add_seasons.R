context("Add seasons")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date)
  flowdata <- add_seasons(flowdata, dates = Dates, seasons_length = 6)
  expect_true(all(c("Dates") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_seasons(station_number = stns, seasons_length = 6)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_seasons(station_number = stns, seasons_length = 6)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_seasons actually adds proper columns",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  n <- 6
  data <- add_seasons(station_number = stns, seasons_length = n)
  expect_true(all(c("Season") %in% names(data)))
})

# Function modifiers

test_that("number of seasons is correct",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  n <- 4
  data <- add_seasons(station_number = stns, seasons_length = n)
  expect_true(length(unique(data$Season)) == 12/n)
})

test_that("start month of first season is the start of year",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  n <- 4
  data <- fill_missing_dates(station_number = stns, water_year_start = 3) %>% 
    add_seasons(seasons_length = n, water_year_start = 3) %>% 
    add_date_variables(water_year_start = 3) %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::top_n(n = 1, dplyr::desc(Date))
  expect_true(unique(substr(data$Season, 1, 3)) == unique(data$MonthName))
})


