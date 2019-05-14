context("Add cumulative volume")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date, Flows = Value, Stations = STATION_NUMBER)
  flowdata <- add_cumulative_volume(flowdata, dates = Dates, values = Flows, groups = Stations)
  expect_true(all(c("Dates", "Flows", "Stations") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_cumulative_volume(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_cumulative_volume(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_cumulative_volume actually adds a column called Cumul_Volume_m3",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  cumu_col <- add_cumulative_volume(station_number = stns, water_year_start = 8)
  expect_true("Cumul_Volume_m3" %in% names(cumu_col))
})

test_that("add_cumulative_volume only fills years with complete data, otherwise none",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data <- add_cumulative_volume(station_number = stns) %>% 
    fill_missing_dates() %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarize(Days = dplyr::n(),
                     Cumul = sum(is.na(Cumul_Volume_m3)),
                     Test = Cumul / Days)
  expect_true(any(data$Test == 0 | data$Test == 1)) # either 0 or 100 percent of days per year
})

test_that("the total cumulative value at the end of each year is the total volume",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM116"
  max_data <- add_cumulative_volume(station_number = stns) %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarize(Sum = max(Cumul_Volume_m3))
  sum_data <- fill_missing_dates(station_number = stns) %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarise(Sum = sum(Value, na.rm = FALSE) * 86400)
  expect_identical(max_data, sum_data)
})

# Function modifiers

test_that("modifying water year changes outputs",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  water_year_true <- add_cumulative_volume(station_number = stns, water_year_start = 8)
  water_year_false <- add_cumulative_volume(station_number = stns)
  expect_false(identical(water_year_true, water_year_false))
})

test_that("record starts in the month specified by water_year_start",{
  skip_on_cran()
  skip_on_travis()
  month_to_start_water_year <- 5
  stns <- "08NM003"
  water_year_true <- add_cumulative_volume(station_number = stns,  
                                           water_year_start = month_to_start_water_year)
  ## Extract Date of first non na value in Cumul_Volume_m3 column
  Date <- water_year_true[which.min(is.na(water_year_true$Cumul_Volume_m3)),]$Date
  expect_identical(as.numeric(substr(Date, 6, 7)), month_to_start_water_year)
})

