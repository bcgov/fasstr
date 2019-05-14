context("Add cumulative yield")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date, Flows = Value, Stations = STATION_NUMBER)
  flowdata <- add_cumulative_yield(flowdata, dates = Dates, values = Flows, groups = Stations)
  expect_true(all(c("Dates", "Flows", "Stations") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_cumulative_yield(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_cumulative_yield(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_cumulative_yield actually adds a column called Cumul_Yield_mm",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  cumu_col <- add_cumulative_yield(station_number = stns, water_year_start = 8)
  expect_true("Cumul_Yield_mm" %in% names(cumu_col))
})

test_that("add_cumulative_yield only fills years with complete data, otherwise none",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data <- add_cumulative_yield(station_number = stns) %>% 
    fill_missing_dates() %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarize(Days = dplyr::n(),
                     Cumul = sum(is.na(Cumul_Yield_mm)),
                     Test = Cumul / Days)
  expect_true(any(data$Test == 0 | data$Test == 1)) # either 0 or 100 percent of days per year
})

test_that("the total cumulative value at the end of each year is the total volume",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM116"
  max_data <- add_cumulative_yield(station_number = stns) %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarize(Sum = round(max(Cumul_Yield_mm), 1))
  sum_data <- fill_missing_dates(station_number = stns) %>% 
    add_date_variables() %>% 
    add_basin_area() %>% 
    dplyr::mutate(Value = Value * 86400 / 1000 / Basin_Area_sqkm) %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarise(Sum = round(sum(Value, na.rm = FALSE), 1))
  expect_identical(max_data, sum_data)
})

# Function modifiers

test_that("modifying water year changes outputs",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  water_year_true <- add_cumulative_yield(station_number = stns, water_year_start = 8)
  water_year_false <- add_cumulative_yield(station_number = stns)
  expect_false(identical(water_year_true, water_year_false))
})

test_that("record starts in the month specified by water_year_start",{
  skip_on_cran()
  skip_on_travis()
  month_to_start_water_year <- 5
  stns <- "08NM003"
  water_year_true <- add_cumulative_yield(station_number = stns,  
                                           water_year_start = month_to_start_water_year)
  ## Extract Date of first non na value in Cumul_Yield_mm column
  Date <- water_year_true[which.min(is.na(water_year_true$Cumul_Yield_mm)),]$Date
  expect_identical(as.numeric(substr(Date, 6, 7)), month_to_start_water_year)
})

test_that("setting basin_area makes proper yield",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM116"
  area <- 1000
  custom_data <- add_cumulative_yield(station_number = stns, basin_area = area) %>% 
    fill_missing_dates() %>% 
    dplyr::mutate(New_Yield = Value * 86400 / 1000 / area) %>% 
    add_date_variables() %>% 
    dplyr::group_by(WaterYear) %>% 
    dplyr::summarize(Cumul = round(max(Cumul_Yield_mm), 2),
                     Cumul_New = round(sum(New_Yield, na.rm = FALSE), 2))
  expect_identical(custom_data$Cumul, custom_data$Cumul_New)
})

