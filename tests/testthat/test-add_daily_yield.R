context("Add daily yield")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Flows = Value)
  flowdata <- add_daily_yield(flowdata, values = Flows)
  expect_true(all(c("Flows") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_daily_yield(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_daily_yield(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_daily_yield actually adds a column called Yield_mm",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data_col <- add_daily_yield(station_number = stns)
  expect_true("Yield_mm" %in% names(data_col))
})


# Function modifiers

test_that("setting basin_area to a value overrides the HYDAT value",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM116"
  area <- 1000
  data <- add_daily_yield(station_number = stns, basin_area = area) %>% 
    add_basin_area(basin_area = area) %>% 
    dplyr::mutate(Yield = round(Value * 86400 / 1000 / 1000,3),
                  Yield_mm = round(Yield_mm,3))
  expect_identical(data$Yield_mm, data$Yield)
})


test_that("setting basin_area list overrides all basin areas",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM116", "08NM003")
  area <- c("08NM116" = 1000, "08NM003" = 10)
  data <- add_daily_yield(station_number = stns, basin_area = area) %>% 
    add_basin_area(basin_area = area) %>% 
    dplyr::mutate(Yield = round(Value * 86400 / 1000 / Basin_Area_sqkm, 3),
                  Yield_mm = round(Yield_mm, 3))
  expect_identical(data$Yield_mm, data$Yield)
})





