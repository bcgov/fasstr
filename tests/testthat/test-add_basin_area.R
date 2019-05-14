context("Add basin area")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date, Flows = Value, Stations = STATION_NUMBER)
  flowdata <- add_basin_area(flowdata, groups = Stations)
  expect_true(all(c("Stations") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_basin_area(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_basin_area(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_basin_area actually adds a column called Basin_Area_sqkm",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data <- add_basin_area(station_number = stns)
  expect_true("Basin_Area_sqkm" %in% names(data))
})

# Function modifiers

test_that("setting basin_area to a value overrides the HYDAT value",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM116"
  area <- 1000
  data <- add_basin_area(station_number = stns, basin_area = area)
  expect_true(unique(data$Basin_Area_sqkm) == area)
})

test_that("setting basin_area list overrides all basin areas",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM116", "08NM003")
  area <- c("08NM116" = 1000, "08NM003" = 10)
  data <- add_basin_area(station_number = stns, basin_area = area) %>% 
    dplyr::group_by(STATION_NUMBER) %>% 
    dplyr::summarise(Area = mean(Basin_Area_sqkm))
  
  expect_true(data[data$STATION_NUMBER == "08NM116", 2] == 1000 &
                data[data$STATION_NUMBER == "08NM003", 2] == 10)
})

test_that("setting basin_area list with NA overrides basin areas",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM116", "08NM003")
  area <- c("08NM116" = 1000, "08NM003" = NA)
  data <- add_basin_area(station_number = stns, basin_area = area) %>% 
    dplyr::group_by(STATION_NUMBER) %>% 
    dplyr::summarise(Area = mean(Basin_Area_sqkm))
  expect_true(data[data$STATION_NUMBER == "08NM116", 2] == 1000 &
                is.na(data[data$STATION_NUMBER == "08NM003", 2]))
})

test_that("setting basin_area list grabs the HYDAT area if missing",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM116", "08NM003")
  area <- c("08NM003" = 10)
  data <- suppressWarnings(add_basin_area(station_number = stns, basin_area = area)) %>% 
    dplyr::group_by(STATION_NUMBER) %>% 
    dplyr::summarise(Area = mean(Basin_Area_sqkm))
  expect_true(data[data$STATION_NUMBER == "08NM116", 2] == 795 &
                data[data$STATION_NUMBER == "08NM003", 2] == 10)
})


