context("Add daily volume")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Flows = Value)
  flowdata <- add_daily_volume(flowdata, values = Flows)
  expect_true(all(c("Flows") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_daily_volume(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_daily_volume(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_daily_volume actually adds a column called Volume_m3",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  data_col <- add_daily_volume(station_number = stns)
  expect_true("Volume_m3" %in% names(data_col))
})



