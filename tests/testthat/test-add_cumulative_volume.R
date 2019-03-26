context("Add cumulative volume")

test_that("add_cumulative_volume actually adds a column called Cumul_Volume_m3",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  cumu_col <- add_cumulative_volume(station_number = stns, water_year_start = 8)
  expect_true("Cumul_Volume_m3" %in% names(cumu_col))
})

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