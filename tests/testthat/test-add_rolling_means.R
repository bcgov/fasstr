context("Add rolling means")

# Data inputs 

test_that("dataframe can be provided and using different column names",{
  skip_on_cran()
  skip_on_travis()
  flowdata <- tidyhydat::hy_daily_flows("08NM116") %>% 
    dplyr::rename(Dates = Date, Flows = Value, Stations = STATION_NUMBER)
  flowdata <- add_rolling_means(flowdata, dates = Dates, values = Flows, groups = Stations)
  expect_true(all(c("Dates", "Flows", "Stations") %in% colnames(flowdata)) &
                nrow(flowdata) >= 1)
})

test_that("station_number can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  stations_data <- add_rolling_means(station_number = stns)
  expect_true(stns %in% unique(stations_data$STATION_NUMBER) &
                nrow(stations_data) >= 1)
})

test_that("multiple station_numbers can be provided",{
  skip_on_cran()
  skip_on_travis()
  stns <- c("08NM003","08NM116")
  stations_data <- add_rolling_means(station_number = stns)
  expect_true(all(unique(stations_data$STATION_NUMBER) %in% stns) &
                nrow(stations_data) >= 1)
})

# Function results

test_that("add_rolling_means actually adds columns",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08NM003"
  n_days <- c(1,3)
  data <- add_rolling_means(station_number = stns, roll_days = n_days)
  expect_true(all(paste0("Q", n_days, "Day") %in% names(data)))
})

test_that("add_rolling_means restart if missing data",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08HB048"
  n_days <- 3
  data <- tidyhydat::hy_daily_flows(stns) %>% 
    dplyr::filter(Date <= "1973-01-10" | Date >= "1973-01-20") %>% 
    add_rolling_means(roll_days = n_days) %>% 
    #fill_missing_dates() %>% 
    dplyr::filter(Date >= "1973-01-01",
                  Date <= "1973-01-31")
  expect_true(data[which.min(!is.na(data$Q3Day)),]$Date != data[which.min(!is.na(data$Q3Day))-1,]$Date+1  &# there is a gap
                all(is.na(data[(which.min(!is.na(data$Q3Day))):(which.min(!is.na(data$Q3Day))+n_days-2),6])))
})

# Function modifiers

test_that("add_rolling_means alight right is correct",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08HB048"
  n_days <- 3
  data <- add_rolling_means(station_number = stns, roll_days = n_days, roll_align = "right")[1:3,]
  expect_true(mean(data$Value) == data$Q3Day[3])
})

test_that("add_rolling_means alight left is correct",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08HB048"
  n_days <- 3
  data <- add_rolling_means(station_number = stns, roll_days = n_days, roll_align = "left")[1:3,]
  expect_true(mean(data$Value) == data$Q3Day[1])
})

test_that("add_rolling_means alight centre is correct",{
  skip_on_cran()
  skip_on_travis()
  stns <- "08HB048"
  n_days <- 3
  data <- add_rolling_means(station_number = stns, roll_days = n_days, roll_align = "center")[1:3,]
  expect_true(mean(data$Value) == data$Q3Day[2])
})
