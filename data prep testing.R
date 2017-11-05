test <- fasstr_add_date_vars(HYDAT = "08HB048",water_year_start = 2)

water_year <- T
HYDAT <- "08HB048"
water_year_start <- 2
flowdata <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
flowdata <- dplyr::select(flowdata,Date,Q=Value)
test <- fasstr_fill_missing_dates(flowdata = flowdata)
test <- fasstr_fill_missing_dates(HYDAT = "08HB048")
test <- fasstr_fill_missing_dates(HYDAT = "08HB048",water_year = T, water_year_start = 1)
test2 <- fasstr_add_rolling_means(HYDAT = "08NM116")
test2 <- fasstr_fill_missing_dates(flowdata = test2)
test2 <- fasstr_add_date_vars(flowdata = data)


test2 <- fasstr_fill_missing_dates(HYDAT = "08NM116")
test2 <- fasstr_add_rolling_means(flowdata = test2)
test2 <- fasstr_add_date_vars(test2)

test2 <- fasstr_add_rolling_means(HYDAT = "08NM116")
test2 <- fasstr_fill_missing_dates(flowdata = test2)
test2 <- fasstr_add_date_vars(test2)

test2 <- fasstr_add_date_vars(HYDAT = "08NM116")
test2 <- fasstr_add_rolling_means(test2)
test2 <- fasstr_fill_missing_dates(test2)


test2 <- fasstr_fill_missing_dates(HYDAT = "08NM116")



