### Quick fasstr demo



# Install
install.packages("devtools")
devtools::install_github("bcgov/fasstr")

library(fasstr)
library(dplyr)

# Download HYDAT
tidyhydat::download_hydat()



# get outputs from typing

3

5

"Hello World"

# Calculations

3 + 5

3 * (4 + 5)

# Assignment (shortcut for "<-" == alt-minus)

hrs_per_day <- 24
days_per_week <- 7
hrs_per_week <- hrs_per_day * days_per_week
hrs_per_week

# Functions

log(10)

sqrt(4)

max(4,8,2,6,9)


# Arguments (show ?)

log(5) # positional matching important

log(5, 10)

log(x=5, base=10) 

log(base=10, x=5)

log(10,5)


# Data structures

x <- "Hello World"
class(x)

y <- 10
class(y)

q <- TRUE
class(q)

5 > 3 # >, <, >=, <=, ==, !=

a <- 9
b <- 10
a == b
a != b

# Vectors - one-dimensional with a series of values of the same class

x <- c(1, 2, 3)
x
x <- 1:4
x
x <- c(1:4,10,50)
x

length(x)

y <- c(TRUE, TRUE, FALSE, FALSE)

first.names <- c("Erin", "Kyle", "Natasha")

# Coersion

as.numeric("1")

as.character(1:2)

x <- c("txt", "one", "1", "1.9")
y <- as.numeric(x)


# Indexing

first.names <- c("Sarah", "Tracy", "John")
first.names[1]
first.names[c(1,3)]
first.names[-2]

first.names[first.names %in% c("Sarah", "John")]



# Data frames - de facto data structure for most tabular data and what we use for statistics

dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
dat
dat <- read.csv("08MH152_with_USA_DATA.csv")
dat
View(dat)

head(dat) # show first 6 rows
tail(dat) # show last 6 rows
dim(dat) # returns the dimensions
nrow(dat) # number of rows
ncol(dat) # number of columns
str(dat) # structure of each column
names(dat) # shows the names attribute for a data frame, which gives the column names

dat[2]
dat$Date


write.csv(dat, file = "example_data.csv", row.names = FALSE)

# Tidyverse and  Piping - working in a pipeline

library(dplyr)
library(tidyr)

dat <- read.csv("08MH152_with_USA_DATA.csv")
dat <- select(dat, Date, Value)  # select the columns
dat <- mutate(dat,  #mutate - create/format a column
              Date = as.Date(Date),
              Volume = Value * 86400,
              Yield = Volume / 1000 / 10.3)
dat <- filter(dat, Yield >= 100)  # filter a column by specified 


dat <- read.csv("08MH152_with_USA_DATA.csv") %>% 
  select(Date, Value) %>% 
  mutate(Date = as.Date(Date),
         Volume = Value * 86400,
         Yield = Volume / 1000 / 10.3) %>% 
  filter(Yield >= 100)




## tidyhydat
#############

library(tidyhydat)

hy_version()

# Daily data
?hy_daily_flows

dat <- hy_daily_flows(station_number = "08NJ168") # Five mile creek above city intake
dat
dat <- hy_daily_flows(station_number = c("08NJ168", "08NJ013")) #slocan river near crescent valley
dat

hy_plot(station_number = "08NJ168")

hy_annual_stats(station_number = "08NJ168")


rt <- realtime_dd(station_number = "08NJ013")
rt_daily <- realtime_daily_mean(rt)

realtime_plot(station_number = "08NJ013")

meta <- hy_stations()

# Get list of active 08NE stations
stn_list <- search_stn_number("08NE") %>% 
  pull(STATION_NUMBER)

stn_list <- hy_stations(station_number = stn_list) %>% 
  filter(HYD_STATUS == "ACTIVE",
         REAL_TIME == TRUE) %>% 
  pull(STATION_NUMBER)

columbia_flows <- hy_daily_flows(station_number = stn_list) # Five mile creek above city intake

?tidyhydat



# Plotting

library(ggplot2)

dat <- hy_daily_flows(station_number = "08NJ013") # Five mile creek above city intake

ggplot(data = dat, aes(x = Date, y = Value)) + 
  geom_line() +
  geom_point(colour = "blue") +
  ylab("Discharge (cms)") +
  ggtitle("Mean Daily Flows - Five Mile Creek")


## fasstr
#############

# Data sourcing - station_number or data arguments

stats <- calc_annual_stats(station_number = "08NJ168")

flow_data <- hy_daily_flows(station_number = "08NJ168")
stats <- calc_annual_stats(data = flow_data,
                           dates = Date,
                           values = Value)
stats <- calc_annual_stats(data = flow_data)

stats <- hy_daily_flows(station_number = "08NJ168") %>% 
  calc_annual_stats()

# dataframe
stats <- calc_annual_stats(station_number = c("08NJ013", "08NJ168"))

# list of plots
plot_annual_stats(station_number = "08NJ168")
plot_annual_stats(station_number = c("08NJ013", "08NJ168"))

# Data cleaning (adding rows and columns)

plot_flow_data(station_number = "08NJ013")

flow_data <- hy_daily_flows(station_number = "08NJ013") %>% 
  fill_missing_dates()

flow_data <- hy_daily_flows(station_number = "08NJ013") %>% 
  fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield() %>% #basin_area = 10.3
  add_cumulative_yield() %>%  #basin_area = 10.3
  add_seasons()


# Arguments

stats <- calc_annual_stats(station_number = "08NJ168")

stats <- calc_annual_stats(station_number = "08NJ168",
                           start_year = 1990,
                           end_year = 2000,
                           exclude_years = 1992:1993,
                           roll_days = 7,
                           percentiles = c(5,10,15,25,75))

?calc_annual_stats

stats <- calc_annual_stats(data = NULL, 
                           dates = Date, 
                           values = Value,
                           groups = STATION_NUMBER, 
                           station_number = "08NJ168", 
                           roll_days = 1,
                           roll_align = "right",
                           percentiles = c(10, 90),
                           water_year = FALSE,
                           water_year_start = 10, 
                           start_year = 0,
                           end_year = 9999,
                           exclude_years = NULL,
                           months = 1:12,
                           transpose = FALSE,
                           ignore_missing = FALSE)

?plot_annual_stats

plot_annual_stats(station_number = "08NJ168",
                  start_year = 1990,
                  end_year = 2000,
                  exclude_years = 1992:1993,
                  roll_days = 7,
                  percentiles = c(5,10,15,25,75))

plot_annual_stats(data = NULL, 
                  dates = Date, 
                  values = Value,
                  groups = STATION_NUMBER, 
                  station_number = "08NJ168", 
                  percentiles = NA,
                  roll_days = 1, 
                  roll_align = "right", 
                  water_year = FALSE,
                  water_year_start = 10, 
                  start_year = 0, 
                  end_year = 9999,
                  exclude_years = NULL, 
                  months = 1:12, 
                  ignore_missing = FALSE,
                  log_discharge = FALSE,
                  include_title = FALSE)

## Data screening



screening <- screen_flow_data(station_number = "08NJ168")

plot_data_screening(station_number = "08NJ168")

plot_missing_dates(station_number = "08NJ168")

plot_missing_dates(station_number = "08NM116")
plot_missing_dates(station_number = "08NM116",
                   start_year = 1980)

plot_flow_data(station_number = "08NJ168")
plot_flow_data(station_number = c("08NJ168","08NJ013"))
plot_flow_data(station_number = c("08NJ168","08NJ013"),
               one_plot = TRUE)

### Example - station yield comparison

data <- hy_daily_flows(station_number = c("08NP003","08NP004")) %>% 
  add_daily_yield()

plot_flow_data(data = data)
plot_flow_data(data = data,
               one_plot = TRUE)

plot_flow_data(data = data, 
               values = Yield_mm, 
               start_year = 1978, 
               end_year=1995,
               one_plot = TRUE)


## Basic summary stats

stats <- calc_longterm_stats(station_number = "08NJ168",
                             start_year = 1990)
stats <- calc_annual_stats(station_number = "08NJ168",
                           start_year = 1990)
stats <- calc_monthly_stats(station_number = "08NJ168",
                            start_year = 1990)
stats <- calc_daily_stats(station_number = "08NJ168",
                          start_year = 1990)


plot_longterm_stats(station_number = "08NJ168",
                    start_year = 1990)
plot_annual_stats(station_number = "08NJ168",
                  start_year = 1990)
plot_monthly_stats(station_number = "08NJ168",
                   start_year = 1990)
plot_daily_stats(station_number = "08NJ168",
                 start_year = 1990)
plot_daily_stats(station_number = "08NJ168",
                 start_year = 1990,
                 include_year = 1991)

# Flow duration
plot_flow_duration(station_number = "08NJ168",
                   start_year = 1990)

plot_flow_duration(station_number = "08NJ168",
                   start_year = 1990,
                   months = 5:9,
                   include_longterm = FALSE)

# Example of yield

stats <- add_daily_yield(station_number = "08NJ168") %>% 
  calc_daily_stats(ignore_missing = TRUE)

daily <- add_daily_yield(station_number = "08NJ168") %>% 
  plot_daily_stats(ignore_missing = TRUE)

daily <- add_daily_yield(station_number = "08NJ168") %>% 
  plot_daily_stats(values = Yield_mm,
                   ignore_missing = TRUE)


ggsave("daily_runoff.png", daily[[1]])
ggsave("daily_runoff.png", daily$Daily_Stats)


write_plots(plots = daily, foldername = "Test folder", plot_type = "png")
write_plots(plots = daily, foldername = "Test folder", plot_type = "png",
            width = 10, height = 4)

## Cumulative Stats

stats <- calc_annual_cumulative_stats(station_number = "08NJ168")
stats <- calc_annual_cumulative_stats(station_number = "08NJ168",
                                      use_yield = TRUE)
stats <- calc_monthly_cumulative_stats(station_number = "08NJ168",
                                       use_yield = TRUE)
stats <- calc_daily_cumulative_stats(station_number = "08NJ168",
                                     use_yield = TRUE)

plot_annual_cumulative_stats(station_number = "08NJ168",
                             use_yield = TRUE)
plot_monthly_cumulative_stats(station_number = "08NJ168",
                              use_yield = TRUE)
plot_daily_cumulative_stats(station_number = "08NJ168",
                            use_yield = TRUE,
                            start_year = 1984
                            )

## Annual  stats

calc_annual_stats(station_number = "08NJ168")
calc_annual_flow_timing(station_number = "08NJ168")
calc_annual_lowflows(station_number = "08NJ168")
calc_annual_outside_normal(station_number = "08NJ168")
calc_annual_cumulative_stats(station_number = "08NJ168", include_seasons = TRUE)

plot_annual_stats(station_number = "08NJ168")
plot_annual_flow_timing(station_number = "08NJ168")
plot_annual_lowflows(station_number = "08NJ168")
plot_annual_outside_normal(station_number = "08NJ168")
plot_annual_cumulative_stats(station_number = "08NJ168", include_seasons = TRUE)
plot_annual_cumulative_stats(station_number = "08NJ168", include_seasons = TRUE,
                             use_yield = TRUE)



stats <- calc_all_annual_stats(station_number = "08NJ168")


## Trending

?compute_annual_trends

trends <- compute_annual_trends(station_number = "08NJ168",
                                zyp_method = "yuepilon",
                                zyp_alpha = 0.05)
trends$Annual_Trends_Data
trends$Annual_Trends_Results
trends$Annual_Maximum

trends_data <- trends$Annual_Trends_Data
trends_results <- trends$Annual_Trends_Results

trends_plot <- trends[-(1:2)]


## Frequency Analysis

?compute_annual_frequencies

freq <- compute_annual_frequencies(station_number = "08NJ168")

compute_annual_frequencies(data = NULL, 
                           dates = Date,
                           values = Value,
                           station_number = NULL,
                           roll_days = c(1, 3, 7, 30),
                           roll_align = "right",
                           use_max = FALSE, 
                           use_log = FALSE,
                           prob_plot_position = c("weibull","median", "hazen"),
                           prob_scale_points = c(0.9999, 0.999, 0.99, 0.9, 0.5, 0.2, 0.1, 0.02, 0.01, 0.001, 1e-04), 
                           fit_distr = c("PIII", "weibull"),
                           fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                           fit_quantiles = c(0.975, 0.99, 0.98, 0.95, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05,0.01), 
                           water_year = FALSE,
                           water_year_start = 10,
                           start_year = 0,
                           end_year = 9999, 
                           exclude_years = NULL,
                           months = 1:12,
                           ignore_missing = FALSE)


freq$Q_stat
freq$plotdata
freq$freqplot
freq$fit
freq$fitted_quantiles


## Compute everythiiiiing (The Robin Pike Button)

?compute_full_analysis


stats <- compute_full_analysis(station_number = "08NJ168",
                               ignore_missing = TRUE)

stats <- compute_full_analysis(station_number = "08NJ168",
                               ignore_missing = TRUE,
                               sections = 4)


stats <- compute_full_analysis(station_number = "08NJ168",
                               ignore_missing = TRUE,
                               write_to_dir = TRUE,
                               foldername = "Five Mile")












results <- calc_annual_stats(data = flow_data,
                             start_year = 1973,
                             end_year = 2010,
                             exclude_years = 1999:2001,
                             water_year = TRUE,
                             water_year_start = 10,
                             months = 7:9,
                             roll_days = 7,
                             percentiles = c(5,10,20,25),
                             ignore_missing = TRUE)


results <- screen_flow_data(data = flow_data)
results <- calc_longterm_stats(data = flow_data)
results <- calc_longterm_stats(data = flow_data, ignore_missing = TRUE)
results <- calc_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data, incl_seasons = TRUE)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)

results <- calc_daily_stats(data = flow_data, ignore_missing = TRUE)
results <- calc_daily_cumulative_stats(data = flow_data, ignore_missing = TRUE)

results <- calc_monthly_stats(data = flow_data)
results <- calc_monthly_cumulative_stats(data = flow_data)

results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data, percent_MAD = c(5,10,20))
results <- calc_lt_percentile(data = flow_data, percentiles =  50)

plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_annual_stats(data = flow_data, values = Volume_m3)
plot_annual_stats(data = flow_data, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data)
plot_daily_stats(data = flow_data)
plot_daily_stats(data = flow_data, ignore_missing = TRUE)


plot_daily_stats(data = flow_data, values = Volume_m3)
plot_daily_stats(data = flow_data, values = Yield_mm)
plot_data_screening(data = flow_data)
plot_data_screening(data = flow_data, values = Volume_m3)
plot_data_screening(data = flow_data, values = Yield_mm)
plot_flow_duration(data = flow_data, ignore_missing = TRUE)
plot_flow_duration(data = flow_data, values = Volume_m3)
plot_flow_duration(data = flow_data, values = Yield_mm)
plot_longterm_stats(data = flow_data, ignore_missing = TRUE)
plot_longterm_stats(data = flow_data, values = Volume_m3)
plot_longterm_stats(data = flow_data, values = Yield_mm)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data)
plot_monthly_stats(data = flow_data, ignore_missing = TRUE)
plot_monthly_stats(data = flow_data, values = Volume_m3)
plot_monthly_stats(data = flow_data, values = Yield_mm)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)



## Examples with a station_number


flow_data <- fill_missing_dates(station_number = "08HB048") %>% 
  add_basin_area() %>% 
  add_seasons() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield() %>% 
  add_cumulative_yield(basin_area = 10.2)

results <- calc_longterm_stats(station_number = "08HB048", ignore_missing = F)
results <- calc_annual_stats(station_number = "08HB048")
results <- calc_all_annual_stats(station_number = "08HB048")
results <- calc_annual_cumulative_stats(station_number = "08HB048", use_yield = T, incl_seasons = T)
results <- calc_annual_flow_timing(station_number = "08HB048")
results <- calc_annual_lowflows(station_number = "08HA066")
results <- calc_annual_outside_normal(station_number = "08HB048")
results <- calc_daily_stats(station_number = "08HB048", months = 6:7)
results <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1990)
results <- calc_flow_percentile(station_number = "08HB048", flow_value = 10000)
results <- calc_lt_mad(station_number = "08HB048")
results <- calc_lt_percentile(station_number = "08HB048", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08HB048")
results <- calc_monthly_stats(station_number = "08HB048")
results <- screen_flow_data(station_number = "08HA066")

plot_flow_data(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", incl_seasons = T)
plot_annual_flow_timing(station_number = "08HB048")
plot_annual_outside_normal(station_number = "08HB048")
plot_annual_stats(station_number = "08HB048", percentiles = 1:20)
plot_annual_lowflows(station_number = "08HB048")
plot_daily_cumulative_stats(station_number = "08HB048", use_yield = T, start_year = 1980)
plot_daily_stats(station_number = "08HB048", start_year = 1973)
plot_data_screening(station_number = "08HB048")
plot_flow_duration(station_number = "08HB048", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = T)
plot_longterm_stats(station_number = "08HB048", ignore_missing = T)
plot_missing_dates(station_number = "08HB048")
plot_monthly_cumulative_stats(station_number = "08HB048", use_yield = T)
plot_monthly_stats(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", use_yield = T)

## Mulitple stations

results <- calc_longterm_stats(station_number = c("08HB048","08NM116"))

plot_daily_stats(station_number = c("08HB048","08NM116"), complete_years = T, include_title = T)



# Trending

trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)


# Volume Frequency analysis


data <- compute_annual_frequencies(station_number = "08HB048", water_year = F, start_year = 1980, end_year = 2010, exclude_years = 1999)
data <- compute_hydat_peak_frequencies(station_number = "08NM116", use_max = TRUE)

# Writing

write_flow_data(station_number = c("08HB048","08NM116"))

write_full_analysis(station_number = "08HB048", 
                    start_year = 1980, 
                    end_year = 2010, 
                    exclude_years = c(1995:1997, 1999),
                    table_filetype = "xlsx",
                    plot_filetype = "png",
                    foldername = "Carn",
                    ignore_missing = TRUE)




