



### GETTING STARTED
### ----------------

## Note: this does not need to be run every session.  Just the first time its run and when need to update.
# Install packages
install.packages(c("tidyverse", "tidyhydat", "devtools"))
devtools::install_github("bcgov/fasstr", build_vignettes = TRUE)
tidyhydat::download_hydat() #then enter 1 to download





### ----------------------------------------------------------------

### R Basics
### --------

# This is a comment and will not be run in the console :)

## Get outputs from typing

3

5

"Hello World"

## Calculations

3 + 5

3 * (4 + 5)

## Assigning objects (shortcut for "<-" == Alt-minus)
## object named on left becomes whatever is on the right side of the arrow (or single equal sign)

annual_volume <- 26759981 #m3
drainage_area <- 10.3 #sqkm
runoff_mm <- annual_volume / drainage_area / 1000 # 1000 conversion to mm
runoff_mm

## Functions  ( function() )

log(10)

sqrt(4)

max(4,8,2,6,9)

?max #? before a function gives you more information


## Arguments 

?log

log(5) # positional matching important

log(5, 10)

log(x=5, base=10) 

log(base=10, x=5)

log(10,5)

# Missing dates

max(4,8,2,6,9)
max(4,8,2,6,9,NA)
max(4,8,2,6,9,NA, na.rm = FALSE)
max(4,8,2,6,9,NA, na.rm = TRUE)


## Data structures

x <- "Hello World"
class(x) # character

y <- 10
class(y) # numeric

q <- TRUE
class(q) # logical

## Comparisons for logical results

5 > 3 # >, <, >=, <=, ==, !=   (! == does not)

a <- 9
b <- 10
a == b
a != b

## Vectors - one-dimensional with a series of values of the same class

x <- c(1, 2, 3)  # c() == combine
x
x <- 1:4
x
x <- c(1:4,10,50)
x

length(x)

y <- c(TRUE, TRUE, FALSE, FALSE)

first.names <- c("Erin", "Kyle", "Natasha")

stations <- c("08HB048", "08NM116", "07EA005")


## Coersion

as.numeric("1")

as.character(1:2)

x <- c("txt", "one", "1", "1.9")
y <- as.numeric(x)
y


## Indexing / subsetting

first.names <- c("Erin", "Kyle", "Natasha") # vector of character names
first.names[1] # select first in vector
first.names[c(1,3)] # select first and third in vector
first.names[-2] # select all but the second

first.names[first.names %in% c("Erin", "Natasha")] # select names if they match names in this vector

first.names[first.names %in% "Robin"]


## Data frames - de facto data structure for most tabular data and what we use for statistics

dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
dat
dat <- read.csv("mission_creek.csv") # base R
dat
View(dat)

head(dat) # show first 6 rows
tail(dat) # show last 6 rows
dim(dat) # returns the dimensions
nrow(dat) # number of rows
ncol(dat) # number of columns
str(dat) # structure of each column
names(dat) # shows the names attribute for a data frame, which gives the column names

dat[2] # subset the second column
dat$Date # selet the Date column

write.csv(dat, file = "example_data.csv", row.names = FALSE)

# recent innovations to read and write files in Excel format
writexl::write_xlsx(dat, path = "example_data.xlsx")
readxl::read_xlsx("mission_creek.xlsx") 



## Tidyverse and Piping - working in a pipeline

# Call the packages
library(dplyr)
library(tidyr)

# Typical non-piping method (keep making changes to the same object)
# object x or data is usually the first argument in a function
dat <- read.csv("mission_creek.csv")
dat <- select(dat, Date, Value)  # select the columns
dat <- mutate(dat,  #mutate - create/format a column
              Date = as.Date(Date),  #format column as Date format
              Volume = Value * 86400, # create a colume called Volume per day
              Yield = Volume / 795 / 1000) # create columns of yield per day
dat <- filter(dat, Yield >= 8)  # filter a column by specified 

# Piping method (changes to object contained in one line)
# %>% shortcut == Shift+Ctrl+m
dat <- read.csv("mission_creek.csv") %>% 
  select(Date, Value) %>% 
  mutate(Date = as.Date(Date),
         Volume = Value * 86400,
         Yield = Volume / 795 / 1000) %>% 
  filter(Yield >= 8)

## Basic Plotting

library(ggplot2)

dat <- read.csv("mission_creek.csv") %>% 
  mutate(Date = as.Date(Date))

plot <- ggplot(data = dat, aes(x = Date, y = Value)) + # create the object and set the data sources
  geom_line() + # add lines
  geom_point(colour = "blue") + #add points
  ylab("Discharge (cms)") + # add a y-axis label
  ggtitle("Mean Daily Flows - Mission Creek") # add a title
plot


### ----------------------------------------------------------------

### tidyhydat
### ---------

library(tidyhydat)

# tidyhydat introduction
vignette("tidyhydat_an_introduction", package = "tidyhydat")
?tidyhydat


# Check the HYDAT version
hy_version()

# Daily data
?hy_daily_flows

dat <- hy_daily_flows(station_number = "08NJ168") # Five mile creek above city intake
dat

# Multiple stations - make a vector!!
dat <- hy_daily_flows(station_number = c("08NJ168", "08NJ013")) #multiple stations -- slocan river near crescent valley
dat

stations_list <- c("08NJ168", "08NJ013")
dat <- hy_daily_flows(station_number = stations_list) #multiple stations -- slocan river near crescent valley
dat

hy_plot(station_number = "08NJ168") # plot the dataset

hy_annual_stats(station_number = "08NJ168") # get annual stats out of HYDAT

# Real-time data
rt <- realtime_dd(station_number = "08NJ013") # get realtime data
rt
rt_daily <- realtime_daily_mean(rt) # create daily averages of realtime data
rt_daily

realtime_plot(station_number = "08NJ013") # plot realtime data

# Meta-data
meta <- hy_stations()

## Example workflow
## Get daily flows of all active 08NE stations

# search for all stations and pull the columns into a vector called stn_list
stn_list <- search_stn_number("08NE") %>% 
  pull(STATION_NUMBER)

# Get metadata for only those stations, filter for ACTIVE stations, and pull the list again
stn_list <- hy_stations(station_number = stn_list) %>% 
  filter(HYD_STATUS == "ACTIVE") %>% 
  pull(STATION_NUMBER)







### ----------------------------------------------------------------

### fasstr
### ------

library(fasstr)

# fasstr user guide
vignette("fasstr_users_guide", package = "fasstr")
?fasstr



# Or call specific function from a specific package without loading it from the library()
fasstr::calc_annual_stats(station_number = "08NJ168")


## Data sourcing - station_number or data arguments

# 1) station_number argument
stats <- calc_annual_stats(station_number = "08NJ168")

# 2) data argument (your own data or previously pulled from HYDAT)
flow_data <- hy_daily_flows(station_number = "08NJ168")
flow_data <- read.csv("mission_creek.csv")

stats <- calc_annual_stats(data = flow_data,
                           dates = Date, # can specify the column name
                           values = Value) # can specify the column name
stats <- calc_annual_stats(data = flow_data) # or dont specify column names if they are the defaults as above

stats <- hy_daily_flows(station_number = "08NJ168") %>% # pipeline!
  calc_annual_stats()

# Multiple Stations
# calc_ creates data frames
stats <- calc_annual_stats(station_number = c("08NJ013", "08NJ168"))

# plots_ and computes_ create lists of objects (plots and tables)
plot_annual_stats(station_number = "08NJ168")
plot_annual_stats(station_number = c("08NJ013", "08NJ168"))

plot_annual_stats(station_number = c("08NJ013", "08NJ168"))[1]



## Data cleaning functions (adding rows and columns)

# Lets view data first to know what we're working with
plot_flow_data(station_number = "08NJ013")

# No spacing between gaps - fill_missing_dates with NA
flow_data <- hy_daily_flows(station_number = "08NJ013") %>% 
  fill_missing_dates()

# All sorts of cleaning functions
flow_data <- hy_daily_flows(station_number = "08NJ013") %>% 
  fill_missing_dates() %>% # fills date gaps with NA
  add_date_variables(water_year = TRUE) %>% # adds date columns
  add_seasons() %>% # adds seasons
  add_rolling_means(roll_days = 7) %>% # adds rolling means
  add_basin_area() %>% # adds basin area - leave blank if in HYDAT
  add_basin_area() %>% # adds basin area - basin_area if not in HYDAT
  add_daily_volume() %>% # adds a column of volume per day in cubic metres
  add_daily_yield() %>% # adds a column of yield per day in millimtres
  add_daily_yield() %>% # adds a column of yield per day in millimtres
  add_cumulative_volume() %>% # adds a column of annual cumulative volume per day in cubic metres
  add_cumulative_yield() # adds a column of annual cumulative yield per day in millimtres


## Customizing Functions with Arguments

# Basic
stats <- calc_annual_stats(station_number = "08NJ168")

# Some of the arguments
stats <- calc_annual_stats(station_number = "08NJ168",
                           start_year = 1990,
                           end_year = 2000,
                           exclude_years = 1992:1993,
                           water_year = TRUE,
                           roll_days = 7,
                           percentiles = c(5,10,15,25,75))

# See help for all arguments for a function
?calc_annual_stats

# All options and defaults for this function
stats <- calc_annual_stats(data = NULL, # a data frame
                           dates = Date, # a column in the data frame
                           values = Value, # a column in the data frame
                           groups = STATION_NUMBER,  # a column in the data frame
                           station_number = "08NJ168", # a single or vector of station numbers
                           roll_days = 1, # a single number
                           roll_align = "right", # character "left" "center" or "right"
                           percentiles = c(10, 90),  # a single to vector of numbers
                           water_year = FALSE, # logical value - TRUE or FALSE
                           water_year_start = 10, # a single number
                           start_year = 0, # a single number
                           end_year = 9999, # a single number
                           exclude_years = NULL, # leave NULL or vector of numbers (years)
                           months = 1:12, # vector of numbers between 1 and 12
                           transpose = FALSE, # logical value - TRUE or FALSE
                           ignore_missing = FALSE # logical value - TRUE or FALSE
                           )

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

# Screen for outliers and missing dates
screening <- screen_flow_data(station_number = "08NJ168")

# plot the data for outliers
plot_data_screening(station_number = "08NJ168")

# plot the data for data availability
plot_missing_dates(station_number = "08NJ168")

# Very gappy
plot_missing_dates(station_number = "08NM116")
plot_missing_dates(station_number = "08NM116",
                   start_year = 1980)

# plotting the daily data and comparing
plot_flow_data(station_number = "08NJ168")

# Plotting two stations will automatically save as separate
plot_flow_data(station_number = c("08NJ168","08NJ013"))

# use one_plot argument to plot on same (only this function)
plot_flow_data(station_number = c("08NJ168","08NJ013"),
               one_plot = TRUE)

## Example - station yield comparison

# Get the daily data and add the yield runoff column
data <- hy_daily_flows(station_number = c("08NP003","08NP004")) %>% 
  add_daily_yield()
data

# PLot the data with discharge (separate and one plot)
plot_flow_data(data = data)
plot_flow_data(data = data,
               one_plot = TRUE)

# Choose the values argument - Yield_mm to plot with select years that match
plot_flow_data(data = data, 
               values = Yield_mm, 
               start_year = 1978, 
               end_year = 1995,
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
stats <- calc_daily_stats(station_number = "08NJ168",
                          start_year = 1990,
                          percentiles = c(5,10,20,25,75))


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

# Example of daily yield

# tables of data
stats <- add_daily_yield(station_number = "08NJ168") %>% 
  calc_daily_stats(ignore_missing = TRUE)

#plot of data - just discharge
daily <- add_daily_yield(station_number = "08NJ168") %>% 
  plot_daily_stats(ignore_missing = TRUE)
daily

# specify the values to plot in yield
daily <- add_daily_yield(station_number = "08NJ168") %>% 
  plot_daily_stats(values = Yield_mm,
                   ignore_missing = TRUE)
daily

# Save with ggplot2::ggsave() by subsetting plot from the list
ggsave("daily_runoff.png", daily[[1]])
ggsave("daily_runoff.png", daily$Daily_Stats)

# or use the fasstr::write_plots() - creates a folder and saves the name as the object name
write_plots(plots = daily, foldername = "Test folder", plot_type = "png")

# specify the size
write_plots(plots = daily, foldername = "Test folder", plot_type = "png",
            width = 10, height = 4)

## Cumulative Stats (volumetric or yield)

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
                            use_yield = TRUE),
                            start_year = 1984)

## Annual stats

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






## Function Examples

flow_data <- fill_missing_dates(station_number = "08NM116") 
flow_data <- add_basin_area(station_number = "08NM116")
flow_data <- add_seasons(station_number = "08NM116")
flow_data <- add_date_variables(station_number = "08NM116", water_year = T)
flow_data <- add_rolling_means(station_number = "08NM116")
flow_data <- add_daily_volume(station_number = "08NM116")
flow_data <- add_cumulative_volume(station_number = "08NM116")
flow_data <- add_daily_yield(station_number = "08NM116")
flow_data <- add_cumulative_yield(station_number = "08NM116", basin_area = 10.2)

results <- calc_longterm_stats(station_number = "08NM116", ignore_missing = T)
results <- calc_annual_stats(station_number = "08NM116")
results <- calc_all_annual_stats(station_number = "08NM116")
results <- calc_annual_cumulative_stats(station_number = "08NM116", use_yield = T, include_seasons = T)
results <- calc_annual_flow_timing(station_number = "08NM116")
results <- calc_annual_lowflows(station_number = "08NM116")
results <- calc_annual_outside_normal(station_number = "08NM116")
results <- calc_daily_stats(station_number = "08NM116", ignore_missing = TRUE)
results <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1990)
results <- calc_flow_percentile(station_number = "08NM116", flow_value = 10)
results <- calc_lt_mad(station_number = "08NM116", percent_MAD = c(5,10,20))
results <- calc_lt_percentile(station_number = "08NM116", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08NM116")
results <- calc_monthly_stats(station_number = "08NM116")
results <- screen_flow_data(station_number = "08NM116", months = 7:9)

plot_flow_data(station_number = "08NM116")
plot_annual_cumulative_stats(station_number = "08NM116", include_seasons = T)
plot_annual_flow_timing(station_number = "08NM116")
plot_annual_outside_normal(station_number = "08NM116")
plot_annual_stats(station_number = "08NM116")
plot_annual_lowflows(station_number = "08NM116")
plot_daily_cumulative_stats(station_number = "08NM116", use_yield = T, start_year = 1980)
plot_daily_stats(station_number = "08NM116", start_year = 1973)
plot_data_screening(station_number = "08NM116")
plot_flow_duration(station_number = "08NM116", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = T)
plot_longterm_stats(station_number = "08NM116", ignore_missing = T)
plot_missing_dates(station_number = "08NM116")
plot_monthly_cumulative_stats(station_number = "08NM116", use_yield = T)
plot_monthly_stats(station_number = "08NM116")

trends <- compute_annual_trends(station_number = "08NM116", zyp_method = "yuepilon", zyp_alpha = 0.05)
trends$Annual_Trends_Data
trends$Annual_Trends_Results
trends$Min_1_Day_DoY

frequency <- compute_annual_frequencies(station_number = "08NM116", start_year = 1974)
frequency$Q_stat
frequency$plotdata
frequency$freqplot
frequency$fit
frequency$fitted_quantiles

everything <- compute_full_analysis(station_number = "08NM116", start_year = 1974,
                                    write_to_dir = TRUE, foldername = "Mission Creek")
# play around with the $ sign to get the data, or the folders
