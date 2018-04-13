# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.





### Basic fasstr use
### ================


# These provide some example on how to use fasstr




# Install fasstr from github (and devtools if not to do so)
# --------------------------

# Note: it may take few moments as there are lots of packages to download with it

if (!"devtools" %in% row.names(installed.packages())) { install.packages("devtools") }
devtools::install_github("bcgov/fasstr")

library(fasstr)
library(dplyr)
library(ggplot2)



# ------------------
# Basic data inputs
# -----------------

# Two options:

# 1) A data frame of data (from tidyhydat or read.csv) with columns of dates, values (flows), and groups (station numbers; optional)
#    By default the functions look for columns named "Date", "Value", and (optional) "STATION_NUMBER", so no need to list them
#    unless you have different column names (see examples).

    # Example of default columns:

    flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
    head(flow_data) # Example of column names and first few rows of data
    
    calc_longterm_stats(data = flow_data, 
                        ignore_missing = TRUE) # ignore_missing means data is calculated regardless of missing dates

    # Example of different column names

    flow_data2 <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
      dplyr::rename(Days = Date, Flows = Value, Stations = STATION_NUMBER) # Rename the columns for this example
    head(flow_data2)
    
    calc_longterm_stats(data = flow_data2, 
                        dates = Days,
                        values = Flows,
                        groups = Stations,
                        ignore_missing = TRUE)

    # If you dont list groups argument, and STATION_NUMBER doesnt exist in your data (i.e. you have your own data
    # that doesnt have a station number) then it wont group any of the data by the groups (stations)
    # You'll see the station colum is missing below

    calc_longterm_stats(data = flow_data2, 
                        dates = Days,
                        values = Flows,
                        ignore_missing = TRUE)

# 2) You can list a HYDAT hydrometric station number using the station_number argument, and can ignore the data,
#   dates, values, and groups arguments entirely

    calc_longterm_stats(station_number = "08NM116",
                        ignore_missing = TRUE)

    # You can list multiple stations to get multiple data set summaries (also with using the data and groups options as above)
    calc_longterm_stats(station_number = c("08NM116", "08NM242"),
                        ignore_missing = TRUE)

    # If having troubles with data entering, you can try looking for help in the documentation:
    ?calc_longterm_stats


    
# ------------------------
# Data tidying and options
# ------------------------

# Some functions provided add columns/rows of useful data to your daily data so you can summarize your data

# Filling missing dates:
    # Use fill_missing_dates() to fill any days with missing flow values with NA. Compare the two to see the differences:

    # Very gappy:
    tidyhydat::hy_daily_flows(station_number = "08NM116")

    # Gap filled with NA (one line with station_number)
    fill_missing_dates(station_number = "08NM116")

    # Gap filled with NA (using data in tidy method)
    tidyhydat::hy_daily_flows(station_number = "08NM116") %>% 
      fill_missing_dates()  # when using pipes, the pipes assume you are moving data to the next line
    
# Add date variables (years, months, days)
    
    # Just calendar year info
    add_date_variables(station_number = "08NM116")
    
    # If water years are require (default starting october)
    add_date_variables(station_number = "08NM116", water_year = TRUE)
    
    # If water years are require starting August (month number)
    add_date_variables(station_number = "08NM116", water_year = TRUE, water_year_start = 8)
    
# Add seasons
    # Adds two columns: one with 2 6-month seasons categories, and one with 4 3-month seasons
    # The timing of the seasons starts with the start month (water_year = T or D, and month)
    
    #  Seasons starting January
    add_seasons(station_number = "08NM116")
    
    #  Seasons starting October
    add_seasons(station_number = "08NM116", water_year = TRUE)
    
    #  Seasons starting December
    add_seasons(station_number = "08NM116", water_year = TRUE, water_year_start = 12)
    
# Add rolling mean days
    
    # Defaults to adding 3, 7, and 30 day rolling means
    add_rolling_means(station_number = "08NM116")
    
    # Just 7 day rolling means added (default "right" aligned, mean the date of the rolling mean is the last day)
    add_rolling_means(station_number = "08NM116", roll_days = 7)
    
    # Just 7 day rolling means added with "left" aligned (date is the first day of all 7 days)
    add_rolling_means(station_number = "08NM116", roll_days = 7, roll_align = "left")
    
# Add basin area column

    # If you have a column with HYDAT station number (with station_number or data/groups arguments) then it
    # will automatically look up the number
    add_basin_area(station_number = "08NM116")
    
    # If no basin area provided or you want to use a different one provided, sue the basin_area argument
    add_basin_area(station_number = "08NM116", basin_area = 800)
    
    # If you have multiple stations you need to apply basin_areas to, use the basin_area argument as a list
    # and name each station
    add_basin_area(station_number = c("08NM116", "08NM242"), basin_area = c("08NM116" = 800, "08NM242" = 4))
    
# Add daily volume (in cubic metres)
    # converts daily discharge into volume
    
    add_daily_volume(station_number = "08NM116")    
    
# Add cumulative daily volumes for each year  (in cubic metres)
    # Converts daily discharge to volume, then sums all preceding days for each year (if complete year of data)
    
    add_cumulative_volume(station_number = "08NM116") 
    
# Add daily yield (in millimetres)
    # converts daily discharge into mm depth using a basin area (see add_basin_area() for basin_area options)
    
    # If HYDAT station number listed or is in groups colums
    add_daily_yield(station_number = "08NM116")   
    
    # If no basin area provided or custom basin area
    add_daily_yield(station_number = "08NM116", basin_area = 800)   
    
    
# Add cumulative daily volumes for each year (in millimetres) 
    # Converts daily discharge into mm depth using a basin area, then sums all preceding days for each year (if complete year of data)
    
    # If HYDAT station number listed or is in groups colums
    add_cumulative_yield(station_number = "08NM116")   
    
    # If no basin area provided or custom basin area
    add_cumulative_yield(station_number = "08NM116", basin_area = 800)  
    

# Using the pipe operator and tidy method, you can add all the columns and fill missing dates if you wish
    # Note, it is best to fill missing dates before other add_ functions to fill dates properly

    # One station with Date and Value
    fill_missing_dates(station_number = "08NM116") %>% 
      fill_missing_dates() %>% 
      add_basin_area() %>% 
      add_date_variables(water_year = TRUE) %>%
      add_rolling_means() %>%
      add_daily_volume() %>%
      add_cumulative_volume() %>% 
      add_daily_yield() %>%
      add_cumulative_yield() %>% 
      add_seasons()
    
    # Multiple stations and different column names
    tidyhydat::hy_daily_flows(station_number = c("08NM242","08NM116")) %>% 
      dplyr::rename(Days = Date, Flows = Value) %>% # rename the columns
      fill_missing_dates(dates = Days, values = Flows) %>% 
      add_basin_area() %>% 
      add_date_variables(dates = Days) %>% 
      add_rolling_means(dates = Days, values = Flows) %>% 
      add_daily_volume(values = Flows) %>% 
      add_cumulative_volume(dates = Days, values = Flows) %>% 
      add_daily_yield(values = Flows) %>%
      add_cumulative_yield(dates = Days, values = Flows) %>% 
      add_seasons(dates = Days)
    
    # No groups columns (i.e. no STATION_NUMBER) (no STATION_NUMBER column)
   tidyhydat::hy_daily_flows(station_number = "08NM116") %>% 
      dplyr::select(Date, Value) %>% # select just dates and values
      fill_missing_dates() %>%
      add_basin_area(basin_area = 10.555) %>%
      add_date_variables(water_year = T) %>%
      add_rolling_means() %>%
      add_daily_volume() %>%
      add_cumulative_volume() %>% 
      add_daily_yield(basin_area = 10.3) %>%
      add_cumulative_yield(basin_area = 10.3) %>% 
      add_seasons()
   
   
# ------------------------
# Data screening
# ------------------------   
   
# To look for outliers and missing data to see which years of data you need, there are a few data and plotting functions
   
   # Screening tells you the amount of days and data per year and each month per year
   # and some basic annual stats to look for outliers
   screen_flow_data(station_number = "08NM116")

   # This function plots the annual metrics for outliers and patterns
   plot_data_screening(station_number = "08NM116")
   
   # This function plots the monthly missing days for each month for each year
   plot_missing_dates(station_number = "08NM116")
   
   
   
# --------------------------------------------------
# Calcuation and plotting function arguments/options
# --------------------------------------------------
   
# Notes about missing dates:
# --------------

  # When there are missing dates, most functions will automatically not calculate a statistic for that period
  # You will get a warning if there are missing dates/NAs in the resulting tabl
   
   # Example - only years with no missing dates had stats
   calc_annual_stats(station_number = "08NM116")
   
   # If you want to calculate the stats regardless of missing dates, use the ignore_missing = TRUE argument (default FALSE)
   calc_annual_stats(station_number = "08NM116", ignore_missing = TRUE)
   
   # Alternative, some functions have a complete_years function where it will calculate this only from full years of data
   calc_longterm_stats(station_number = "08NM116")
   calc_longterm_stats(station_number = "08NM116", complete_years = TRUE)
   calc_longterm_stats(station_number = "08NM116", ignore_missing = TRUE) # this calcs all years, not just complete years
   
   # Some functions only use complete year (analysis only works with fill years), so years with NA will be ignored
   calc_annual_flow_timing(station_number = "08NM116")
   
# Filtering for years and months
# ----------
   # If you have missing data in certain years or you have a specific period to analyze, you can filter using several options:

   # Start and end years
    calc_annual_stats(station_number = "08NM116", 
                     start_year = 1980, 
                     end_year = 2010)
    
    # Removing certain years (outliers, bad data, etc) using exclude_years
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, 
                      end_year = 2010,
                      exclude_years = 1982)
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, 
                      end_year = 2010,
                      exclude_years = c(1982:1984))
    
    # Some functions allow you use data from selected months
    # This example shows the annual stats for just June-August (Default 1:12)
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      months = 6:8)
    
# Water Year
# ----------
    
    # To have years starting in October
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      water_year = TRUE)
    # To have years starting in August
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      water_year = TRUE,
                      water_year_start = 8)
    
# Rolling Day and align
# ----------
    
    # Some functions (mostly calc_***_stats() functions) allow you to analyze rolling mean days instead of 
    # the daily mean (roll_days default is 1)
    
    # This calculates the annual 7-day rolling mean statistics
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      roll_days = 7)
    plot_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      roll_days = 7)
    
    # This calculates the annual 7-day rolling mean statistics with align left (see add_rolling_days() above)
    calc_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      roll_days = 7,
                      roll_align = "left")
    plot_annual_stats(station_number = "08NM116", 
                      start_year = 1980, end_year = 2010,
                      roll_days = 7,
                      roll_align = "left")
    

# Percentiles and other stats
# ----------
    
    # Functions come with their default settings for what is calculated and they can be changed with argument setting.
    # Look at the help files to see default and arguments
    
    # calc_annual_stats defauls to Mean, Meidan, Maximum, and Minimum stats with 10th and 90th percentiles
    # you can change just the percentiles using the argument
        calc_annual_stats(station_number = "08NM116", 
                          start_year = 1980, end_year = 2010,
                          percentiles = c(1:5,20,30))
        plot_annual_stats(station_number = "08NM116", 
                          start_year = 1980, end_year = 2010,
                          percentiles = c(60,70,80))
    
    # For annual flow timing, you can change the percent of total annual flow you want to know the date of:
        # Default 25, 33.3, 50, and 75 percent of total flow
        calc_annual_flow_timing(station_number = "08NM116", 
                                start_year = 1980, end_year = 2010)
        # Just timing of half flow
        calc_annual_flow_timing(station_number = "08NM116", 
                                start_year = 1980, end_year = 2010, 
                                percent_total = 50)
        
# CUmulative functions
# ----------
        
    # These default to total daily volume flows in their cumulative calculations, see:
        calc_daily_cumulative_stats(station_number = "08NM116",
                                    start_year = 1980, end_year = 2010)
        plot_daily_cumulative_stats(station_number = "08NM116",
                                    start_year = 1980, end_year = 2010)
    # But using the use_yield function, and a basin area (see above for options) you and use yield
        calc_daily_cumulative_stats(station_number = "08NM116",
                                    start_year = 1980, end_year = 2010,
                                    use_yield = TRUE)
        plot_daily_cumulative_stats(station_number = "08NM116",
                                    start_year = 1980, end_year = 2010,
                                    use_yield = TRUE)
        
# Volume_m3 or Yield_mm as values argument for plotting
# ----------
        
    # Plots with a discharge y-axis default to units of cubic metres per second, if your data has added
    # columns of daily yield or volume, most axes will adjust accordingly:
        # Volume and annual stats
        add_daily_volume(station_number = "08HB048") %>% 
          plot_annual_stats(values = Volume_m3,
                            start_year = 1980, end_year = 2010)
        # Yield and monthly stats
        add_daily_yield(station_number = "08HB048") %>% 
          plot_monthly_stats(values = Yield_mm,
                             start_year = 1980, end_year = 2010)
        
# Transposing your data tibbles
# ----------
        
    # If you want your data set to have the statistics in rows and the years as columns, 
    # most functions havea tranpose argument
        calc_annual_stats(station_number = "08NM116", 
                          start_year = 1980, end_year = 2010,
                          transpose = TRUE)
        
# Discharge on logarithmic scale for your plots
# ----------
        
        # Normal scale:
        plot_annual_stats(station_number = "08NM116", 
                          start_year = 1980, end_year = 2010)
        # Normal scale:
        plot_annual_stats(station_number = "08NM116", 
                          start_year = 1980, end_year = 2010,
                          log_discharge = TRUE)
    
# Multiple stations and plotting
# ----------
        
    # If multiple groups/stations are provided, then a plot will be created for each station
        
        # One station example:
        plot_daily_stats(station_number = "08NM116", complete_years = TRUE)
        
        # Multiple stations example:
        plot_daily_stats(station_number = c("08NM116","08HB048", "07EA005"), complete_years = TRUE)
        
    # As there are no titles, you can choose to add the station ID to each plot (they are plotted in the order 
    # they are printed in the console)
        plot_daily_stats(station_number = c("08NM116","08HB048", "07EA005"), complete_years = TRUE,
                         include_title = TRUE)

# Adding a year to plot
# ----------
        
    #  Some plotting functions provide the ability to plot indiviual years on plots where stats from all years are summarize
    #  (ex. plot_daily_stats, plot_daily_cumulative stats, plot_monthly_cumulative_stats)
        plot_daily_stats(station_number = "08NM116", complete_years = TRUE,
                         include_year = 2010)
        
        
        
# --------------------------------------------------
# Writing functions
# --------------------------------------------------

# Writing daily data
    # You can directly save a dataset of flow data to you computer as an Excel or CSV file using the following function
    # This function allows for most filtering options as well. If file anem is provided a default is provided and printed in the concoles
    
    write_flow_data(station_number = "08NM116",
                    start_year = 1980, end_year = 2010)
        
# Writing a data frame and rounding in the function

        results <- calc_longterm_stats(station_number = "08NM116", complete_years = TRUE)
        write_results(data = results,
                      file = "Mission Creek Long-term Stats.xlsx",
                      digits = 3)
        
# Saving plots to your computer
    # as all plots in fasstr are produced as lists, this function makes it easier to save them
    # This function takes your list of plots and saves them in a named folder (named by the plot name) or combined PDF document
    # The function also uses options of height, width, and dpi to that of ggplot2::ggsave
        
        monthly_plots <- plot_monthly_stats(station_number = "08NM116", start_year = 1985,
                                            include_title = TRUE)
        
        write_plots(plots = monthly_plots,
                    foldername = "Mission Creek Monthly Stats",
                    type = "png")
        
        write_plots(plots = monthly_plots,
                    foldername = "Mission Creek Monthly Stats",
                    combined_pdf = TRUE)
        
        
        
        
# --------------------------------------------------
# Trending functions
# --------------------------------------------------

# The compute_annual_trends function calculates non-parametric trends (Mann-Kendall, Sen's slope methods)
# using the zyp package (see methods https://cran.r-project.org/web/packages/zyp/index.html) 

# the compute function calculates all the metrics for the analysis, plus some summary statistics
# and all the annual data from all the annual_stats functions
# a zyp_method "yuepilon" or "zhang" is required (see zyp methods)
trending <- compute_annual_trends(station_number = "08NM116", zyp_method = "yuepilon", start_year = 1981)

  # All the trends can be plotted using there compute_ tibble, and a line can be added to the trends if
  # if you decide a certain alpha significance.  A plot for each metric and station will be created, so it 
  # may take some time to plot all plots
  plots <- plot_annual_trends(trending, zyp_alpha = 0.05)




# --------------------------------------------------
# Frequency analysis
# --------------------------------------------------
  
# This follows the same methods as HEC-SSP.  See the help file for more info.
# A complete guide to this function will be built.
  
?compute_annual_frequencies
  
freq_data <- compute_annual_frequencies(station_number = "08HB048", water_year = T)
freq_data <- compute_hydat_peak_frequencies(station_number = "08NM116")

freq_data <- compute_annual_frequencies(station_number = "08NM116")
freq_data <- compute_frequency_stat(station_number = "08NM116", roll_day = 7, return_period = 10)

        
   
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

   
# --------------------------------------------------
# fasstr functions and examples
# --------------------------------------------------
   
   

# One station and data
# --------
        
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()

results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data)
results <- calc_monthly_stats(data = flow_data)
results <- screen_flow_data(data = flow_data)

plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_annual_stats(data = flow_data, values = Volume_m3)
plot_annual_stats(data = flow_data, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data)
plot_daily_stats(data = flow_data)
plot_daily_stats(data = flow_data, values = Volume_m3)
plot_daily_stats(data = flow_data, values = Yield_mm)
plot_data_screening(data = flow_data)
plot_data_screening(data = flow_data, values = Volume_m3)
plot_data_screening(data = flow_data, values = Yield_mm)
plot_flow_duration(data = flow_data)
plot_flow_duration(data = flow_data, values = Volume_m3)
plot_flow_duration(data = flow_data, values = Yield_mm)
plot_longterm_stats(data = flow_data)
plot_longterm_stats(data = flow_data, values = Volume_m3)
plot_longterm_stats(data = flow_data, values = Yield_mm)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data)
plot_monthly_stats(data = flow_data)
plot_monthly_stats(data = flow_data, values = Volume_m3)
plot_monthly_stats(data = flow_data, values = Yield_mm)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)


# Multiple stations and data, and renamed dates and values
# --------

flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Days = Date, Flows = Value) %>% 
  fill_missing_dates(dates = Days, values = Flows) %>% 
  add_basin_area() %>% 
  add_date_variables(dates = Days) %>% 
  add_rolling_means(dates = Days, values = Flows) %>% 
  add_daily_volume(values = Flows) %>% 
  add_cumulative_volume(dates = Days, values = Flows) %>% 
  add_daily_yield(values = Flows) %>%
  add_cumulative_yield(dates = Days, values = Flows) %>% 
  add_seasons(dates = Days)

results <- calc_longterm_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_annual_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_all_annual_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_annual_cumulative_stats(data = flow_data, dates = Days, values = Flows, incl_seasons = T)
results <- calc_annual_flow_timing(data = flow_data, dates = Days, values = Flows)
results <- calc_annual_lowflows(data = flow_data, dates = Days, values = Flows)
results <- calc_annual_outside_normal(data = flow_data, dates = Days, values = Flows)
results <- calc_daily_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_daily_cumulative_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801, dates = Days, values = Flows)
results <- calc_lt_mad(data = flow_data, dates = Days, values = Flows)
results <- calc_lt_percentile(data = flow_data, percentiles =  50, dates = Days, values = Flows)
results <- calc_monthly_cumulative_stats(data = flow_data, dates = Days, values = Flows)
results <- calc_monthly_stats(data = flow_data, dates = Days, values = Flows)
results <- screen_flow_data(data = flow_data, dates = Days, values = Flows)

plot_flow_data(data = flow_data, dates = Days, values = Flows)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T, dates = Days, values = Flows)
plot_annual_flow_timing(data = flow_data, dates = Days, values = Flows)
plot_annual_outside_normal(data = flow_data, dates = Days, values = Flows)
plot_annual_stats(data = flow_data, dates = Days, values = Flows)
plot_annual_stats(data = flow_data, dates = Days, values = Volume_m3)
plot_annual_stats(data = flow_data, dates = Days, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data, dates = Days, values = Flows)
plot_daily_stats(data = flow_data, dates = Days, values = Flows)
plot_daily_stats(data = flow_data, dates = Days, values = Volume_m3)
plot_daily_stats(data = flow_data, dates = Days, values = Yield_mm)
plot_data_screening(data = flow_data, dates = Days, values = Flows)
plot_data_screening(data = flow_data, dates = Days, values = Volume_m3)
plot_data_screening(data = flow_data, dates = Days, values = Yield_mm)
plot_flow_duration(data = flow_data, dates = Days, values = Flows)
plot_flow_duration(data = flow_data, dates = Days, values = Volume_m3)
plot_flow_duration(data = flow_data, dates = Days, values = Yield_mm)
plot_longterm_stats(data = flow_data, dates = Days, values = Flows)
plot_longterm_stats(data = flow_data, dates = Days, values = Volume_m3)
plot_longterm_stats(data = flow_data, dates = Days, values = Yield_mm)
plot_missing_dates(data = flow_data, dates = Days, values = Flows)
plot_monthly_cumulative_stats(data = flow_data, dates = Days, values = Flows)
plot_monthly_stats(data = flow_data, dates = Days, values = Flows)
plot_monthly_stats(data = flow_data, dates = Days, values = Volume_m3)
plot_monthly_stats(data = flow_data, dates = Days, values = Yield_mm)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon",dates = Days, values = Flows)
trending_plots <- plot_annual_trends(trending)


# One station and data, with no groups/stations column 
# --------

flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  fill_missing_dates() %>%
  add_basin_area(basin_area = 10.555) %>%
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()

results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_monthly_stats(data = flow_data)
results <- screen_flow_data(data = flow_data)

plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T, use_yield = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_daily_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_daily_stats(data = flow_data, include_year = 1990, complete_years = T, include_title = T)
plot_data_screening(data = flow_data)
plot_flow_duration(data = flow_data)
plot_longterm_stats(data = flow_data)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_monthly_stats(data = flow_data)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)




### HYDAT
### ----------

# One station and HYDAT
# -------- 

flow_data <- fill_missing_dates(station_number = "08HB048")
flow_data <- add_basin_area(station_number = "08HB048")
flow_data <- add_seasons(station_number = "08HB048")
flow_data <- add_date_variables(station_number = "08HB048", water_year = T)
flow_data <- add_rolling_means(station_number = "08HB048")
flow_data <- add_daily_volume(station_number = "08HB048")
flow_data <- add_cumulative_volume(station_number = "08HB048")
flow_data <- add_daily_yield(station_number = "08HB048")
flow_data <- add_cumulative_yield(station_number = "08HB048", basin_area = 10.2)

results <- calc_longterm_stats(station_number = "08HB048", ignore_missing = F)
results <- calc_annual_stats(station_number = "08HB048")
results <- calc_all_annual_stats(station_number = "08HB048")
results <- calc_annual_cumulative_stats(station_number = "08HB048", use_yield = T, incl_seasons = T)
results <- calc_annual_flow_timing(station_number = "08HB048")
results <- calc_annual_lowflows(station_number = "08HB048")
results <- calc_annual_outside_normal(station_number = "08HB048")
results <- calc_daily_stats(station_number = "08HB048", months = 6:7)
results <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1990)
results <- calc_flow_percentile(station_number = "08HB048", flow_value = 10000)
results <- calc_lt_mad(station_number = "08HB048")
results <- calc_lt_percentile(station_number = "08HB048", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08HB048")
results <- calc_monthly_stats(station_number = "08HB048")
results <- screen_flow_data(station_number = "08HB048")

plot_flow_data(station_number = "08HB048", exclude_years = 2000)
plot_annual_cumulative_stats(station_number = "08HB048", incl_seasons = T)
plot_annual_flow_timing(station_number = "08HB048")
plot_annual_outside_normal(station_number = "08HB048")
plot_annual_stats(station_number = "08HB048")
plot_annual_lowflows(station_number = "08HB048")
plot_daily_cumulative_stats(station_number = "08HB048")
plot_daily_stats(station_number = "08HB048", include_year = 1999)
plot_data_screening(station_number = "08HB048")
plot_flow_duration(station_number = "08HB048", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = T)
plot_longterm_stats(station_number = "08HB048")
plot_missing_dates(station_number = "08HB048")
plot_monthly_cumulative_stats(station_number = "08HB048")
plot_monthly_stats(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", use_yield = T)

trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)



# Multiple stations and HYDAT
# -------- 

flow_data <- fill_missing_dates(station_number = c("08HB048","08NM116"))
flow_data <- add_basin_area(station_number = c("08HB048","08NM116"))
flow_data <- add_seasons(station_number = c("08HB048","08NM116"))
flow_data <- add_date_variables(station_number = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_yield(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(station_number = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))

results <- calc_longterm_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_stats(station_number = c("08HB048","08NM116"))
results <- calc_all_annual_stats(station_number = c("08HB048","08NM116"), transpose = T)
results <- calc_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_flow_timing(station_number = c("08HB048","08NM116"))
results <- calc_annual_lowflows(station_number = c("08HB048","08NM116"))
results <- calc_annual_outside_normal(station_number = c("08HB048","08NM116"))
results <- calc_daily_stats(station_number = c("08HB048","08NM116"))
results <- calc_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_flow_percentile(station_number = c("08HB048","08NM116"), flow_value = 10000)
results <- calc_lt_mad(station_number = c("08HB048","08NM116"))
results <- calc_lt_percentile(station_number = c("08HB048","08NM116"), percentiles = 50)
results <- calc_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_monthly_stats(station_number = c("08HB048","08NM116"))
results <- screen_flow_data(station_number = c("08HB048","08NM116"))

plot_flow_data(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_annual_flow_timing(station_number = c("08HB048","08NM116"))
plot_annual_outside_normal(station_number = c("08HB048","08NM116"))
plot_annual_stats(station_number = c("08HB048","08NM116"), include_title = T)
plot_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_daily_stats(station_number = c("08HB048","08NM116"), complete_years = T, include_title = T)
plot_data_screening(station_number = c("08HB048","08NM116"))
plot_flow_duration(station_number = c("08HB048","08NM116"))
plot_longterm_stats(station_number = c("08HB048","08NM116"))
plot_missing_dates(station_number = c("08HB048","08NM116"))
plot_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_monthly_stats(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))

trending <- compute_annual_trends(station_number = c("08HB048","08NM116"), zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)

write_flow_data(station_number = c("08HB048","08NM116"))






