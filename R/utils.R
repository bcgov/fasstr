# Copyright 2019 Province of British Columbia
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

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

## Checks on data and importing HYDAT data
## ---------------------------------------

flowdata_import <- function(data = NULL, station_number = NULL){
  
  # Check if both data and station_number are or arent't provided, must choose one
  if (is.null(station_number) & is.null(data))    
    stop("Must select one of data or station_number arguments to supply data.", call. = FALSE)
  if (!is.null(station_number) & !is.null(data))  
    stop("Must select either data or station_number arguments, not both, to supply data.", call. = FALSE)
  
  # If a station_number is provided, check if they exist in HYDAT, if they do extract the daily data
  if (is.null(data)) {
    if (!file.exists(file.path(tidyhydat::hy_dir(),"HYDAT.sqlite3")))
      stop("A HYDAT database has not been downloaded yet using the tidyhydat::download_hydat() function. 
       Download HYDAT before using station_number argument.", call. = FALSE)
    if (!is.character(station_number))  stop("station_number must be a character vector containing HYDAT station number(s).", call. = FALSE)
    station_number <- toupper(station_number) # make lower-case typos into uppercase
    if (!all(station_number %in% dplyr::pull(suppressMessages(tidyhydat::hy_stations()[1])))) 
      stop("One or more station numbers listed do not have historical daily flows in HYDAT.", call. = FALSE)
    data <- as.data.frame(suppressMessages(tidyhydat::hy_daily_flows(station_number =  station_number)))
    
  # If data is provided, make sure it's a data frame
  } else {
    if (!is.data.frame(data))  stop("data argument is not a data frame.", call. = FALSE)
    data <- as.data.frame(data)
  }
  
  # Convert Parameter and Symbol columns (if they exist) to characters
  if ("Parameter" %in% names(data)) {
    data$Parameter <- as.character(data$Parameter)
  }
  if ("Symbol" %in% names(data)) {
    data$Symbol <- as.character(data$Symbol)
  }
  
  data
  
}



## Check for dates and proper formatting
## -------------------------------------

format_dates_col <- function(data,
                             dates = "Date"){
  
  # Check if column exists
  if (!dates %in% names(data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.", call. = FALSE)
  
  # Rename values to "Date" (and change original if required so no duplication)
  if ("Date" %in% colnames(data) & dates != "Date") {
    names(data)[names(data) == "Date"] <- "Date_orig"
  }
  names(data)[names(data) == dates] <- "Date"
  
  # Check formatting
  if (!inherits(data$Date[1], "Date"))  {
    
    # Attempt to format as dates if not class "Date"
    test <- try(as.Date(data$Date, "%Y-%m-%d"), silent = TRUE)
    if ("try-error" %in% class(test)) {
      stop("Dates in dates column must be formatted as dates (YYYY-MM-DD).", call. = FALSE)
    } else {
      data$Date <- as.Date(data$Date, "%Y-%m-%d")
      if (any(is.na(data$Date)))
        stop("At least one date in dates column is not a date (YYYY-MM-DD).", call. = FALSE)
    }
  }
  
  data
}


## Check for values and proper formatting
## --------------------------------------

format_values_col <- function(data,
                              values = "Value"){
  
  # Check if column exists
  if (!values %in% names(data)) 
    stop("values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.", call. = FALSE)
  
  # Rename values to "Value" (and change original if required so no duplication)
  if ("Value" %in% colnames(data) & values != "Value") {
    names(data)[names(data) == "Value"] <- "Value_orig"
  }
  names(data)[names(data) == values] <- "Value"
  
  # Check formatting
  if (!is.numeric(data$Value))  stop("Values in values column must be numeric.", call. = FALSE)
  
  data
}


## Check for groups and proper formatting
## --------------------------------------

format_groups_col <- function(data,
                              groups = "STATION_NUMBER"){
  
  # Check if column exists
  if (groups != "STATION_NUMBER" & !groups %in% names(data)) 
    stop("Groups not found in data frame. Leave blank for no grouping, rename groups column to 'STATION_NUMBER', or identify the column using 'groups' argument.", call. = FALSE)
  
  if (!groups %in% names(data)) {
    data[, groups] <- "XXXXXXX"
  }
  
  # Rename values to "STATION_NUMBER" (and change original if required so no duplication)
  if ("STATION_NUMBER" %in% colnames(data) & groups != "STATION_NUMBER") {
    names(data)[names(data) == "STATION_NUMBER"] <- "STATION_NUMBER_orig"
  }
  
  names(data)[names(data) == groups] <- "STATION_NUMBER"
  
  # Change formaet to character
  data$STATION_NUMBER <- as.character(data$STATION_NUMBER)
  
  data
}

## Check for dates, values, and groups proper formatting
## -----------------------------------------------------

format_all_cols <- function(data,
                            dates = "Date",
                            values = "Value",
                            groups = "STATION_NUMBER",
                            rm_other_cols = FALSE){
  
  # Check format all columns
  data <- format_dates_col(data, dates = dates)
  data <- format_values_col(data, values = values)
  data <- format_groups_col(data, groups = groups)
  
  # Remove all other columns if TRUE
  if (rm_other_cols) {
    data <- dplyr::select(data, STATION_NUMBER, Date, Value)
  }
  
  data
}




## Fill missing dates, add date variables and add AnalysisYear, DoY, and/or Date
## Used in prep for analyses
## -----------------------------------------------------------------------------

analysis_prep <- function(data,
                          water_year_start,
                          date = FALSE){
  
  # Fill in missing dates to ensure all years are covered
  data <- fill_missing_dates(data = data, water_year_start = water_year_start)
  data <- add_date_variables(data = data, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
 
    if (date) {
      if (water_year_start == 1)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1989-12-31")
      } else if (water_year_start == 2)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-01-31")
      } else if (water_year_start == 3)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-02-28")
      } else if (water_year_start == 4)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-03-31")
      } else if (water_year_start == 5)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-04-30")
      } else if (water_year_start == 6)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-05-31")
      } else if (water_year_start == 7)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-06-30")
      } else if (water_year_start == 8)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-07-31")
      } else if (water_year_start == 9)  {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-08-31")
      } else if (water_year_start == 10) {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-09-30")
      } else if (water_year_start == 11) {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-10-31")
      } else if (water_year_start == 12) {data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-11-30")
      }
    }
    
   data
}


filter_complete_yrs <- function(complete_years, flow_data) {
  
  if (complete_years){
    comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                   complete_yr = ifelse(sum(!is.na(RollingValue)) == length(WaterYear), TRUE, FALSE))
    flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "WaterYear"))
    flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
    flow_data <- dplyr::select(flow_data, -complete_yr)
  }
  
  flow_data
}



## add water year months (reorders the months to the start of the water year)

add_water_months <- function(data, water_year_start){
  
  if (water_year_start > 1) {
    data <- dplyr::mutate(data,
                          AnalysisMonth = Month)
  } else {
    data <- dplyr::mutate(data,
                          AnalysisMonth = ifelse(Month < water_year_start,
                                                 Month - water_year_start + 13,
                                                 Month - water_year_start + 1))
  }
  
  data
}

## Various check functions


one_station_number_stop <- function(station_number) {
  if (length(station_number) > 1) stop("Multiple station_numbers were provided, only one can be listed for this function.", call. = FALSE)
}

one_station_number_stop_data <- function(data){
  if (length(unique(data$STATION_NUMBER)) > 1) 
    stop("Multiple station numbers were provided in the groups column, only one can be listed for this function. Filter for one station or remove the column.", call. = FALSE)
}

rolling_days_checks <- function(roll_days, roll_align , multiple = FALSE) {
  if (!multiple) {
    if (length(roll_days) > 1)                          stop("Only one roll_days value can be listed for this function.", call. = FALSE)
  }
  if (!is.numeric(roll_days))                         stop("roll_days argument must be numeric.", call. = FALSE)
  if (!all(roll_days %in% c(1:180)))                  stop("roll_days argument must be integers > 0 and <= 180).", call. = FALSE)
  if (!roll_align %in% c("right", "left", "center"))  stop("roll_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
}

# rolling_days_multiple_checks <- function(roll_days, roll_align) {
#   if (!is.numeric(roll_days))                         stop("roll_days argument must be numeric.", call. = FALSE)
#   if (!all(roll_days %in% c(1:180)))                  stop("roll_days argument must be integers > 0 and <= 180).", call. = FALSE)
#   if (!roll_align %in% c("right", "left", "center"))  stop("roll_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
# }

water_year_checks <- function(water_year_start) {
  if (!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (length(water_year_start) > 1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).", call. = FALSE)
}

years_checks <- function(start_year, end_year, exclude_years) {
  if (length(start_year) > 1)        stop("Only one start_year value can be listed.", call. = FALSE)
  if (!start_year %in% c(0:9999))    stop("start_year must be an integer.", call. = FALSE)
  if (length(end_year) > 1)          stop("Only one end_year value can be listed.", call. = FALSE)
  if (!end_year %in% c(0:9999))      stop("end_year must be an integer.", call. = FALSE)
  if (start_year > end_year)         stop("start_year must be less than or equal to end_year.", call. = FALSE)
  
  if (!is.null(exclude_years) & !is.numeric(exclude_years)) stop("List of exclude_years must be numeric - ex. 1999 or c(1999,2000).", call. = FALSE)
  if (!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.", call. = FALSE)
}

complete_yrs_checks <- function(complete_years) {
  if (length(complete_years) > 1)        stop("Only one complete_years logical value can be listed.", call. = FALSE)
  if (!is.logical(complete_years))       stop("complete_years argument must be logical (TRUE/FALSE).", call. = FALSE)
}

months_checks <- function(months) {
  if (!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
}

custom_months_checks <- function(custom_months, custom_months_label) {
  if (!is.null(custom_months) & !is.numeric(custom_months))  
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (!all(custom_months %in% c(1:12)))                      
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (!is.na(custom_months_label) & !is.character(custom_months_label)) 
    stop("custom_months_label argument must be a character string.", call. = FALSE)
}

percentiles_checks <- function(percentiles) {
  if (!all(is.na(percentiles))){
    if (!is.numeric(percentiles))                   stop("percentiles argument must be numeric.", call. = FALSE)
    if (!all(percentiles > 0 & percentiles < 100))  stop("percentiles must be > 0 and < 100.", call. = FALSE)
  }
}

transpose_checks <- function(transpose) {
  if (length(transpose) > 1)        stop("Only one transpose logical value can be listed.", call. = FALSE)
  if (!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).", call. = FALSE)
}

spread_checks <- function(spread) {
  if (length(spread) > 1)        stop("Only one spread logical value can be listed.", call. = FALSE)
  if (!is.logical(spread))       stop("spread argument must be logical (TRUE/FALSE).", call. = FALSE)
}

ignore_missing_checks <- function(ignore_missing) {
  if (length(ignore_missing) > 1)   stop("Only one ignore_missing logical value can be listed.", call. = FALSE)
  if (!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).", call. = FALSE)
}

log_discharge_checks <- function(log_discharge) {
  if (length(log_discharge) > 1)   stop("Only one log_discharge logical value can be listed.", call. = FALSE)
  if (!is.logical(log_discharge))  stop("log_discharge argument must be logical (TRUE/FALSE).", call. = FALSE)
}

include_title_checks <- function(include_title) {
  if (length(include_title) > 1)   stop("Only one include_title logical value can be listed.", call. = FALSE)
  if (!is.logical(include_title))  stop("include_title argument must be logical (TRUE/FALSE).", call. = FALSE)
}

use_yield_checks <- function(use_yield) {
  if (length(use_yield) > 1)   stop("Only one use_yield logical value can be listed.", call. = FALSE)
  if (!is.logical(use_yield))  stop("use_yield argument must be logical (TRUE/FALSE).", call. = FALSE)
}

include_seasons_checks <- function(include_seasons) {
  if (length(include_seasons) > 1)   stop("Only one include_seasons logical value can be listed.", call. = FALSE)
  if (!is.logical(include_seasons))  stop("include_seasons argument must be logical (TRUE/FALSE).", call. = FALSE)
}

percent_total_checks <- function(percent_total) {
  if (!is.numeric(percent_total))                    stop("percent_total must be numeric.", call. = FALSE)
  if (!all(percent_total > 0 & percent_total < 100)) stop("percent_total must be > 0 and < 100).", call. = FALSE)
}

normal_percentiles_checks <- function(normal_percentiles) {
  if (!is.numeric(normal_percentiles) )                stop("normal_percentiles must be two numeric values.", call. = FALSE)
  if (length(normal_percentiles) != 2 )                stop("normal_percentiles must be two numeric values (ex. c(25,75)).", call. = FALSE)
  if (!all(is.na(normal_percentiles)) & (!all(normal_percentiles > 0 & normal_percentiles < 100)) )  
    stop("normal_percentiles must be >0 and <100)", call. = FALSE)
}

lowflow_days_checks <- function(lowflow_days, lowflow_align) {
  if (!is.numeric(lowflow_days))                         stop("lowflow_days argument must be numeric.", call. = FALSE)
  if (!all(lowflow_days %in% c(1:180)))                  stop("lowflow_days argument must be integers > 0 and <= 180).", call. = FALSE)
  if (!lowflow_align %in% c("right", "left", "center"))  stop("lowflow_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
}

stats_days_checks <- function(stats_days, stats_align) {
  if (length(stats_days) > 1)                          stop("Only one stats_days value can be listed for this function.", call. = FALSE)
  if (!is.numeric(stats_days))                         stop("stats_days argument must be numeric.", call. = FALSE)
  if (!all(stats_days %in% c(1:180)))                  stop("stats_days argument must be integers > 0 and <= 180).", call. = FALSE)
  if (!stats_align %in% c("right", "left", "center"))  stop("stats_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
}

ann_percentiles_checks <- function(annual_percentiles) {
  if (!all(is.na(annual_percentiles))){
    if (!is.numeric(annual_percentiles))                          stop("annual_percentiles argument must be numeric.", call. = FALSE)
    if (!all(annual_percentiles > 0 & annual_percentiles < 100))  stop("annual_percentiles must be > 0 and < 100.", call. = FALSE)
  }
}

mon_percentiles_checks <- function(monthly_percentiles) {
  if (!all(is.na(monthly_percentiles))){
    if (!is.numeric(monthly_percentiles))                           stop("monthly_percentiles argument must be numeric.", call. = FALSE)
    if (!all(monthly_percentiles > 0 & monthly_percentiles < 100))  stop("monthly_percentiles must be > 0 and < 100.", call. = FALSE)
  }
}

timing_pct_checks <- function(timing_percent) {
  if (!is.numeric(timing_percent))                     stop("timing_percent must be numeric.", call. = FALSE)
  if (!all(timing_percent > 0 & timing_percent < 100)) stop("timing_percent must be > 0 and < 100).", call. = FALSE)
}


include_year_checks <- function(include_year) {
  if(!is.null(include_year)){
    if(length(include_year) != 1)  stop("Only one include_year numeric value can be provided.", call. = FALSE)
    if(!is.numeric(include_year))  stop("include_year argument must be numeric.", call. = FALSE)
  }
}

# for some single calculation functions
missing_values_warning_noNA <- function(x) {
  if (anyNA(x)) 
    warning("Calculation ignored missing values in data. Filter data to complete years or months if desired.", call. = FALSE)
}

missing_values_warning <- function(x) {
  if (anyNA(x)) 
    warning("One or more calculations included missing values and NA's were produced. Filter data for complete years or months, or use to ignore_missing = TRUE to ignore missing values.", call. = FALSE)
}

# For annual timing, normals
missing_complete_yr_warning <- function(x) {
  if (anyNA(x)) 
    warning("One or more years contained partial or missing data and NA's were produced. Only time periods with complete data were calculated.", call. = FALSE)
}

zyp_method_checks <- function(zyp_method) {
  if (is.na(zyp_method) | !zyp_method %in% c("yuepilon", "zhang") )   
    stop('zyp_trending argument must be either "yuepilon" or "zhang"', call. = FALSE)
}

zyp_alpha_checks <- function(zyp_alpha){
  if(!is.na(zyp_alpha) & !is.numeric(zyp_alpha) )              
    stop("zyp_alpha must be numeric.", call. = FALSE)
  if(!is.na(zyp_alpha) & !all(zyp_alpha >= 0 & zyp_alpha <= 1))  
    stop("timing_percent must be >= 0 and <= 1)", call. = FALSE)
}

include_longterm_checks <- function(include_longterm){
  if (length(include_longterm) > 1)   stop("Only one include_longterm logical value can be listed.", call. = FALSE)
  if (!is.logical(include_longterm))  stop("include_longterm argument must be logical (TRUE/FALSE).", call. = FALSE)
}




