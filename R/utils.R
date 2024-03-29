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
  
  
  data
  
}

## Check for dates and proper formatting
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

## Check for symbols and proper formatting

format_symbols_col <- function(data,
                               symbols = "Symbol"){
  
  # Check if column exists
  if (!symbols %in% names(data)) 
    stop("Symbols not found in data frame.", call. = FALSE)
  
  # Rename values to "STATION_NUMBER" (and change original if required so no duplication)
  if ("Symbol" %in% colnames(data) & symbols != "Symbol") {
    names(data)[names(data) == "Symbol"] <- "Symbol_orig"
  }
  
  names(data)[names(data) == symbols] <- "Symbol"
  
  data$Symbol[data$Symbol == ""] <- NA 
  
  data
}

## Check for dates, values, and groups proper formatting
format_all_cols <- function(data,
                            dates = "Date",
                            values = "Value",
                            groups = "STATION_NUMBER",
                            symbols = "Symbol",
                            rm_other_cols = FALSE,
                            keep_symbols = FALSE){
  
  # Check format all columns
  data <- format_dates_col(data, dates = dates)
  data <- format_values_col(data, values = values)
  data <- format_groups_col(data, groups = groups)
  if (keep_symbols) {
    data <- format_symbols_col(data, symbols = symbols)
  }
  
  # Remove all other columns if TRUE
  if (rm_other_cols & !keep_symbols) {
    data <- dplyr::select(data, STATION_NUMBER, Date, Value)
  }
  
  if (rm_other_cols & keep_symbols) {
    data <- dplyr::select(data, STATION_NUMBER, Date, Value, Symbol)
  }
  
  data
}

## Fill missing dates, add date variables and add AnalysisYear, DoY, and/or Date
## Used in prep for analyses
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

# Get the origin date for plotting days of years
get_origin_date <- function(water_year_start){
  if (water_year_start == 1)         {origin_date <- as.Date("1899-12-31")
  } else if (water_year_start == 2)  {origin_date <- as.Date("1899-01-31")
  } else if (water_year_start == 3)  {origin_date <- as.Date("1899-02-28")
  } else if (water_year_start == 4)  {origin_date <- as.Date("1899-03-31")
  } else if (water_year_start == 5)  {origin_date <- as.Date("1899-04-30")
  } else if (water_year_start == 6)  {origin_date <- as.Date("1899-05-31")
  } else if (water_year_start == 7)  {origin_date <- as.Date("1899-06-30")
  } else if (water_year_start == 8)  {origin_date <- as.Date("1899-07-31")
  } else if (water_year_start == 9)  {origin_date <- as.Date("1899-08-31")
  } else if (water_year_start == 10) {origin_date <- as.Date("1899-09-30")
  } else if (water_year_start == 11) {origin_date <- as.Date("1899-10-31")
  } else if (water_year_start == 12) {origin_date <- as.Date("1899-11-30")
  }
  origin_date
}

# Filter data for complete years (require fill_missing and add_dates and Value = RollingValue beforehand)
filter_complete_yrs <- function(complete_years, flow_data, keep_all = FALSE) {
  if (complete_years){
    comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                   complete_yr = ifelse(sum(!is.na(RollingValue)) == length(WaterYear), TRUE, FALSE))
    flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "WaterYear"))
    if (!keep_all) {
      flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
    } else {
      flow_data <- dplyr::mutate(flow_data,
                                 RollingValue = ifelse(complete_yr, RollingValue, NA))
    }
    flow_data <- dplyr::select(flow_data, -complete_yr)
    
  }
  flow_data
}

# Filter data for complete years (require fill_missing and add_dates and Value = RollingValue beforehand)
filter_complete_yrs_val <- function(complete_years, flow_data, keep_all = FALSE) {
  if (complete_years){
    comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                   complete_yr = ifelse(sum(!is.na(Value)) == length(WaterYear), TRUE, FALSE))
    flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "WaterYear"))
    if (!keep_all) {
      flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
    } else {
      flow_data <- dplyr::mutate(flow_data,
                                 Value = ifelse(complete_yr, Value, NA))
    }
    flow_data <- dplyr::select(flow_data, -complete_yr)
    
  }
  flow_data
}

# Fn to get lists of incomplete/complete years from data
get_complete_years_lists <- function(flow_data, water_year_start = 1, months = 1:12) {
  data1 <- add_date_variables(flow_data, water_year_start = water_year_start)
  all_data_years <- unique(data1$WaterYear)
  data1 <- fill_missing_dates(data1, water_year_start = water_year_start)
  data1 <- add_date_variables(data1, water_year_start = water_year_start)
  data1 <- dplyr::filter(data1, Month %in% months)
  data1 <- dplyr::summarise(dplyr::group_by(data1, WaterYear),
                            n_na = sum(is.na(Value)), .groups = "keep")
  complete_years <- dplyr::filter(data1, n_na == 0)
  complete_years <- dplyr::pull(complete_years, WaterYear)
  incomplete_years <- dplyr::filter(data1, n_na > 0)
  incomplete_years <- dplyr::pull(incomplete_years, WaterYear)
  
  list(complete_years = complete_years,
       incomplete_years = incomplete_years,
       data_years = all_data_years)
}

get_complete_years_vars <- function(flow_data, water_year_start = 1, months = 1:12) {
  cmplt_years <- get_complete_years_lists(flow_data, water_year_start, months)
  start_year <- min(cmplt_years$complete_years)
  end_year <- max(cmplt_years$complete_years)
  exclude_years <- cmplt_years$incomplete_years
  exclude_years <- exclude_years[exclude_years %in% cmplt_years$data_years]
  exclude_years <- exclude_years[exclude_years > start_year]
  exclude_years <- exclude_years[exclude_years < end_year]
  list(start_year = start_year,
       end_year = end_year,
       exclude_years = exclude_years,
       complete_years = cmplt_years$complete_years)
}

## add water year months (reorders the months to the start of the water year)
add_water_months <- function(data, water_year_start){
  
  if (water_year_start == 1) {
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

# General logical argument check
logical_arg_check <- function(logical_arg) {
  if (length(logical_arg) > 1)        
    stop(paste0("Only one ", as.character(substitute(logical_arg)), " logical value can be listed."), call. = FALSE)
  if (!is.logical(logical_arg))       
    stop(as.character(substitute(logical_arg)), " argument must be logical (TRUE/FALSE).", call. = FALSE)
}

one_station_number_stop <- function(station_number) {
  if (length(station_number) > 1) stop("Multiple station_numbers were provided, only one can be listed for this function.", call. = FALSE)
}

one_station_number_stop_data <- function(data){
  if (length(unique(data$STATION_NUMBER)) > 1) 
    stop("Multiple station numbers were provided in the groups column, only one can be listed for this function. Filter for one station or remove the column.", call. = FALSE)
}

rolling_days_checks <- function(roll_days, roll_align , multiple = FALSE) {
  if (!multiple) {
    if (length(roll_days) > 1)                          stop(paste0("Only one ",as.character(substitute(roll_days)), 
                                                                    " value can be listed for this function."), call. = FALSE)
  }
  if (!is.numeric(roll_days))                         stop(paste0(as.character(substitute(roll_days)), " argument must be numeric."), call. = FALSE)
  if (!all(roll_days %in% c(1:180)))                  stop(paste0(as.character(substitute(roll_days)), " argument must be integers > 0 and <= 180)."), call. = FALSE)
  roll_align <- ifelse(roll_align == "l", "left", ifelse(roll_align == "r", "right", ifelse(roll_align == "c", "center", ifelse(roll_align == "centre", "center", roll_align))))
  if (!roll_align %in% c("right", "left", "center"))  stop("roll_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
}

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

numeric_range_checks <- function(percentiles) {
  if (!all(is.na(percentiles))){
    if (!is.numeric(percentiles))                   
      stop(paste0(as.character(substitute(percentiles)), " argument must be numeric."), call. = FALSE)
    if (!all(percentiles >= 0 & percentiles <= 100))  
      stop(paste0(as.character(substitute(percentiles)), " must be >= 0 and <= 100."), call. = FALSE)
  }
}

numeric_checks <- function(numbers) {
  if (!all(is.na(numbers))){
    if (!is.numeric(numbers))                   
      stop(paste0(as.character(substitute(numbers)), " argument must be numeric."), call. = FALSE)
  }
}

list_check <- function(argument, list) {
  if (length(argument) > 1)        stop("Only one plot_type logical value can be listed.", call. = FALSE)
  if (!argument %in% list)       
    stop(paste0(as.character(substitute(numbers))," argument must be one of '",paste0(list, collapse = "', '"),"'."), call. = FALSE)
}

log_ticks_checks <- function(log_ticks, log_discharge) {
  if (length(log_ticks) > 1)   stop("Only one log_ticks logical value can be listed.", call. = FALSE)
  if (!is.logical(log_ticks))  stop("log_ticks argument must be logical (TRUE/FALSE).", call. = FALSE)
  if (!log_discharge & log_ticks) warning("logarithmic scale ticks will not be plotted on linear discharge.", call. = FALSE)
}

normal_percentiles_checks <- function(normal_percentiles) {
  if (!is.numeric(normal_percentiles) )                stop("normal_percentiles must be two numeric values.", call. = FALSE)
  if (length(normal_percentiles) != 2 )                stop("normal_percentiles must be two numeric values (ex. c(25,75)).", call. = FALSE)
  if (!all(is.na(normal_percentiles)) & (!all(normal_percentiles >= 0 & normal_percentiles <= 100)) )  
    stop("normal_percentiles must be >= 0 and <= 100)", call. = FALSE)
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

add_year_checks <- function(add_year) {
  if(!is.null(add_year)){
    if(length(add_year) != 1)  stop("Only one add_year numeric value can be provided.", call. = FALSE)
    if(!is.numeric(add_year))  stop("add_year argument must be numeric.", call. = FALSE)
  }
}

# for some single calculation functions
missing_values_warning_noNA <- function(x) {
  if (anyNA(x)) 
    warning("Calculation ignored missing values in data. Filter data to complete years or months if desired.", call. = FALSE)
}

missing_values_warning <- function(x) {
  if (anyNA(x)) 
    warning(paste0("One or more calculations included missing values and NA's were produced. If desired, filter data for complete years or months,",
                   " or use the 'ignore_missing' or 'allowed_missing' arguments (if applicable) to ignore or allow some missing values."), call. = FALSE)
}

# For annual timing, normals
missing_complete_yr_warning <- function(x) {
  if (anyNA(x)) 
    warning("One or more years contained partial or missing data and NA's were produced. Only time periods with complete data were calculated.", call. = FALSE)
}

zyp_method_checks <- function(zyp_method) {
  if (is.na(zyp_method) | !zyp_method %in% c("yuepilon", "zhang") )   
    stop('zyp_trending argument must be either "zhang" or "yuepilon". "zhang" is recommended for hydrologic applications.', 
         call. = FALSE)
}

zyp_alpha_checks <- function(zyp_alpha){
  if(!is.na(zyp_alpha) & !is.numeric(zyp_alpha) )              
    stop("zyp_alpha must be numeric.", call. = FALSE)
  if(!is.na(zyp_alpha) & !all(zyp_alpha >= 0 & zyp_alpha <= 1))  
    stop("timing_percent must be >= 0 and <= 1)", call. = FALSE)
}

ptile_ribbons_checks <- function(inner_percentiles, outer_percentiles){
  if(!is.null(inner_percentiles)) {
    if (!is.numeric(inner_percentiles) )                stop("inner_percentiles must be two numeric values.", call. = FALSE)
    if (length(inner_percentiles) != 2 )                stop("inner_percentiles must be two numeric values (ex. c(25,75)).", call. = FALSE)
    if (!all(is.na(inner_percentiles)) & (!all(inner_percentiles >= 0 & inner_percentiles <= 100)) )  
      stop("inner_percentiles must be >= 0 and <= 100)", call. = FALSE)
  }
  if(!is.null(outer_percentiles)) {
    if (!is.numeric(outer_percentiles) )                stop("outer_percentiles must be two numeric values.", call. = FALSE)
    if (length(outer_percentiles) != 2 )                stop("outer_percentiles must be two numeric values (ex. c(25,75)).", call. = FALSE)
    if (!all(is.na(outer_percentiles)) & (!all(outer_percentiles >= 0 & outer_percentiles <= 100)) )  
      stop("outer_percentiles must be >= 0 and <= 100)", call. = FALSE)
  }
}

# new argument allows existing ignore_missing to work the same, with added usage of percentage missing
allowed_missing_checks <- function(allowed_missing, ignore_missing) {
  if (length(allowed_missing) > 1)        
    stop(paste0("Only one '", allowed_missing, "' value can be listed."), call. = FALSE)
  if (!dplyr::between(allowed_missing, 0 ,100))  
    stop(paste0("'", allowed_missing, "' value must be a number between 0 and 100."), call. = FALSE)
  if (!is.numeric(allowed_missing))                   
    stop(paste0("'", allowed_missing, "' value must be a number between 0 and 100."), call. = FALSE)
  if (ignore_missing & allowed_missing == 0) {
    ignore_missing <- FALSE
    ## remove this notes?
    warning(paste0("With 'ignore_missing = TRUE' and 'allowed_missing' = 0, '",
                   allowed_missing, "' supercedes 'ignore_missing' and values will",
                   " return NA if any missing data."), call. = FALSE)
  }
}

# ignore_missing replacement: if percent of NA is greater than allowed, dont calc, otherwise do so
allowed_narm <- function(value, allowed_missing){
  ifelse(sum(is.na(value)/length(value))*100 >= allowed_missing, FALSE, TRUE)
}

no_values_error <- function(values) {
  if (all(is.na(values))) stop("All daily values are NA, select or filter data for years with data.", call. = FALSE)
}

scales_checks <- function(scales_discharge) {
  if (!scales_discharge %in% c("fixed", "free"))  stop("scales_discharge argument must be 'fixed' or 'free'.", call. = FALSE)
}

conseq <- function(s, type = "num", wrap = NULL, sort = TRUE) {
  
  if(is.null(wrap) && type == "num") wrap <- TRUE
  if(is.null(wrap) && type == "month") wrap <- FALSE
  
  if(sort) s <- sort(as.numeric(s))
  
  if(length(s) == 1) {
    if(type == "num") return(as.character(s))
    if(type == "month") return(month.abb[s])
  }
  
  dif <- s[seq(length(s))][-1] - s[seq(length(s)-1)]
  new <- !c(0, dif == 1)
  cs <- cumsum(new)
  res <- vector(mode="list", max(cs))
  for(i in seq(res)){
    s.i <- s[which(cs == i)]
    if(length(s.i) > 2){
      if(type == "num") res[[i]] <- paste(min(s.i), max(s.i), sep=":")
      if(type == "month") res[[i]] <- paste(month.abb[min(s.i)],
                                            month.abb[max(s.i)], sep="-")
    } else {
      if(type == "num") res[[i]] <- as.character(s.i)
      if(type == "month") res[[i]] <- as.character(month.abb[s.i])
    }
  }
  
  res <- paste(unlist(res), collapse = ", ")
  
  if(wrap) res <- paste0("c(", res, ")")
  
  res
}

get_stn_years <- function(flow_data, year_col = "WaterYear"){
  names(flow_data)[names(flow_data) == year_col] <- "WaterYear"
  years_att <- dplyr::group_by(flow_data, STATION_NUMBER)
  years_att <- dplyr::summarise(
    years_att, 
    start_year = min(WaterYear),
    end_year = max(WaterYear),
    excluded_years = list((start_year:end_year)[!start_year:end_year %in% unique(WaterYear)]))
  years_att
}
