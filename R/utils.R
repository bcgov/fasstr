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


## Checks on data and importing HYDAT data
## ---------------------------------------

flowdata_import <- function(data = NULL, station_number = NULL){
  
  # Check if data is provided
  if (is.null(station_number) & is.null(data))    stop("must select one of data or station_number arguments", call. = FALSE)
  if (!is.null(station_number) & !is.null(data))  stop("must select either data or station_number arguments, not both", call. = FALSE)
  
  if (is.null(data)) {
    if (!is.character(station_number))  stop("station_number must be a character vector containing station numbers", call. = FALSE)
    if (!all(station_number %in% dplyr::pull(suppressMessages(tidyhydat::hy_stations()[1])))) 
      stop("One or more station numbers listed do not have historical daily flows in HYDAT.", call. = FALSE)
    data <- as.data.frame(suppressMessages(tidyhydat::hy_daily_flows(station_number =  station_number)))
  } else {
    if (!is.data.frame(data))  stop("data arguments is not a data frame", call. = FALSE)
    data <- as.data.frame(data)
  }
  
  # Others
  if ("Parameter" %in% names(data)) {
    data$Parameter <- as.character(data$Parameter)
  }
  if ("Symbol" %in% names(data)) {
    data$Symbol <- as.character(data$Symbol)
  }
  
  data
}



## Check for dates, values, and groups and rename
## ----------------------------------------------

format_all_cols <- function(data,
                               dates = "Date",
                               values = "Value",
                               groups = "STATION_NUMBER"){
  
  
  data <- format_dates_col(data, dates = dates)
  data <- format_values_col(data, values = values)
  data <- format_groups_col(data, groups = groups)
  
  # # If no groups (default STATION_NUMBER) in data, make it so (required)
  # if (!groups %in% colnames(data)) {
  #   data[, groups] <- "XXXXXXX"
  # }
  # 
  # # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # # This method allows the user to select the Station, Date or Value columns if the column names are different
  # if (!values %in% names(data) & !dates %in% names(data))
  #   stop("dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.", call. = FALSE)
  # if (!dates %in% names(data))
  #   stop("dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.", call. = FALSE)
  # if (!values %in% names(data))
  #   stop("values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.", call. = FALSE)
  # 
  # # Temporarily rename the Date and Value columns
  # names(data)[names(data) == groups] <- "STATION_NUMBER"
  # names(data)[names(data) == dates] <- "Date"
  # names(data)[names(data) == values] <- "Value"
  # 
  # # Check dates are dates, and attempt to format
  # if (!inherits(data$Date[1], "Date"))  {
  #   test <- try(as.Date(data$Date, "%Y-%m-%d"), silent = TRUE)
  #   if ("try-error" %in% class(test)) {
  #     stop("dates cannot be formatted as dates (YYYY-MM-DD).")
  #   } else {
  #     data$Date <- as.Date(data$Date, "%Y-%m-%d")
  #   }
  # }
  # 
  # # Check if values are numeric
  # if (!is.numeric(data$Value))  stop("values in values column ", call. = FALSE)
  # 
  # # Make groups characters
  # data$STATION_NUMBER <- as.character(data$STATION_NUMBER)
  
  data
}


format_dates_col <- function(data,
                         dates = "Date"){
  
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if (!dates %in% names(data))  
    stop("dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.", call. = FALSE)
  
  # Temporarily rename the Date and Value columns
  names(data)[names(data) == dates] <- "Date"
  
  # Check dates are dates, and attempt to format
  if (!inherits(data$Date[1], "Date"))  {
    test <- try(as.Date(data$Date, "%Y-%m-%d"), silent = TRUE)
    if ("try-error" %in% class(test)) {
      stop("dates cannot be formatted as dates (YYYY-MM-DD).")
    } else {
      data$Date <- as.Date(data$Date, "%Y-%m-%d")
    }
  }
  
  data
}


format_values_col <- function(data,
                          values = "Value"){
  
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if (!values %in% names(data)) 
    stop("values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.", call. = FALSE)
  
  # Temporarily rename the Date and Value columns
  names(data)[names(data) == values] <- "Value"
  
  # Check if values are numeric
  if (!is.numeric(data$Value))          stop("values in values column ", call. = FALSE)
  
  data
}



format_groups_col <- function(data,
                          groups = "STATION_NUMBER"){
  
  # If no groups (default STATION_NUMBER) in data, make it so (required)
  if (!groups %in% colnames(data)) {
    data[, groups] <- "XXXXXXX"
  }
  
  # Temporarily rename the Date and Value columns
  names(data)[names(data) == groups] <- "STATION_NUMBER"
  
  # Make groups characters
  data$STATION_NUMBER <- as.character(data$STATION_NUMBER)
  
  data
}











## Various check functions

rolling_days_checks <- function(rolling_days, rolling_align) {
  if (!is.numeric(rolling_days))                         stop("rolling_days argument must be numeric.", call. = FALSE)
  if (!all(rolling_days %in% c(1:180)))                  stop("rolling_days argument must be integers > 0 and <= 180).", call. = FALSE)
  if (!rolling_align %in% c("right", "left", "center"))  stop("rolling_align argument must be 'right', 'left', or 'center'.", call. = FALSE)
}

water_year_checks <- function(water_year, water_year_start) {
  if (!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).", call. = FALSE)
  if (!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).", call. = FALSE)
  if (!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).", call. = FALSE)
}

years_checks <- function(start_year, end_year, exclude_years) {
  if(length(start_year) > 1)        stop("Only one start_year value can be listed.", call. = FALSE)
  if(!start_year %in% c(0:9999))    stop("start_year must be an integer.", call. = FALSE)
  if(length(end_year) > 1)          stop("Only one end_year value can be listed.", call. = FALSE)
  if(!end_year %in% c(0:9999))      stop("end_year must be an integer.", call. = FALSE)
  if(start_year > end_year)         stop("start_year must be less than or equal to end_year.", call. = FALSE)
  
  if(!is.null(exclude_years) & !is.numeric(exclude_years)) stop("List of exclude_years must be numeric - ex. 1999 or c(1999,2000).", call. = FALSE)
  if(!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.", call. = FALSE)
}

months_checks <- function(months) {
  if(!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
  if(!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).", call. = FALSE)
}

percentiles_checks <- function(percentiles) {
  if(!all(is.na(percentiles))){
    if(!is.numeric(percentiles))                   stop("percentiles argument must be numeric.", call. = FALSE)
    if(!all(percentiles > 0 & percentiles < 100))  stop("percentiles must be > 0 and < 100.", call. = FALSE)
  }
}



















