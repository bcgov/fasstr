# Copyright 2017 Province of British Columbia
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


#' @title Fill dates of missing flow values with NA
#'
#' @description Adds rows of dates with missing flow values to a streamflow dataset with daily flow values of NA. Missing dates will 
#'    be filled in gaps between data and compeltely fill the first and last years (calendar or water year if selected).
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groupings (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param grouping Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and grouping column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' 
#' @return A data frame of the source data with additional rows of filled values of missing dates.
#'
#' @examples
#' \dontrun{
#' 
#'fill_missing_dates(data = flow_data)
#' 
#'fill_missing_dates(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


fill_missing_dates <- function(data = NULL,
                               dates = Date,
                               values = Value,
                               grouping = STATION_NUMBER,
                               water_year=FALSE,
                               water_year_start=10){
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
    if(is.vector(data)) {
    if(!all(data %in% dplyr::pull(tidyhydat::allstations[1]))) 
      stop("One or more stations numbers listed in data argument do not exist in HYDAT. Re-check numbers or provide a data frame of data.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
  } else {
    flow_data <- data
  }
  if(!is.data.frame(flow_data)) stop("Incorrect selection for data argument, must provide a data frame or HYDAT station number(s).")
  flow_data <- as.data.frame(flow_data) # Getting random 'Unknown or uninitialised column:' warnings if using tibble
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # If no grouping (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(grouping)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(grouping))] <- "XXXXXXX"
  }
  
  # Get the just grouping (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using
         'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Temporarily rename the Date and Value columns
  names(flow_data)[names(flow_data) == as.character(substitute(grouping))] <- "STATION_NUMBER"
  names(flow_data)[names(flow_data) == as.character(substitute(dates))] <- "Date"
  names(flow_data)[names(flow_data) == as.character(substitute(values))] <- "Value"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  
  ## FILL IN GAPS
  ## ------------
  
  # Loop through each station number, fill in gaps and append
  flow_data_new <- flow_data[0,]
  for (stn in unique(flow_data$STATION_NUMBER)) {
    
    # Filter for station number
    flow_data_stn <- dplyr::filter(flow_data, STATION_NUMBER == stn)
    flow_data_stn <- flow_data_stn[order(flow_data_stn$Date), ]
    
    # Fill if water year is TRUE and month is not January
    if (water_year & water_year_start > 1) {
      
      # Determine the min months and years to set the start_date
      # If the month in the data is less than the water_year_start, the water year will begin in the previous calendar year
      min_month_wy <- lubridate::month(min(flow_data_stn$Date))
      min_year_wy <- lubridate::year(min(flow_data_stn$Date))
      if (min_month_wy < water_year_start) {
        start_date=as.Date(paste(min_year_wy - 1, water_year_start, '01', sep='-'), "%Y-%m-%d")
      } else {
        start_date=as.Date(paste(min_year_wy, water_year_start, '01', sep='-'), "%Y-%m-%d")
      }
      
      # Determine the max months and years to set the start_date
      # If the month in the data is greater than the water_year_start, the water year will end in the next calendar year
      max_month_wy <- lubridate::month(max(flow_data_stn$Date))
      max_year_wy <- lubridate::year(max(flow_data_stn$Date))
      if (max_month_wy > water_year_start) {
        end_date=as.Date(paste(max_year_wy + 1, water_year_start, '01', sep='-'), "%Y-%m-%d") - 1
      } else {
        end_date=as.Date(paste(max_year_wy, water_year_start, '01', sep='-'), "%Y-%m-%d") - 1
      }
      
      # Fill in missing dates
      flow_data_stn <- merge(flow_data_stn, 
                             data.frame(Date=seq(start_date, end_date, 1)),
                             all.y = TRUE)
      
      
      # Fill not water year, or January is chosen as water year start  
    } else {
      min_year <- lubridate::year(min(flow_data_stn$Date))
      max_year <- lubridate::year(max(flow_data_stn$Date))
      
      # Fill in missing dates
      flow_data_stn <- merge(flow_data_stn, 
                             data.frame(Date=seq(as.Date(paste(min_year, '01-01', sep='-'), "%Y-%m-%d"),
                                                 as.Date(paste(max_year  ,'12-31',sep='-'), '%Y-%m-%d'), 
                                                 1)),
                             all.y = TRUE)
    }
    
    # Fill in station number and parameter gaps (removed if not originally there)
    flow_data_stn$STATION_NUMBER <- stn
    flow_data_stn$Parameter <- "Flow"
    
    # Append to flow_data
    flow_data_new <- dplyr::bind_rows(flow_data_new, flow_data_stn)
    
  }
  flow_data <- flow_data_new
  
  # Return the original names of the columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(grouping))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  #Return columns to original order
  flow_data <-  flow_data[,orig_cols]
  
  
  dplyr::as_tibble(flow_data)
  
  
} 
