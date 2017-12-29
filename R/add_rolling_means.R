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

#' @title Add rolling n-day averages
#'
#' @description Adds selected n-day rolling means to a streamflow dataset. Based on selected n-days and alignment, the rolling mean for
#'   a given day is obtained by averaging the adjacent dates of daily mean values. For example, rolling days of '7' and 'right' alignment 
#'   would obtain a mean of the given and previous 6 days of daily mean flow. Rolling mean values will not be calculated if there is less 
#'   than the n-days provided.
#'
#' @param flow_data a data frame of daily mean flow data that contains columns of dates, flow values, and (optional) station 
#'    names/numbers. Leave blank if using \code{HYDAT} argument.
#' @param flow_dates a column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Leave blank if using \code{HYDAT} 
#'    argument. Default \code{Date}. 
#' @param flow_values a column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Leave blank if using \code{HYDAT} argument. Default \code{Value}.
#' @param flow_stations a column in flow_data that contains station identifiers for each flow data set, if required. Default 
#'    \code{STATION_NUMBER}. 
#' @param HYDAT a character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the 
#'    \code{tidyhydat} package and a HYDAT database are required. Leave blank if using \code{flow_data} arguments.
#' @param days a numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param align a character identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' 
#' @return A data frame of the original flow_data or HYDAT data with additional column(s) of:
#'   \item{QnDay}{rolling means of the n-day flow values of the designated date and adjacent dates, direction of mean specified by align}
#'   Default additional columns:
#'   \item{Q3Day}{rolling means of the 3-day flow values of the designated date and previous 2 days (align="right")}
#'   \item{Q7Day}{rolling means of the 7-day flow values of the designated date and previous 6 days (align="right")}
#'   \item{Q30Day}{rolling means of the 30-day flow values of the designated date and previous 29 days (align="right")}
#'    
#' @examples
#' \dontrun{
#' 
#'add_rolling_means(flow_data = flow_data, days = 7, align = 'centre')
#' 
#'add_rolling_means(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_rolling_means <- function(flow_data = NULL,
                              flow_dates = Date,
                              flow_values = Value,
                              flow_stations = STATION_NUMBER,
                              HYDAT = NULL,
                              days = c(3,7,30),
                              align = "right"){
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(flow_data) & is.null(HYDAT))   stop("No flow data provided, must use flow_data or HYDAT arguments.")
  if(!is.null(flow_data) & !is.null(HYDAT)) stop("Only one of flow_data or HYDAT arguments can be used.")
  
  # Get HYDAT data if selected and stations exist
  if(!is.null(HYDAT)) {
    if(!all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1]))) stop("One or more stations listed in 'HYDAT' do not exist.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Get groups of flow_data to return after
  grouping <- dplyr::group_vars(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for station grouping)
  if(!as.character(substitute(flow_stations)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(flow_stations))] <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Temporarily rename the Date and Value columns
  names(flow_data)[names(flow_data) == as.character(substitute(flow_stations))] <- "STATION_NUMBER"
  names(flow_data)[names(flow_data) == as.character(substitute(flow_dates))] <- "Date"
  names(flow_data)[names(flow_data) == as.character(substitute(flow_values))] <- "Value"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in flow_data data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.numeric(days))                         stop("days argument must be integers > 0 and <= 180.")
  if(!all(days %in% c(1:180)))                  stop("days argument must be integers > 0 and <= 180.")
  if(!align %in% c("right", "left", "center"))  stop("align argument must be 'right', 'left', or 'center'.")
  
  
  ## ADD ROLLING MEANS
  ## -----------------
  
  # Loop through each station number, and add rolling means
  flow_data_new <- flow_data[0,]
  for (stn in unique(flow_data$STATION_NUMBER)) {
    
    # Filter for station number
    flow_data_stn <- dplyr::filter(flow_data, STATION_NUMBER == stn)
    flow_data_stn <- flow_data_stn[order(flow_data_stn$Date), ]
    dates_list <- c(flow_data_stn$Date)
    
    # fill in missing dates to ensure means roll over consecutive days
    flow_data_stn <- fasstr::fill_missing_dates(flow_data = flow_data_stn)
    
    # Add rolling means
    for (x in days) {
      flow_data_stn[, paste0("Q", x, "Day")] <- RcppRoll::roll_mean(flow_data_stn$Value,  n=x, fill=NA, align = align)
    }
    
    # Return flow_data_stn to original dates
    flow_data_stn <- dplyr::filter(flow_data_stn, Date %in% dates_list)
    
    # Append to flow_data
    flow_data_new <- dplyr::bind_rows(flow_data_new, flow_data_stn)
    
  }
  flow_data <- flow_data_new
  
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(flow_stations))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(flow_dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(flow_values))
  
  # Remove the STATION_NUMBER columns if one wasn't in flowdata originally
  if(!as.character(substitute(flow_stations)) %in% orig_cols) {
    flow_data <- dplyr::select(flow_data, -STATION_NUMBER)
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(grouping))
  
  flow_data
  
} 

