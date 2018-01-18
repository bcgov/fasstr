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
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groups (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param groups Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and groups column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' 
#' @return A data frame of the source data with an additional column(s):
#'   \item{QnDay}{rolling means of the n-day flow values of the designated date and adjacent dates, direction of mean specified by align}
#'   Default additional columns:
#'   \item{Q3Day}{rolling means of the 3-day flow values of the designated date and previous 2 days (align="right")}
#'   \item{Q7Day}{rolling means of the 7-day flow values of the designated date and previous 6 days (align="right")}
#'   \item{Q30Day}{rolling means of the 30-day flow values of the designated date and previous 29 days (align="right")}
#'    
#' @examples
#' \dontrun{
#' 
#'add_rolling_means(data = flow_data, days = 7, align = 'centre')
#' 
#'add_rolling_means(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_rolling_means <- function(data = NULL,
                              dates = Date,
                              values = Value,
                              groups = STATION_NUMBER,
                              days = c(3,7,30),
                              align = "right"){
  
  
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
  
  # Get groups of flow_data to return after
  flow_data_groups <- dplyr::group_vars(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for station groups)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using
         'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Temporarily rename the Date and Value columns
  names(flow_data)[names(flow_data) == as.character(substitute(groups))] <- "STATION_NUMBER"
  names(flow_data)[names(flow_data) == as.character(substitute(dates))] <- "Date"
  names(flow_data)[names(flow_data) == as.character(substitute(values))] <- "Value"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in data frame does not contain numeric values.")
  
  
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
    flow_data_stn <- fill_missing_dates(data = flow_data_stn)
    
    # Add rolling means
    for (x in days) {
      flow_data_stn[, paste0("Q", x, "Day")] <- RcppRoll::roll_mean(flow_data_stn$Value, n = x, fill = NA, align = align)
    }
    
    # Return flow_data_stn to original dates
    flow_data_stn <- dplyr::filter(flow_data_stn, Date %in% dates_list)
    
    # Append to flow_data
    flow_data_new <- dplyr::bind_rows(flow_data_new, flow_data_stn)
    
  }
  flow_data <- flow_data_new
  
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  # Remove the STATION_NUMBER columns if one wasn't in flowdata originally
  if(!"STATION_NUMBER" %in% orig_cols) {
    flow_data <- dplyr::select(flow_data, -STATION_NUMBER)
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(flow_data_groups))
  
  flow_data
  
} 

