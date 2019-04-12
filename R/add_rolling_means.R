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

#' @title Add rolling n-day averages
#'
#' @description Adds selected n-day rolling means to a streamflow dataset. Based on selected n-days and alignment, the rolling mean for
#'   a given day is obtained by averaging the adjacent dates of daily mean values. For example, rolling days of '7' and 'right' alignment 
#'   would obtain a mean of the given and previous 6 days of daily mean flow. Rolling mean values will not be calculated if there is less 
#'   than the n-days provided.
#'
#' @inheritParams calc_annual_stats
#' @param roll_days Numeric values of the number of days to apply a rolling mean. Default \code{c(3,7,30)}.
#' 
#' @return A data frame of the source data with an additional column(s):
#'   \item{QnDay}{rolling means of the n-day flow values of the designated date and adjacent dates, direction of mean specified by roll_align}
#'   Default additional columns:
#'   \item{Q3Day}{rolling means of the 3-day flow values of the designated date and previous 2 days (roll_align = "right")}
#'   \item{Q7Day}{rolling means of the 7-day flow values of the designated date and previous 6 days (roll_align = "right")}
#'   \item{Q30Day}{rolling means of the 30-day flow values of the designated date and previous 29 days (roll_align = "right")}
#'    
#' @examples
#' \dontrun{
#' 
#' # Add default 3, 7, and 30-day rolling means, with "right" alignment
#' add_rolling_means(station_number = "08NM116")
#'
#' # Add custom 5 and 10-day rolling means
#' add_rolling_means(station_number = "08NM116",
#'                   roll_days = c(5,10))
#'                   
#' # Add default 3, 7, and 30-day rolling means, with "left" alignment
#' add_rolling_means(station_number = "08NM116",
#'                   roll_align = "left")                
#' }
#' @export


add_rolling_means <- function(data,
                              dates = Date,
                              values = Value,
                              groups = STATION_NUMBER,
                              station_number,
                              roll_days = c(3,7,30),
                              roll_align = "right"){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  
  rolling_days_checks(roll_days, roll_align, multiple = TRUE)

  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Get groups of flow_data to return after
  flow_data_groups <- dplyr::group_vars(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = FALSE)
  
  
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
    for (x in roll_days) {
      flow_data_stn[, paste0("Q", x, "Day")] <- RcppRoll::roll_mean(flow_data_stn$Value, n = x, fill = NA, align = roll_align)
    }
    
    # Return flow_data_stn to original dates
    flow_data_stn <- dplyr::filter(flow_data_stn, Date %in% dates_list)
    
    # Append to flow_data
    flow_data_new <- dplyr::bind_rows(flow_data_new, flow_data_stn)
    
  }
  flow_data <- flow_data_new
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  # Remove the STATION_NUMBER columns if one wasn't in flowdata originally
  if(!as.character(substitute(groups)) %in% orig_cols) {
    flow_data <- dplyr::select(flow_data, -STATION_NUMBER)
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(flow_data_groups))
  
  
  dplyr::as_tibble(flow_data)
} 

