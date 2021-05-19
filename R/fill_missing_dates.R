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


#' @title Fills data gaps of missing dates
#'
#' @description Fills data gaps of missing dates of the data provided. Builds a continuous data set from the start date to the end date.
#'    Only missing dates are filled, columns not specified as dates or groups will be filled with NA. Will completely fill first and 
#'    last years, unless specified using \code{fill_end_years = FALSE}.
#'
#' @inheritParams calc_annual_stats
#' @param values Name of column in \code{data} that contains numeric flow values, in units of cubic metres per second. Not required as
#'   of fasstr 0.3.3 as all other columns are filled with \code{NA}.
#' @param fill_end_years Logical value indicating whether to fill incomplete start and end years with rows of dates. 
#'    If \code{FALSE} then only missing dates between the provided start and end dates will be filled. Default \code{TRUE}.
#'  
#' @return A tibble data frame of the source data with additional rows where mising dates existed.
#'
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Fill missing dates with NA using calendar years
#' fill_missing_dates(station_number = "08NM116")
#' 
#' # Fill missing dates with NA using water years starting in August
#' fill_missing_dates(station_number = "08NM116", 
#'                    water_year_start = 8)
#'                    
#' }
#' @export


fill_missing_dates <- function(data,
                               dates = Date,
                               values = Value,
                               groups = STATION_NUMBER,
                               station_number,
                               water_year_start = 1,
                               fill_end_years = TRUE){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (!is.logical(fill_end_years[1]))        
    stop("fill_end_years must be logical (TRUE/FALSE).", call. = FALSE)
  
  if (as.character(substitute(values)) != "Value") 
    message("values argument is deprected for this function and not required. values still filled with NA if provided.")
  
  water_year_checks(water_year_start)
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Save the original columns and groups to return at the end
  orig_cols <- names(flow_data)
  orig_groups <- dplyr::group_vars(flow_data)
  
  # Check and rename columns
  flow_data <- format_dates_col(data = flow_data, dates = as.character(substitute(dates)))
  flow_data <- format_groups_col(data = flow_data, groups = as.character(substitute(groups)))
  
  
  ## FILL IN GAPS
  ## ------------
  
  # Loop through each station number, fill in gaps and append
  flow_data <- dplyr::bind_rows(
    lapply(unique(flow_data$STATION_NUMBER), function(stn){
      
      # Filter for station number
      flow_data_stn <- dplyr::filter(flow_data, STATION_NUMBER == stn)
      flow_data_stn <- flow_data_stn[order(flow_data_stn$Date), ]
      
      # Get the start/end dates
      start_date <- min(flow_data_stn$Date, na.rm = TRUE)
      end_date <- max(flow_data_stn$Date, na.rm = TRUE)
      
      # Override start/end dates if filling end years
      if (fill_end_years[1]) {
        
        min_month <- lubridate::month(min(flow_data_stn$Date, na.rm = TRUE))
        min_year <- lubridate::year(min(flow_data_stn$Date, na.rm = TRUE))
        start_date <- as.Date(paste(ifelse(min_month < water_year_start, min_year - 1, min_year),
                                    water_year_start, '01', sep = '-'), "%Y-%m-%d")
        
        max_month <- lubridate::month(max(flow_data_stn$Date, na.rm = TRUE))
        max_year <- lubridate::year(max(flow_data_stn$Date, na.rm = TRUE))
        end_date <- as.Date(paste(ifelse(max_month > water_year_start, max_year + 1, max_year),
                                  water_year_start, '01', sep = '-'), "%Y-%m-%d") - 1
        
      }
      
      # Fill in missing dates
      flow_data_stn <- merge(flow_data_stn, data.frame(Date = seq(start_date, end_date, 1)), all.y = TRUE)
      
      # Fill in station number and parameter gaps (will be removed if not originally there)
      flow_data_stn$STATION_NUMBER <- stn
      flow_data_stn$Parameter <- "Flow"
      
      # Return and bind
      flow_data_stn
    }))
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the columns
  if (is.null(station_number)) {
    names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
    names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  }
  
  #Return columns to original order
  flow_data <-  flow_data[, orig_cols]
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(orig_groups))
  
  
  dplyr::as_tibble(flow_data)
} 
