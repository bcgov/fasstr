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

#' @title Add daily cumulative volumetric flows on an annual basis
#'
#' @description Add a column of daily cumulative volumetric flows on an annual basis to a streamflow dataset. Adds the volumetric discharge 
#'    from each day with the previous day(s) for each year, in units of cubic metres. The cumulative flows restart every year.
#'
#' @param flow_data Data frame. A data frame of daily mean flow data. Not required if \code{HYDAT} argument is used.
#' @param flow_dates A column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Default \code{Date}.
#' @param flow_values A column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Default \code{Value}.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flow_data} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' 
#' @return A data frame of the original flow_data or HYDAT data with an additional column:
#'   \item{Cumul_Volume_m3}{cumulative volumetric flows for each day for each year, in units of cubic metres}
#'   
#' @examples
#' \dontrun{
#' 
#'add_cumulative_volume(flow_data = flow_data)
#' 
#'add_cumulative_volume(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_cumulative_volume <- function(flow_data=NULL,
                                  flow_dates=Date,
                                  flow_values=Value,
                                  HYDAT=NULL,
                                  water_year=FALSE,
                                  water_year_start=10){
  
  
  
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
  
  # If no STATION_NUMBER in flow_data, make it so (required for grouping)
  if(!"STATION_NUMBER" %in% colnames(flow_data)) {
    flow_data$STATION_NUMBER <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Temporarily rename the Date and Value columns
  names(flow_data)[names(flow_data) == as.character(substitute(flow_dates))] <- "Date"
  names(flow_data)[names(flow_data) == as.character(substitute(flow_values))] <- "Value"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in flow_data data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  
  ## PREP FLOW_DATA
  ## -----------------
  
  # Fill in missing dates to ensure all years are covered
  flow_data_temp <- fasstr::fill_missing_dates(flow_data = flow_data)
  flow_data_temp <- fasstr::add_date_variables(flow_data = flow_data_temp, water_year = TRUE, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data_temp$AnalysisYear <- flow_data_temp$WaterYear
  }  else {
    flow_data_temp$AnalysisYear <- flow_data_temp$Year
  }
  
  ## ADD VOLUME COLUMN
  ## -----------------
  
  # Caluclate volumes
  flow_data_temp <- dplyr::mutate(dplyr::group_by(flow_data_temp, STATION_NUMBER, AnalysisYear), Cumul_Volume_m3 = cumsum(Value) * 86400)
  flow_data_temp <- dplyr::select(dplyr::ungroup(flow_data_temp), STATION_NUMBER, Date, Cumul_Volume_m3)
  
  # Add column
  flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
  
  
  ## ---------------
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(flow_dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(flow_values))
  
  # Return columns to original order plus new column
  flow_data <-  flow_data[,c(orig_cols,paste("Cumul_Volume_m3"))]
  
  
  flow_data
  
}

