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

#' @title Add daily cumulative runoff yield flows on an annual basis
#'
#' @description Add a column of daily cumulative runoff yield flows on an annual basis to a streamflow dataset. Adds the runoff yield 
#'    discharge from each day with the previous day(s) for each year, in units of millimetres. The cumulative flows restart every year.
#'    Converts cumulative discharge to a depth of water based on the upstream drainge basin area.
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
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each groups factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' 
#' @return A data frame of the source data with an additional column:
#'   \item{Cumul_Yield_mm}{cumulative yield flows for each day for each year, in units of millimetres}
#'
#' @examples
#' \dontrun{
#' 
#'add_cumulative_yield(data = flow_data, area = 105.6)
#' 
#'add_cumulative_yield(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_cumulative_yield <- function(data = NULL,
                                 dates = Date,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 basin_area = NA,
                                 water_year = FALSE,
                                 water_year_start = 10){
  
  
  
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
  
  # If no groups (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
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
  
  
  ## SET UP BASIN AREA
  ## -----------------
  
  suppressWarnings(flow_data <- fasstr::add_basin_area(flow_data, basin_area = basin_area))
  flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  
  ## PREP FLOW_DATA
  ## -----------------
  
  # Fill in missing dates to ensure all years are covered
  flow_data_temp <- fill_missing_dates(data = flow_data)
  flow_data_temp <- add_date_variables(data = flow_data_temp, water_year = TRUE, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data_temp$AnalysisYear <- flow_data_temp$WaterYear
  }  else {
    flow_data_temp$AnalysisYear <- flow_data_temp$Year
  }
  
  ## ADD VOLUME COLUMN
  ## -----------------
  
  # Create cumsum function to not create cumsum if any NA's in a given year
  cumsum_na <- function(x) {
    if (any(is.na(x))) {
      return(rep(NA, length(x)))
    } else {
      cumsum(x)
    }
  }
  
  # Add cumulative volume column and ungroup (remove analysisyear group)
  flow_data_temp <- dplyr::ungroup(flow_data_temp)
  flow_data_temp <- dplyr::mutate(dplyr::group_by(flow_data_temp, STATION_NUMBER, AnalysisYear, add = TRUE), Cumul_Yield_mm = cumsum_na(Value) * 86400 / (Basin_Area_sqkm_temp * 1000))
  flow_data_temp <- dplyr::ungroup(flow_data_temp)
  
  # Get new column and merge back with
  flow_data_temp <- dplyr::select(flow_data_temp, STATION_NUMBER, Date, Cumul_Yield_mm)
  if("Cumul_Yield_mm" %in% orig_cols){
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
    flow_data$Cumul_Yield_mm <- flow_data$Cumul_Yield_mm.y
    flow_data <- dplyr::select(flow_data,-Cumul_Yield_mm.y,-Cumul_Yield_mm.x)
  } else {
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
  }

  
  ## ---------------
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  # Return columns to original order plus new column
  if("Cumul_Yield_mm" %in% orig_cols){
    flow_data <-  flow_data[,c(orig_cols)]
  } else {
    flow_data <-  flow_data[,c(orig_cols, paste("Cumul_Yield_mm"))]
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(flow_data_groups))
  
  
  
  dplyr::as_tibble(flow_data)
  
  
}

