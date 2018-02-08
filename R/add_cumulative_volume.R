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
#' @description Add a column of daily cumulative volumetric flows on an annual basis to a streamflow dataset. Adds the volumetric 
#'    discharge from each day with the previous day(s) for each year, in units of cubic metres. The cumulative flows restart every year.
#'
#' @param data A data frame of daily data that contains columns of dates, values, and (optional) groups (ex. station 
#'    names/numbers). Leave blank if using \code{station_number} argument.
#' @param dates Column in \code{data} that contains dates formatted YYYY-MM-DD. Only required if dates column name is not '
#'    Date' (default). Leave blank if using \code{station_number} argument.
#' @param values Column in \code{data} that contains numeric flow values, in units of cubic metres per second. 
#'    Only required if values column name is not 'Value' (default). Leave blank if using \code{station_number} argument.
#' @param groups Column in \code{data} that contains unique identifiers for different data sets. Only required if groups 
#'    column name is not 'STATION_NUMBER'. Function will automatically group by a column named 'STATION_NUMBER' if present.
#'    Remove the 'STATION_NUMBER' column or identify another non-existing column name to remove this grouping.
#'    Leave blank if using \code{station_number} argument.
#' @param station_number A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of 
#'    which to extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database. 
#'    Leave blank if using \code{data} argument.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' 
#' @return A tibble data frame of the source data with an additional column:
#'   \item{Cumul_Volume_m3}{cumulative volumetric flows for each day for each year, in units of cubic metres}
#'   
#' @examples
#' \dontrun{
#' 
#'add_cumulative_volume(data = flow_data)
#' 
#'add_cumulative_volume(station_number = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_cumulative_volume <- function(data = NULL,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number = NULL,
                                  water_year = FALSE,
                                  water_year_start = 10){
  
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Get groups of flow_data to return after
  orig_groups <- dplyr::group_vars(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)))
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  water_year_checks(water_year, water_year_start)
  
  
  ## PREP FLOW_DATA
  ## -----------------
  
  # Fill in missing dates to ensure all years are covered
  flow_data_temp <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data_temp <- add_date_variables(data = flow_data_temp, water_year = water_year, water_year_start = water_year_start)
  
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
  flow_data_temp <- dplyr::mutate(dplyr::group_by(flow_data_temp, STATION_NUMBER, AnalysisYear, add = TRUE), Cumul_Volume_m3 = cumsum_na(Value) * 86400)
  flow_data_temp <- dplyr::ungroup(flow_data_temp)
  
  # Get new column and merge back with
  flow_data_temp <- dplyr::select(flow_data_temp, STATION_NUMBER, Date, Cumul_Volume_m3)
  if("Cumul_Volume_m3" %in% orig_cols){
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
    flow_data$Cumul_Volume_m3 <- flow_data$Cumul_Volume_m3.y
    flow_data <- dplyr::select(flow_data, -Cumul_Volume_m3.y, -Cumul_Volume_m3.x)
  } else {
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
  }
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  # Return columns to original order plus new column
  if("Cumul_Volume_m3" %in% orig_cols){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, paste("Cumul_Volume_m3"))]
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(orig_groups))
  
  dplyr::as_tibble(flow_data)
  
}

