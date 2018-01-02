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

#' @title Calculate the long-term mean annual discharge
#'
#' @description Calculates the long-term mean annual discharge of a streamflow dataset. Averages all daily discharge values from all years,
#'   unless specified.
#'
#' @param flow_data a data frame of daily mean flow data that contains columns of dates, flow values, and (optional) station 
#'    names/numbers. Leave blank if using \code{HYDAT} argument.
#' @param flow_dates a column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Leave blank if using \code{HYDAT} 
#'    argument. Default \code{Date}. 
#' @param flow_values a column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Leave blank if using \code{HYDAT} argument. Default \code{Value}.
#' @param flow_stations a column in flow_data that contains station identifiers for each flow data set.
#'    Removing 'STATION_NUMBER' column in flow_data or incorrectly identifying will calculate statistics on all flow values from all stations.
#'    If using \code{HYDAT} argument, setting \code{flow_stations} to anything besides \code{STATION_NUMBER} will have similar effects.
#'    Default \code{STATION_NUMBER}. 
#' @param HYDAT a character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the 
#'    \code{tidyhydat} package and a HYDAT database are required. Leave blank if using \code{flow_data} arguments.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param months a numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).
#' @param percent_MAD a numeric vector of percents of long-term mean annual discharge to add to the table (ex. 20 for 20 percent MAD).
#'    Leave blank for no values to be calculated.
#'
#' @return A tibble of numeric values of a long-term mean (and percent of long-term mean if selected) of selected years and months.
#' 
#' @examples
#' \dontrun{
#' 
#' calc_lt_mad(flow_data = data, start_year = 1980, end_year = 2010)
#' 
#' calc_lt_mad(HYDAT = "08NM116",water_year = TRUE, exclude_years = (1990, 1992:1994))
#' 
#' calc_lt_mad(HYDAT = "08NM116", percent_MAD = 20)
#' 
#' }
#' @export


calc_lt_mad <- function(flow_data = NULL,
                        flow_dates = Date,
                        flow_values = Value,
                        flow_stations = STATION_NUMBER,
                        HYDAT = NULL,
                        water_year = FALSE,
                        water_year_start = 10,
                        start_year = 0,
                        end_year = 9999,
                        exclude_years = NULL, 
                        months = 1:12,
                        #ignore_missing????? only complete annual years of data
                        percent_MAD = NA){

  
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
  
  # Save the original columns (to check for STATION_NUMBER later) and ungroup
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for station grouping)
  if(!as.character(substitute(flow_stations)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(flow_stations))] <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Gather required columns
  flow_data <- flow_data[,c(as.character(substitute(flow_stations)),
                            as.character(substitute(flow_dates)),
                            as.character(substitute(flow_values)))]
  colnames(flow_data) <- c("STATION_NUMBER","Date","Value")
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in flow_data data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  if(length(start_year)>1)        stop("Only one start_year value can be listed")
  if(!start_year %in% c(0:9999))  stop("start_year must be an integer.")
  if(length(end_year)>1)          stop("Only one end_year value can be listed")
  if(!end_year %in% c(0:9999))    stop("end_year must be an integer.")
  if(start_year > end_year)       stop("start_year must be less than or equal to end_year.")
  
  if(!is.null(exclude_years) & !is.numeric(exclude_years)) stop("List of exclude_years must be numeric - ex. 1999 or c(1999,2000).")
  if(!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.")
  
  if(!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  
  if(!all(is.na(percent_MAD)) & all(percent_MAD <= 0))  stop("percent_MAD argument must be a single number > 0.")

  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fasstr::fill_missing_dates(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- fasstr::add_date_variables(flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  stat_ltmad <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                 LTMAD = mean(Value, na.rm = T))
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percent_MAD))) {
    for (pcnt in percent_MAD) {
      stat_ltmad <- dplyr::mutate(stat_ltmad,
                                  Percent = LTMAD * pcnt / 100)
      names(stat_ltmad)[names(stat_ltmad) == "Percent"] <- paste0("PctMAD_", pcnt)
      
    }
  }

  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    names(stat_ltmad)[names(stat_ltmad) == "STATION_NUMBER"] <- as.character(substitute(flow_stations))
  } else {
    stat_ltmad <- dplyr::select(stat_ltmad, -STATION_NUMBER)
  }
  
  
  stat_ltmad
  
}
