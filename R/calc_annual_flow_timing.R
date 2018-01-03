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

#' @title Calculate annual flow timing
#'
#' @description Calculates annual the timing (day of year) and date of occurrence of portions of total annual flow of daily flow 
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @param flow_data a data frame of daily mean flow data that contains columns of dates, flow values, and (optional) station 
#'    names/numbers. Leave blank if using \code{HYDAT} argument.
#' @param flow_dates a column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Leave blank if using \code{HYDAT} 
#'    argument. Default \code{Date}. 
#' @param flow_values a column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Leave blank if using \code{HYDAT} argument. Default \code{Value}.
#' @param flow_stations a column in flow_data that contains station identifiers (names or numbers) for each flow data set, required if 
#'    calculating results for multiple stations. Removing 'STATION_NUMBER' column in flow_data or incorrectly identifying will calculate 
#'    statistics on all flow values from all stations. If using \code{HYDAT} argument, setting \code{flow_stations} to anything besides 
#'    \code{STATION_NUMBER} will have similar effects. Default \code{STATION_NUMBER}. 
#' @param HYDAT a character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the 
#'    \code{tidyhydat} package and a HYDAT database are required. Leave blank if using \code{flow_data} arguments.
#' @param percent_total a numeric vector of percents of total annual flows to determine dates. Default \code{c(25,33.3,50,75)}.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param transpose a logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' 
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{DoY_'n'pct_TotalQ}{day of year for each n-percent of total volumetric discharge}
#'   \item{Date_'n'pct_TotalQ}{date (YYYY-MM-DD) for each n-percent of total volumetric discharge}
#'   Default columns:
#'   \item{DoY_25pct_TotalQ}{day of year of 25-percent of total volumetric discharge}
#'   \item{Date_25pct_TotalQ}{date (YYYY-MM-DD) of 25-percent of total volumetric discharge}
#'   \item{DoY_33.3pct_TotalQ}{day of year of 33.3-percent of total volumetric discharge}
#'   \item{Date_33.3pct_TotalQ}{date (YYYY-MM-DD) of 33.3-percent of total volumetric discharge}
#'   \item{DoY_50pct_TotalQ}{day of year of 50-percent of total volumetric discharge}
#'   \item{Date_50pct_TotalQ}{date (YYYY-MM-DD) of 50-percent of total volumetric discharge}
#'   \item{DoY_75pct_TotalQ}{day of year of 75-percent of total volumetric discharge}
#'   \item{Date_75pct_TotalQ}{date (YYYY-MM-DD) of 75-percent of total volumetric discharge}
#'   Transposing data creates a column of "Statistics" (just DoY, not Date values) and subsequent columns for each year selected.
#' 
#' @examples
#' \dontrun{
#' 
#'calc_annual_flow_timing(flow_data = flow_data)
#' 
#'calc_annual_flow_timing(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percent_total = 50)
#'
#' }
#' @export


calc_annual_flow_timing <- function(flow_data = NULL,
                                    flow_dates = Date,
                                    flow_values = Value,
                                    flow_stations = STATION_NUMBER,
                                    HYDAT = NULL,
                                    percent_total = c(25,33.3,50,75),
                                    water_year = FALSE,
                                    water_year_start = 10,
                                    start_year = 0,
                                    end_year = 9999,
                                    exclude_years = NULL, 
                                    transpose = FALSE){
  
  
  
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
  
  if(!is.numeric(percent_total))                   stop("percent_total must be numeric")
  if(!all(percent_total > 0 & percent_total < 100)) stop("percent_total must be >0 and <100)")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fasstr::fill_missing_dates(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- fasstr::add_date_variables(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- fasstr::add_cumulative_volume(flow_data, water_year = water_year, water_year_start = water_year_start)
  
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
    flow_data$AnalysisDoY <- flow_data$WaterDayofYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
    flow_data$AnalysisDoY <- flow_data$DayofYear
  }
  
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)

  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Loop through percents
  Q_timing <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear))
  
  for (percent in percent_total) {
    Q_timing_pcnt <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                        TOTALQ_DAY  =  AnalysisDoY[ match(TRUE, Cumul_Volume_m3 > percent / 100 * 
                                                                            ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))],
                                        TOTALQ_DATE =  Date[ match(TRUE, Cumul_Volume_m3 > percent/100 *
                                                                     ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))])
    names(Q_timing_pcnt)[names(Q_timing_pcnt) == "TOTALQ_DAY"] <- paste0("DoY_", percent, "pct_TotalQ")
    names(Q_timing_pcnt)[names(Q_timing_pcnt) == "TOTALQ_DATE"] <- paste0("Date_", percent, "pct_TotalQ")
    Q_timing <- merge(Q_timing, Q_timing_pcnt, by = c("STATION_NUMBER", "AnalysisYear"), all = TRUE)
    
  }
  Q_timing <-   dplyr::rename(Q_timing,Year=AnalysisYear)
  
  
  # Make excluded years data NA
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    Q_timing[Q_timing$Year %in% exclude_years,-(1:2)] <- NA
  } else {
    Q_timing[Q_timing$Year %in% exclude_years,-1] <- NA
  }
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    Q_timing <- dplyr::select(Q_timing, STATION_NUMBER, Year, dplyr::contains("DoY"))
    col_names <- names(Q_timing[-(1:2)])
    Q_timing <- tidyr::gather(Q_timing, Statistic, Value, -Year, -STATION_NUMBER)
    Q_timing <- tidyr::spread(Q_timing, Year, Value)
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    names(Q_timing)[names(Q_timing) == "STATION_NUMBER"] <- as.character(substitute(flow_stations))
  } else {
    Q_timing <- dplyr::select(Q_timing, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(Q_timing)
  
}

