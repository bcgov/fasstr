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
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years with complete annual
#'    data, unless specified.
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
#' @param percent_total Numeric vector of percents of total annual flows to determine dates. Default \code{c(25,33.3,50,75)}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.             
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
#' calc_annual_flow_timing(data = "08NM116", 
#'                         water_year = TRUE, 
#'                         water_year_start = 8, 
#'                         percent_total = 50)
#'
#' }
#' @export


calc_annual_flow_timing <- function(data = NULL,
                                    dates = Date,
                                    values = Value,
                                    groups = STATION_NUMBER,
                                    percent_total = c(25, 33.3, 50, 75),
                                    water_year = FALSE,
                                    water_year_start = 10,
                                    start_year = 0,
                                    end_year = 9999,
                                    exclude_years = NULL, 
                                    transpose = FALSE){
  
  
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
  
  # Save the original columns (to check for groups column later) and ungroup
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no groups (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Gather required columns (will temporarily rename groups column as STATION_NUMBER if isn't already)
  flow_data <- flow_data[,c(as.character(substitute(groups)),
                            as.character(substitute(dates)),
                            as.character(substitute(values)))]
  colnames(flow_data) <- c("STATION_NUMBER","Date","Value")
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in provided data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in provided data frame does not contain numeric values.")
  
  
  
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
  
  if(!is.numeric(percent_total))                    stop("percent_total must be numeric.")
  if(!all(percent_total > 0 & percent_total < 100)) stop("percent_total must be > 0 and < 100).")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_cumulative_volume(flow_data, water_year = water_year, water_year_start = water_year_start)
  
  
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
  timing_stats <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear))
  
  for (percent in percent_total) {
    timing_pcnt <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                        TOTALQ_DAY  =  AnalysisDoY[ match(TRUE, Cumul_Volume_m3 > percent / 100 * 
                                                                            ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))],
                                        TOTALQ_DATE =  Date[ match(TRUE, Cumul_Volume_m3 > percent/100 *
                                                                     ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))])
    names(timing_pcnt)[names(timing_pcnt) == "TOTALQ_DAY"] <- paste0("DoY_", percent, "pct_TotalQ")
    names(timing_pcnt)[names(timing_pcnt) == "TOTALQ_DATE"] <- paste0("Date_", percent, "pct_TotalQ")
    timing_stats <- merge(timing_stats, timing_pcnt, by = c("STATION_NUMBER", "AnalysisYear"), all = TRUE)
    
  }
  timing_stats <-   dplyr::rename(timing_stats,Year=AnalysisYear)
  
  
  # Make excluded years data NA
  if(as.character(substitute(groups)) %in% orig_cols) {
    timing_stats[timing_stats$Year %in% exclude_years,-(1:2)] <- NA
  } else {
    timing_stats[timing_stats$Year %in% exclude_years,-1] <- NA
  }
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    timing_stats <- dplyr::select(timing_stats, STATION_NUMBER, Year, dplyr::contains("DoY"))
    col_names <- names(timing_stats[-(1:2)])
    timing_stats <- tidyr::gather(timing_stats, Statistic, Value, -Year, -STATION_NUMBER)
    timing_stats <- tidyr::spread(timing_stats, Year, Value)
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(timing_stats)[names(timing_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    timing_stats <- dplyr::select(timing_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(timing_stats)
  
}

