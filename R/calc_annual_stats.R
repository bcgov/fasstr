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

#' @title Calculate annual summary statistics
#'
#' @description Calculates annual monthly mean, median, maximum, minimum, and percentiles of daily flow values from a streamflow 
#'    dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
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
#' @param percentiles a numeric vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}.
#' @param rolling_days a numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param rolling_align a character identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param months a numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).
#' @param transpose a logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' @param ignore_missing a logical value indicating whether dates with missing flow values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{Median}{annual median of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{P'n'}{each annual n-th percentile selected of all daily flows}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile of all daily flows for a given year}
#'   \item{P90}{annual 90th percentile of all daily flows for a given year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_annual_stats(flow_data = flow_data)
#' 
#'calc_annual_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_annual_stats(HYDAT = "08NM116", months = 7:9)
#'
#' }
#' @export


calc_annual_stats <- function(flow_data = NULL,
                              flow_dates = Date,
                              flow_values = Value,
                              flow_stations = STATION_NUMBER,
                              HYDAT = NULL,
                              percentiles = c(10,90),
                              rolling_days = 1,
                              rolling_align = "right",
                              water_year = FALSE,
                              water_year_start = 10,
                              start_year = 0,
                              end_year = 9999,
                              exclude_years = NULL, 
                              months = 1:12,
                              transpose = FALSE,
                              ignore_missing = FALSE){
  
  
  
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
  
  if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
  if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  if(!is.numeric(rolling_days))                        stop("rolling_days argument must be numeric")
  if(!all(rolling_days %in% c(1:180)))                 stop("rolling_days argument must be integers > 0 and <= 180)")
  if(!rolling_align %in% c("right", "left", "center")) stop("rolling_align argument must be 'right', 'left', or 'center'")
  
  
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
  
  # Apply rolling mean if designated, default of 1
  flow_data <- fasstr::add_rolling_means(flow_data, days = rolling_days, align = rolling_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  
  ## CALCULATE STATISTICS
  ## --------------------

  # Calculate basic stats
  Q_annual <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                 Mean = mean(RollingValue, na.rm = ignore_missing),  
                                 Median = median(RollingValue, na.rm = ignore_missing), 
                                 Maximum = max (RollingValue, na.rm = ignore_missing),    
                                 Minimum = min (RollingValue, na.rm = ignore_missing))
  
  # Calculate annual percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      Q_annual_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                         Percentile = quantile(RollingValue, ptile / 100, na.rm = TRUE))
      names(Q_annual_ptile)[names(Q_annual_ptile) == "Percentile"] <- paste0("P", ptile)
      
      # Merge with Q_annual
      Q_annual <- merge(Q_annual, Q_annual_ptile, by = c("STATION_NUMBER","AnalysisYear"))
      
      # Remove percentile if mean is NA (workaround for na.rm=FALSE in quantile)
      Q_annual[,ncol(Q_annual)] <- ifelse(is.na(Q_annual$Mean),NA,Q_annual[,ncol(Q_annual)])
    }
  }
  
  # Rename year column
  Q_annual <-   dplyr::rename(Q_annual,Year=AnalysisYear)
  
  
  # Make excluded years data NA
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    Q_annual[Q_annual$Year %in% exclude_years,-(1:2)] <- NA
  } else {
    Q_annual[Q_annual$Year %in% exclude_years,-1] <- NA
  }
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(Q_annual[-(1:2)])
    
    # Transpose the columns for rows
    Q_annual <- tidyr::gather(Q_annual, Statistic, Value, -STATION_NUMBER, -Year)
    Q_annual <- tidyr::spread(Q_annual, Year, Value)
    
    # Order the columns
    Q_annual$Statistic <- as.factor(Q_annual$Statistic)
    levels(Q_annual$Statistic) <- stat_levels
    Q_annual <- with(Q_annual, Q_annual[order(STATION_NUMBER, Statistic),])
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    names(Q_annual)[names(Q_annual) == "STATION_NUMBER"] <- as.character(substitute(flow_stations))
  } else {
    Q_annual <- dplyr::select(Q_annual, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(Q_annual)
  
}

