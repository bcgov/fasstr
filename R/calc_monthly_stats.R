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

#' @title Calculate monthly summary statistics
#'
#' @description Calculates monthly mean, median, maximum, minimum, and percentiles for each month of all years of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
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
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}.
#' @param rolling_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param rolling_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.       
#' @param months Numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}). 
#' @param transpose Logical value indicating if each month statistic should be individual rows. Default \code{FALSE}.
#' @param spread Logical value indicating if each month statistic should be the column name. Default \code{FALSE}.
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Month}{month of the year}
#'   \item{Mean}{mean of all daily flows for a given month and year}
#'   \item{Median}{median of all daily flows for a given month and year}
#'   \item{Maximum}{maximum of all daily flows for a given month and year}
#'   \item{Minimum}{minimum of all daily flows for a given month and year}
#'   \item{P'n'}{each n-th percentile selected for a given month and year}
#'   Default percentile columns:
#'   \item{P10}{10th percentile of all daily flows for a given month and year}
#'   \item{P20}{20th percentile of all daily flows for a given month and year}
#'   Transposing data creates a column of "Statistics" for each month, labeled as "Month-Statistic" (ex "Jan-Mean"),
#'   and subsequent columns for each year selected.
#'   Spreading data creates columns of Year and subsequent columns of Month-Statistics  (ex "Jan-Mean").
#'   
#' @examples
#' \dontrun{
#' 
#'calc_monthly_stats(data = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_monthly_stats(data = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_monthly_stats(data = "08NM116", months = 7:9)
#'
#' }
#' @export


calc_monthly_stats <- function(data = NULL,
                               dates = Date,
                               values = Value,
                               groups = STATION_NUMBER,
                               percentiles = c(10,20),
                               rolling_days = 1,
                               rolling_align = "right",
                               water_year = FALSE,
                               water_year_start = 10,
                               start_year = 0,
                               end_year = 9999,
                               exclude_years = NULL,
                               months = 1:12,
                               transpose = FALSE,
                               spread = FALSE,
                               ignore_missing = FALSE){
  
  
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
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using
         'dates' and 'values' arguments.")
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
  
  if(!is.numeric(rolling_days))                        stop("rolling_days argument must be numeric")
  if(!all(rolling_days %in% c(1:180)))                 stop("rolling_days argument must be integers > 0 and <= 180)")
  if(!rolling_align %in% c("right", "left", "center")) stop("rolling_align argument must be 'right', 'left', or 'center'")
  
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
  
  if(!all(is.na(percentiles))){
    if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
    if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  }
  
  if(!is.logical(transpose))  stop("transpose argument must be logical (TRUE/FALSE).")
  if(!is.logical(spread))     stop("spread argument must be logical (TRUE/FALSE).")
  if(transpose & spread)      stop("Both spread and transpose arguments cannot be TRUE.")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Apply rolling mean if designated, default of 1
  flow_data <- add_rolling_means(data = flow_data, days = rolling_days, align = rolling_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate basic stats
  Q_monthly <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, MonthName),
                                Mean = mean(RollingValue, na.rm = ignore_missing),  
                                Median = median(RollingValue, na.rm = ignore_missing), 
                                Maximum = max (RollingValue, na.rm = ignore_missing),    
                                Minimum = min (RollingValue, na.rm = ignore_missing))
  
  # Calculate annual percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      Q_monthly_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, MonthName),
                                          Percentile = quantile(RollingValue, ptile / 100, na.rm = TRUE))
      names(Q_monthly_ptile)[names(Q_monthly_ptile) == "Percentile"] <- paste0("P", ptile)
      
      # Merge with Q_monthly
      Q_monthly <- merge(Q_monthly, Q_monthly_ptile, by = c("STATION_NUMBER", "AnalysisYear", "MonthName"))
      
      # Remove percentile if mean is NA (workaround for na.rm=FALSE in quantile)
      Q_monthly[, ncol(Q_monthly)] <- ifelse(is.na(Q_monthly$Mean), NA, Q_monthly[, ncol(Q_monthly)])
    }
  }
  
  # Rename year column
  Q_monthly <-   dplyr::rename(Q_monthly, Year = AnalysisYear, Month = MonthName)
  
  
  # Set the levels of the months for proper ordering
  if (water_year) {
    if (water_year_start == 1) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    } else if (water_year_start == 2) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"))
    } else if (water_year_start == 3) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb"))
    } else if (water_year_start == 4) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
    } else if (water_year_start == 5) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))
    } else if (water_year_start == 6) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May"))
    } else if (water_year_start == 7) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))
    } else if (water_year_start == 8) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"))
    } else if (water_year_start == 9) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))
    } else if (water_year_start == 10) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
    } else if (water_year_start == 11) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
    } else if (water_year_start == 12) {
      Q_monthly$Month <- factor(Q_monthly$Month, 
                                levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))
    }
  } else {           
    Q_monthly$Month <- factor(Q_monthly$Month,
                              levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  }
  
  # Reorder months and row.names
  Q_monthly <- with(Q_monthly, Q_monthly[order(Year, Month),])
  
  
  # Make excluded years data NA
  if(as.character(substitute(groups)) %in% orig_cols) {
    Q_monthly[Q_monthly$Year %in% exclude_years,-(1:3)] <- NA
  } else {
    Q_monthly[Q_monthly$Year %in% exclude_years,-(1:2)] <- NA
  }
  
  
  # Transform data to chosen format
  # Spread data if selected
  if (spread | transpose) {
    Q_monthly_spread <- dplyr::summarise(dplyr::group_by(Q_monthly, STATION_NUMBER, Year))
    for (mnth in unique(Q_monthly$Month)) {
      Q_monthly_month <- dplyr::filter(Q_monthly, Month == mnth)
      Q_monthly_month <- tidyr::gather(Q_monthly_month, Statistic, Value, 4:ncol(Q_monthly_month))
      Q_monthly_month <- dplyr::mutate(Q_monthly_month, StatMonth = paste0(Month, "_", Statistic))
      Q_monthly_month <- dplyr::select(Q_monthly_month, -Statistic, -Month)
      Qstat_order <- unique(Q_monthly_month$StatMonth)
      Q_monthly_month <- tidyr::spread(Q_monthly_month, StatMonth, Value)
      Q_monthly_month <-  Q_monthly_month[, c("STATION_NUMBER", "Year", Qstat_order)]
      Q_monthly_spread <- merge(Q_monthly_spread, Q_monthly_month, by = c("STATION_NUMBER", "Year"), all = TRUE)
    }
    Q_monthly <- Q_monthly_spread
    
    if(transpose){
      Q_monthly <- tidyr::gather(Q_monthly, Statistic, Value, -(1:2))
    }
  }
  
  Q_monthly <- with(Q_monthly, Q_monthly[order(STATION_NUMBER, Year),])
  row.names(Q_monthly) <- c(1:nrow(Q_monthly))
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(Q_monthly)[names(Q_monthly) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    Q_monthly <- dplyr::select(Q_monthly, -STATION_NUMBER)
  }
  
  
  
  dplyr::as_tibble(Q_monthly)
  
}
