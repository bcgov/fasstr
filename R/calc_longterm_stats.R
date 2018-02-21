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

#' @title Calculate the long-term and long-term monthly summary statistics
#'
#' @description Calculates the long-term and long-term monthly mean, median, maximum, minimum, and percentiles of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily values from all years, unless specified.
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
#' @param complete_years Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}.          
#' @param custom_months Numeric vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    If wanting months that overlap calendar years (ex. Oct-Mar), choose water_year and a water_year_month that begins before the first 
#'    month listed. Leave blank for no custom month summary.
#' @param custom_months_label Character string to use as a label of custom months. For example, if choosing months 7:9  you may choose 
#'    "Summer" or "Jul-Sep". Default \code{"Custom-Months"}.
#' @param transpose Logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A data frame with the following columns:
#'   \item{Month}{month of the year, included 'Long-term' for all months, and 'Custom-Months' if selected}
#'   \item{Mean}{mean of all daily data for a given month and long-term over all years}
#'   \item{Median}{median of all daily data for a given month and long-term over all years}
#'   \item{Maximum}{maximum of all daily data for a given month and long-term over all years}
#'   \item{Minimum}{minimum of all daily data for a given month and long-term over all years}
#'   \item{P'n'}{each  n-th percentile selected for a given month and long-term over all years}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile selected for a given month and long-term over all years}
#'   \item{P90}{annual 90th percentile selected for a given month and long-term over all years}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_longterm_stats(data = flow_data)
#' 
#'calc_longterm_stats(data = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_longterm_stats(data = c("08NM116","08NM242"), custom_months = c(5:9))
#'
#' }
#' @export


calc_longterm_stats <- function(data = NULL,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                percentiles = c(10,90),
                                rolling_days = 1,
                                rolling_align = "right",
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = 0,
                                end_year = 9999,
                                exclude_years = NULL,
                                complete_years = FALSE,
                                custom_months = NULL,
                                custom_months_label = "Custom-Months",
                                transpose = FALSE,
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
  if(!all(exclude_years %in% c(0:9999)))  stop("Years listed in exclude_years must be integers.")
  
  if(!is.logical(complete_years))         stop("complete_years argument must be logical (TRUE/FALSE).")
  
  if(!is.null(custom_months) & !is.numeric(custom_months))             
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(custom_months %in% c(1:12)))                                 
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!is.na(custom_months_label) & !is.character(custom_months_label)) 
    stop("custom_months_label argument must be a character string.")
  
  if(!all(is.na(percentiles))){
    if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
    if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  }
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_rolling_means(data = flow_data, days = rolling_days, align = rolling_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
  
  # Remove incomplete years if selected
  if(complete_years){
    comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                   complete_yr = ifelse(sum(!is.na(RollingValue)) == length(AnalysisYear), TRUE, FALSE))
    flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "AnalysisYear"))
    flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
    flow_data <- dplyr::select(flow_data, -complete_yr)
  }
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate the monthly and longterm stats
  Q_months <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                               Mean = mean(RollingValue, na.rm = ignore_missing),
                               Median = stats::median(RollingValue, na.rm = ignore_missing),
                               Maximum = max(RollingValue, na.rm = ignore_missing),
                               Minimum = min(RollingValue, na.rm = ignore_missing))
  longterm_stats   <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER),
                                   Mean = mean(RollingValue, na.rm = ignore_missing),
                                   Median = stats::median(RollingValue, na.rm = ignore_missing),
                                   Maximum = max(RollingValue, na.rm = ignore_missing),
                                   Minimum = min(RollingValue, na.rm = ignore_missing))
  longterm_stats <- dplyr::mutate(longterm_stats, MonthName = as.factor("Long-term"))
  
  longterm_stats <- rbind(dplyr::ungroup(Q_months), dplyr::ungroup(longterm_stats))  #dplyr::bindrows gives unnecessary warnings
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      
      Q_months_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                                         Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                             stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
      longterm_stats_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                           Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                               stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
      longterm_stats_ptile <- dplyr::mutate(longterm_stats_ptile, MonthName = "Long-term")
      
      names(Q_months_ptile)[names(Q_months_ptile) == "Percentile"] <- paste0("P", ptile)
      names(longterm_stats_ptile)[names(longterm_stats_ptile) == "Percentile"] <- paste0("P", ptile)
      
      longterm_stats_ptile <- rbind(dplyr::ungroup(Q_months_ptile), dplyr::ungroup(longterm_stats_ptile))  #dplyr::bindrows gives unnecessary warnings
      
      # Merge with longterm_stats
      longterm_stats <- merge(longterm_stats,longterm_stats_ptile,by=c("STATION_NUMBER", "MonthName"))
    }
  }
  
  # Calculate custom_months is selected, append data to end
  if(is.numeric(custom_months) & all(custom_months %in% c(1:12))) {
    
    # Filter months for those selected and calculate stats
    flow_data_temp <- dplyr::filter(flow_data, Month %in% custom_months)
    Q_months_custom <-   dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                          Mean = mean(RollingValue, na.rm = ignore_missing),
                                          Median = stats::median(RollingValue, na.rm = ignore_missing),
                                          Maximum = max(RollingValue,na.rm = ignore_missing),
                                          Minimum = min(RollingValue,na.rm = ignore_missing))
    Q_months_custom <- dplyr::mutate(Q_months_custom, MonthName = paste0(custom_months_label))
    
    # Calculate percentiles
    if (!all(is.na(percentiles))){
      for (ptile in percentiles) {
        Q_ptile_custom <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                           Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                               stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
        Q_ptile_custom <- dplyr::mutate(Q_ptile_custom, MonthName = paste0(custom_months_label))
        names(Q_ptile_custom)[names(Q_ptile_custom) == "Percentile"] <- paste0("P", ptile)
        
        # Merge with custom stats
        Q_months_custom <- merge(dplyr::ungroup(Q_months_custom), dplyr::ungroup(Q_ptile_custom), by = c("STATION_NUMBER", "MonthName"))
      }
    }
    # Merge with longterm_stats
    longterm_stats <- rbind(longterm_stats, Q_months_custom)
  }
  
  # Rename Month column and reorder to proper levels (set in add_date_vars)
  longterm_stats <- dplyr::rename(longterm_stats, Month = MonthName)
  longterm_stats <- with(longterm_stats, longterm_stats[order(STATION_NUMBER, Month),])
  #  row.names(longterm_stats) <- c(1:nrow(longterm_stats))
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(longterm_stats[-(1:2)])
    
    # Transpose the columns for rows
    longterm_stats <- tidyr::gather(longterm_stats, Statistic, Value, -STATION_NUMBER, -Month)
    longterm_stats <- tidyr::spread(longterm_stats, Month, Value)
    
    # Order the columns
    longterm_stats$Statistic <- factor(longterm_stats$Statistic, levels = stat_levels)
    longterm_stats <- dplyr::arrange(longterm_stats, STATION_NUMBER, Statistic)
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(longterm_stats)[names(longterm_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    longterm_stats <- dplyr::select(longterm_stats, -STATION_NUMBER)
  }
  
  
  
  dplyr::as_tibble(longterm_stats)
  
  
}
