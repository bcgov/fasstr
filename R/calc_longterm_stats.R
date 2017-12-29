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
#'    from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
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
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param custom_months a numeric vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    If wanting months that overlap calendar years (ex. Oct-Mar), choose water_year and a water_year_month that begins before the first 
#'    month listed. Leave blank for no custom month summary.
#' @param custom_months_label a character string to use as a label of custom months. For example, if choosing months 7:9  you may choose 
#'    "Summer" or "Jul-Sep". Default \code{"Custom-Months"}.
#' @param transpose a logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' @param ignore_missing a logical value indicating whether dates with missing flow values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A tibble data frame with the following columns:
#'   \item{Month}{month of the year, included Long-term for all months, and Custom-Months if selected}
#'   \item{Mean}{mean of all daily flows for a given month and longterm over all years}
#'   \item{Median}{median of all daily flows for a given month and longterm over all years}
#'   \item{Maximum}{maximum of all daily flows for a given month and longterm over all years}
#'   \item{Minimum}{minimum of all daily flows for a given month and longterm over all years}
#'   \item{P'n'}{each  n-th percentile selected for a given month and longterm over all years}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile selected for a given month and longterm over all years}
#'   \item{P90}{annual 90th percentile selected for a given month and longterm over all years}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_longterm_stats(flow_data = flow_data)
#' 
#'calc_longterm_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_longterm_stats(HYDAT = c("08NM116","08NM242"), custom_months = c(5:9))
#'
#' }
#' @export


calc_longterm_stats <- function(flow_data = NULL,
                                flow_dates = Date,
                                flow_values = Value,
                                flow_stations = STATION_NUMBER,
                                HYDAT = NULL,
                                percentiles = c(10,90),
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = 0,
                                end_year = 9999,
                                exclude_years = NULL,
                                custom_months = NULL,
                                custom_months_label = "Custom-Months",
                                transpose = FALSE,
                                ignore_missing = TRUE){
  
  
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
  if(!all(exclude_years %in% c(0:9999)))  stop("Years listed in exclude_years must be integers.")
  
  if(!is.null(custom_months) & !is.numeric(custom_months))             
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(custom_months %in% c(1:12)))                                 
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!is.na(custom_months_label) & !is.character(custom_months_label)) 
    stop("custom_months_label argument must be a character string.")
  
  if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
  if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  
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
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate the monthly and longterm stats
  Q_months <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                               Mean = mean(Value, na.rm = ignore_missing),
                               Median = median(Value, na.rm = ignore_missing),
                               Maximum = max(Value, na.rm = ignore_missing),
                               Minimum = min(Value, na.rm = ignore_missing))
  Q_longterm   <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER),
                                   Mean = mean(Value, na.rm = ignore_missing),
                                   Median = median(Value, na.rm = ignore_missing),
                                   Maximum = max(Value, na.rm = ignore_missing),
                                   Minimum = min(Value, na.rm = ignore_missing))
  Q_longterm <- dplyr::mutate(Q_longterm, MonthName = as.factor("Long-term"))
  
  Q_longterm <- rbind(dplyr::ungroup(Q_months), dplyr::ungroup(Q_longterm))  #dplyr::bindrows gives unnecessary warnings
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      
      Q_months_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                                         Percentile = ifelse(!is.na(mean(Value, na.rm = FALSE)) | ignore_missing, 
                                                             quantile(Value, ptile / 100, na.rm = TRUE), NA))
      Q_longterm_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                           Percentile = ifelse(!is.na(mean(Value, na.rm = FALSE)) | ignore_missing, 
                                                               quantile(Value, ptile / 100, na.rm = TRUE), NA))
      Q_longterm_ptile <- dplyr::mutate(Q_longterm_ptile, MonthName = "Long-term")
      
      names(Q_months_ptile)[names(Q_months_ptile) == "Percentile"] <- paste0("P", ptile)
      names(Q_longterm_ptile)[names(Q_longterm_ptile) == "Percentile"] <- paste0("P", ptile)
      
      Q_longterm_ptile <- rbind(dplyr::ungroup(Q_months_ptile), dplyr::ungroup(Q_longterm_ptile))  #dplyr::bindrows gives unnecessary warnings
      
      # Merge with Q_longterm
      Q_longterm <- merge(Q_longterm,Q_longterm_ptile,by=c("STATION_NUMBER", "MonthName"))
    }
  }
  
  # Calculate custom_months is selected, append data to end
  if(is.numeric(custom_months) & all(custom_months %in% c(1:12))) {
    
    # Filter months for those selected and calculate stats
    flow_data_temp <- dplyr::filter(flow_data, Month %in% custom_months)
    Q_months_custom <-   dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                          Mean = mean(Value, na.rm = ignore_missing),
                                          Median = median(Value, na.rm = ignore_missing),
                                          Maximum = max(Value,na.rm = ignore_missing),
                                          Minimum = min(Value,na.rm = ignore_missing))
    Q_months_custom <- dplyr::mutate(Q_months_custom, MonthName = paste0(custom_months_label))
    
    # Calculate percentiles
    if (!all(is.na(percentiles))){
      for (ptile in percentiles) {
        Q_ptile_custom <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                           Percentile = ifelse(!is.na(mean(Value, na.rm = FALSE)) | ignore_missing, 
                                                               quantile(Value, ptile / 100, na.rm = TRUE), NA))
        Q_ptile_custom <- dplyr::mutate(Q_ptile_custom, MonthName = paste0(custom_months_label))
        names(Q_ptile_custom)[names(Q_ptile_custom) == "Percentile"] <- paste0("P", ptile)
        
        # Merge with custom stats
        Q_months_custom <- merge(dplyr::ungroup(Q_months_custom), dplyr::ungroup(Q_ptile_custom), by = c("STATION_NUMBER", "MonthName"))
      }
    }
    # Merge with Q_longterm
    Q_longterm <- rbind(Q_longterm, Q_months_custom)
  }
  
  # Rename Month column and reorder to proper levels (set in add_date_vars)
  Q_longterm <- dplyr::rename(Q_longterm, Month = MonthName)
  Q_longterm <- with(Q_longterm, Q_longterm[order(STATION_NUMBER, Month),])
  #  row.names(Q_longterm) <- c(1:nrow(Q_longterm))
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(Q_longterm[-(1:2)])
    
    # Transpose the columns for rows
    Q_longterm <- tidyr::gather(Q_longterm, Statistic, Value, -STATION_NUMBER, -Month)
    Q_longterm <- tidyr::spread(Q_longterm, Month, Value)
    
    # Order the columns
    Q_longterm$Statistic <- as.factor(Q_longterm$Statistic)
    levels(Q_longterm$Statistic) <- stat_levels
    Q_longterm <- with(Q_longterm, Q_longterm[order(STATION_NUMBER, Statistic),])
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(flow_stations)) %in% orig_cols) {
    names(Q_longterm)[names(Q_longterm) == "STATION_NUMBER"] <- as.character(substitute(flow_stations))
  } else {
    Q_longterm <- dplyr::select(Q_longterm, -STATION_NUMBER)
  }
  
  
  
  dplyr::as_tibble(Q_longterm)
  
  
}
