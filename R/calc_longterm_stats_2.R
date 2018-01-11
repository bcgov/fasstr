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
#' @param data Daily data to be analyzed. There are two options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groupings (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param grouping Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and grouping column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.       
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


calc_longterm_stats_2 <- function(data = NULL,
                                  dates = Date,
                                  values = Value,
                                  grouping = STATION_NUMBER,
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
  
  # Save the original columns (to check for grouping column later) and ungroup
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no grouping (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(grouping)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(grouping))] <- "XXXXXXX"
  }
  
  # Get the just grouping (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using
         'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Gather required columns (will temporarily rename grouping column as STATION_NUMBER if isn't already)
  flow_data <- flow_data[,c(as.character(substitute(grouping)),
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
  if(as.character(substitute(grouping)) %in% orig_cols) {
    names(Q_longterm)[names(Q_longterm) == "STATION_NUMBER"] <- as.character(substitute(grouping))
  } else {
    Q_longterm <- dplyr::select(Q_longterm, -STATION_NUMBER)
  }
  
  
  
  dplyr::as_tibble(Q_longterm)
  
  
}
