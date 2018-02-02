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


#' @title Calculate annual lowflows
#'
#' @description Calculates annual n-day minimum values, and the day of year and date of occurrence of daily flow values from a 
#'    streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
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
#' @param transpose Logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#
#'    
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Min_'n'_Day}{annual minimum for each n-day rolling mean, direction of mean specified by rolling_align}
#'   \item{Min_'n'_Day_DoY}{day of year for each annual minimum of n-day rolling mean}
#'   \item{Min_'n'_Day_Date}{date (YYYY-MM-DD) for each annual minimum of n-day rolling mean}
#'   Default columns:
#'   \item{Min_1_Day}{annual 1-day mean minimum (rolling_align=right)}
#'   \item{Min_1_Day_DoY}{day of year of annual 1-day mean minimum}
#'   \item{Min_1_Day_Date}{date (YYYY-MM-DD) of annual 1-day mean minimum}
#'   \item{Min_3_Day}{annual 3-day mean minimum (rolling_align=right)}
#'   \item{Min_3_Day_DoY}{day of year of annual 3-day mean minimum}
#'   \item{Min_3_Day_Date}{date (YYYY-MM-DD) of annual 3-day mean minimum}   
#'   \item{Min_7_Day}{annual 7-day mean minimum (rolling_align=right)}
#'   \item{Min_7_Day_DoY}{day of year of annual 7-day mean minimum}
#'   \item{Min_7_Day_Date}{date (YYYY-MM-DD) of annual 7-day mean minimum}
#'   \item{Min_30_Day}{annual 30-day mean minimum (rolling_align=right)}
#'   \item{Min_30_Day_DoY}{day of year of annual 30-day mean minimum}
#'   \item{Min_30_Day_Date}{date (YYYY-MM-DD) of annual 30-day mean minimum}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected. "Date" statistics
#'   not transposed.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_annual_lowflows(data = "08NM116", water_year = TRUE, water_year_start = 8, rolling_days = c(3,7))
#'
#' }
#' @export



calc_annual_lowflows <- function(data = NULL,
                                 dates = Date,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 rolling_days = c(1, 3, 7, 30),
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
  if(!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.")
  
  if(!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
    flow_data$AnalysisDoY <- flow_data$WaterDayofYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
    flow_data$AnalysisDoY <- flow_data$DayofYear
  }
  
  # Filter data for one year prior and one year after the selected data for proper rolling means
  flow_data <- dplyr::filter(flow_data, Year >= start_year - 1 & Year <= end_year + 1)
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Loop through each rolling_day and compute annual min values and their dates
  lowflow_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear))
  for (day in rolling_days) {
    # Add specified rolling mean
    flow_data_temp <- fasstr::add_rolling_means(data = flow_data, days = day, align = rolling_align)
    names(flow_data_temp)[names(flow_data_temp) == paste0("Q", day, "Day")] <- "RollingValue"
    
    # Filter for selected months
    flow_data_temp <- dplyr::filter(flow_data_temp, Month %in% months)
    
    # Calculate the mins and dates
    lowflow_stats_temp <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER, AnalysisYear),
                                           MIN_VALUE = min(RollingValue, na.rm = ignore_missing),	     
                                           MIN_DAY = ifelse(is.na(MIN_VALUE), NA, AnalysisDoY[which(RollingValue == MIN_VALUE)]),
                                           MIN_DATE= ifelse(is.na(MIN_VALUE), NA, Date[which(RollingValue == MIN_VALUE)]))
    class(lowflow_stats_temp$MIN_DATE) <- "Date" # fixes ifelse and date issue
    names(lowflow_stats_temp)[names(lowflow_stats_temp) == "MIN_VALUE"] <- paste0("Min_", day, "_Day")
    names(lowflow_stats_temp)[names(lowflow_stats_temp) == "MIN_DAY"] <- paste0("Min_", day, "_Day_DoY")
    names(lowflow_stats_temp)[names(lowflow_stats_temp) == "MIN_DATE"] <- paste0("Min_", day, "_Day_Date")
    
    lowflow_stats <- merge(lowflow_stats, lowflow_stats_temp, by = c("STATION_NUMBER", "AnalysisYear"), all = TRUE)
  }
  lowflow_stats <-   dplyr::rename(lowflow_stats, Year = AnalysisYear)
  
  # Filter for start and end years and make excluded years data NA
  lowflow_stats <- dplyr::filter(lowflow_stats, Year >= start_year & Year <= end_year)
  lowflow_stats[lowflow_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    
    # Remove the dates
    lowflow_stats <- dplyr::select(lowflow_stats, STATION_NUMBER, Year, dplyr::contains("Day"), -dplyr::contains("Date"))
    
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(lowflow_stats[-(1:2)])
    lowflow_stats <- tidyr::gather(lowflow_stats, Statistic, Value, -STATION_NUMBER, -Year)
    lowflow_stats <- tidyr::spread(lowflow_stats, Year, Value)
    
    # Order the columns
    lowflow_stats$Statistic <- factor(lowflow_stats$Statistic, levels = stat_levels)
    lowflow_stats <- dplyr::arrange(lowflow_stats, STATION_NUMBER, Statistic)
  }
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(lowflow_stats)[names(lowflow_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    lowflow_stats <- dplyr::select(lowflow_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(lowflow_stats)
  
  
}

