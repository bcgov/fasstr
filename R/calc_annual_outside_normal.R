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

#' @title Calculate annual days above and below normal
#'
#' @description Calculates the number of days per year outside of the 'normal' range (typically between 25 and 75th percentiles) for
#'    each day of the year. Upper and lower-range percentiles are calcuated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. All days above or below the normal range are included. Calculates the statistics 
#'    from all daily discharge values from all years, unless specified.
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
#' @param normal_percentiles Numeric vector of two values, lower and upper percentiles, respectively indicating the limits of the 
#'    normal range. Default \code{c(25,75)}.
#' @param roll_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param roll_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
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
#' 
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Days_Below_Normal}{number of days per year below the daily normal (default 25th percentile)}
#'   \item{Days_Above_Normal}{number of days per year above the daily normal (default 75th percentile)}
#'   \item{Days_Outside_Normal}{number of days per year below and above the daily normal (default 25/75th percentile)}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#' calc_annual_outside_normal(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



calc_annual_outside_normal <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       groups = STATION_NUMBER,
                                       normal_percentiles = c(25, 75),
                                       roll_days = 1,
                                       roll_align = "right",
                                       water_year = FALSE,
                                       water_year_start = 10,
                                       start_year = 0,
                                       end_year = 9999,
                                       exclude_years = NULL, 
                                       months = 1:12,
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
  
  if(!is.numeric(roll_days))                        stop("roll_days argument must be numeric")
  if(!all(roll_days %in% c(1:180)))                 stop("roll_days argument must be integers > 0 and <= 180)")
  if(!roll_align %in% c("right", "left", "center")) stop("roll_align argument must be 'right', 'left', or 'center'")
  
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
  
  if(!is.numeric(normal_percentiles) )                stop("normal_percentiles must be numeric.")
  if(length(normal_percentiles) != 2 )                stop("normal_percentiles must be two percentile values (ex. c(25,75)).")
  if(normal_percentiles[1] >= normal_percentiles[2] ) stop("normal_percentiles[1] must be < normal_percentiles[2].")
  if(!all(is.na(normal_percentiles)) & (!all(normal_percentiles > 0 & normal_percentiles < 100)) )  
    stop("normal_percentiles must be >0 and <100)")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
    flow_data$AnalysisDoY <- flow_data$WaterDayofYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
    flow_data$AnalysisDoY <- flow_data$DayofYear
  }
  
  # Filter the data for the start and end years
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::mutate(flow_data, Value = replace(Value, AnalysisYear %in% exclude_years, NA))
  
  # Determine years with complete data and filter for only those years
  comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                 complete_yr = ifelse(sum(!is.na(RollingValue)) == length(AnalysisYear), TRUE, FALSE))
  flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "AnalysisYear"))
  flow_data <- dplyr::mutate(flow_data, Value = replace(Value, complete_yr == "FALSE", NA))

  
  ## CALCULATE STATISTICS
  ## --------------------
  
  #Compute the normal limits for each day of the year and add each to the flow_data
  daily_normals <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisDoY),
                                    LOWER = stats::quantile(RollingValue, prob = normal_percentiles[1] / 100, na.rm = TRUE),
                                    UPPER = stats::quantile(RollingValue, prob = normal_percentiles[2] / 100, na.rm = TRUE))
  daily_normals <- dplyr::ungroup(daily_normals)
  flow_data_temp <- merge(flow_data, daily_normals, by = c("STATION_NUMBER", "AnalysisDoY"))
  
  #Compute the number of days above and below normal for each year
  normals_stats <- dplyr::summarise(dplyr::group_by(flow_data_temp, STATION_NUMBER, AnalysisYear),
                            Days_Below_Normal = sum(Value < LOWER, na.rm = FALSE),
                            Days_Above_Normal = sum(Value > UPPER, na.rm = FALSE),
                            Days_Outside_Normal = Days_Below_Normal + Days_Above_Normal)
  normals_stats <- dplyr::ungroup(normals_stats)
  normals_stats <- dplyr::rename(normals_stats, Year = AnalysisYear)
  
  
  #Remove any excluded
  normals_stats[normals_stats$Year %in% exclude_years, -(1:2)] <- NA

  # Transpose data if selected
  if(transpose){
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(normals_stats[-(1:2)])

    normals_stats <- tidyr::gather(normals_stats, Statistic, Value, -Year, -STATION_NUMBER)
    normals_stats <- tidyr::spread(normals_stats, Year, Value)

    # Order the columns
    normals_stats$Statistic <- factor(normals_stats$Statistic, levels = stat_levels)
    normals_stats <- dplyr::arrange(normals_stats, STATION_NUMBER, Statistic)
  }


  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(normals_stats)[names(normals_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    normals_stats <- dplyr::select(normals_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(normals_stats)
  
  
}

