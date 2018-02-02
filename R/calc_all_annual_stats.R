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



#' @title Calculate all fasstr annual statistics
#'
#' @description Calculates all annual statistics of daily flow values from a streamflow dataset from all annual fasstr functions. 
#'    Calculates the statistics from all daily discharge values from all years, unless specified. Data is ideally long-term and continuous 
#'    with minimal missing/seasonal data as annual statistics are calculated. Data calculated using the folling functions:
#' \itemize{
#'  \item{calc_annual_stats()}
#'  \item{calc_annual_lowflows()}
#'  \item{calc_annual_total_flows()}
#'  \item{calc_annual_flow_timing()}
#'  \item{calc_monthly_stats()}
#'  \item{calc_annual_outside_normal()}
#'  }
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
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each grouping factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.             
#' @param annual_percentiles Numeric vector of percentiles to calculate annually. Set to NA if none required. Used for calc_annual_stats()
#'    function. Default \code{c(10,90)}.
#' @param monthly_percentiles Numeric vector of percentiles to calculate monthly for each year. Set to NA if none required. Used for 
#'    calc_monthly_stats() function. Default \code{c(10,90)}.
#' @param stats_days Numeric vector of the number of days to apply a rolling mean on basic stats. Default \code{c(1)}.
#'    Used for calc_annual_stats() and calc_monthly_stats() functions.
#' @param stats_align Character string identifying the direction of the rolling mean on basic stats from the specified date, either by 
#'    the first ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#'    Used for calc_annual_stats(), calc_monthly_stats(), and calc_annual_outside_normal() functions.
#' @param lowflow_days Numeric vector of the number of days to apply a rolling mean on lowflow stats. Default \code{c(1,3,7,30)}.
#'    Used for calc_lowflow_stats() function.
#' @param lowflow_align Character string identifying the direction of the rolling mean on lowflow stats from the specified date, either by 
#'    the first ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#'    Used for calc_lowflow_stats() function.
#' @param timing_percent Numeric vector of percents of annual total flows to determine dates. Used for calc_annual_flow_timing() function. 
#'    Default \code{c(25,33.3,50,75)}.
#' @param normal_percentiles Numeric. Lower and upper percentiles, respectively indicating the limits of the normal range. 
#'    Used for calc_annual_outside_normal() function. Default \code{c(25,75)}.
#' @param transpose Logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A data frame with column "Year" and then 107 (default) variables from the fasstr annual functions.
#'    See listed functions above for default variables. Transposing data creates a column of "Statistics" and subsequent
#'    columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#'calc_all_annual_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_all_annual_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------

calc_all_annual_stats <- function(data = NULL,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  basin_area = NA, 
                                  water_year = FALSE,
                                  water_year_start = 10,
                                  start_year = 0,
                                  end_year = 9999,
                                  exclude_years = NULL,
                                  annual_percentiles = c(10,90),
                                  monthly_percentiles = c(10,20),
                                  stats_days = c(1,3,7,30),
                                  stats_align = "right",
                                  lowflow_days = c(1,3,7,30),
                                  lowflow_align = "right",
                                  timing_percent = c(25,33,50,75),
                                  normal_percentiles = c(25,75),
                                  transpose = FALSE,
                                  ignore_missing = FALSE){
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(is.vector(data)) {
    if(!all(data %in% dplyr::pull(tidyhydat::allstations[1]))) 
      stop("One or more stations numbers listed in data argument do not exist in HYDAT. Re-check numbers or provide a data frame of data.")
    data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
  } 
  
  if(!is.data.frame(data)) stop("Incorrect selection for data argument, must provide a data frame or HYDAT station number(s).")
  data <- as.data.frame(data) # Getting random 'Unknown or uninitialised column:' warnings if using tibble
  
  # Save the original columns (to check for groups column later) and ungroup
  orig_cols <- names(data)
  data <- dplyr::ungroup(data)
  
  # If no groups (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(groups)) %in% colnames(data)) {
    data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(data) & !as.character(substitute(dates)) %in% names(data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Gather required columns (will temporarily rename groups column as STATION_NUMBER if isn't already)
  data <- data[,c(as.character(substitute(groups)),
                  as.character(substitute(dates)),
                  as.character(substitute(values)))]
  colnames(data) <- c("STATION_NUMBER","Date","Value")
  
  # Check columns are in proper formats
  if(!inherits(data$Date[1], "Date"))  stop("'Date' column in provided data frame does not contain dates.")
  if(!is.numeric(data$Value))          stop("'Value' column in provided data frame does not contain numeric values.")
  
  
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
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(ignore_missing))  stop("ignore_missing argument must be logical (TRUE/FALSE).")
  
  if(!is.numeric(stats_days))                         stop("stats_days argument must be numeric")
  if(!all(stats_days %in% c(1:180)))                  stop("stats_days argument must be integers > 0 and <= 180)")
  if(!stats_align %in% c("right", "left", "center"))  stop("stats_align argument must be 'right', 'left', or 'center'")
  
  if(!is.numeric(lowflow_days))                        stop("lowflow_days argument must be numeric")
  if(!all(lowflow_days %in% c(1:180)))                 stop("lowflow_days argument must be integers > 0 and <= 180)")
  if(!lowflow_align %in% c("right", "left", "center")) stop("lowflow_align argument must be 'right', 'left', or 'center'")
  
  if(!is.numeric(timing_percent))                 stop("timing_percent must be numeric")
  if(!all(timing_percent > 0 & timing_percent < 100))  stop("timing_percent must be >0 and <100)")
  
  if(!all(is.na(annual_percentiles))){
    if(!is.numeric(annual_percentiles))                          stop("annual_percentiles argument must be numeric.")
    if(!all(annual_percentiles > 0 & annual_percentiles < 100))  stop("annual_percentiles must be > 0 and < 100.")
  }
  if(!all(is.na(monthly_percentiles))){
    if(!is.numeric(monthly_percentiles))                          stop("annual_percentiles argument must be numeric.")
    if(!all(monthly_percentiles > 0 & monthly_percentiles < 100))  stop("annual_percentiles must be > 0 and < 100.")
  }
  
  if(!is.numeric(normal_percentiles))                 stop("normal_percentiles must be numeric.")
  if(length(normal_percentiles) != 2)                 stop("normal_percentiles must be two percentile values (ex. c(25,75)).")
  if(normal_percentiles[1] >= normal_percentiles[2] ) stop("normal_percentiles[1] must be < normal_percentiles[2].")
  if(!all(is.na(normal_percentiles)) & (!all(normal_percentiles > 0 & normal_percentiles < 100)))  
    stop("normal_percentiles must be >0 and <100)")
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  annual_stats <- calc_annual_stats(data = data,
                                    percentiles = annual_percentiles,
                                    rolling_days = stats_days,
                                    rolling_align = stats_align,
                                    water_year = water_year,
                                    water_year_start = water_year_start,
                                    start_year = start_year,
                                    end_year = end_year,
                                    exclude_years = exclude_years, 
                                    ignore_missing = ignore_missing)
  
  
  lowflow_stats <- calc_annual_lowflows(data = data,
                                        rolling_days = lowflow_days,
                                        rolling_align = lowflow_align,
                                        water_year = water_year,
                                        water_year_start = water_year_start,
                                        start_year = start_year,
                                        end_year = end_year,
                                        exclude_years = exclude_years,
                                        ignore_missing = ignore_missing)
  lowflow_stats <- dplyr::select(lowflow_stats, -dplyr::contains("Date"))
  
  
  totalQ_stats <- calc_annual_cumulative_stats(data = data,
                                               use_yield = FALSE,
                                               basin_area = NA,
                                               water_year = water_year,
                                               water_year_start = water_year_start,
                                               start_year = start_year,
                                               end_year = end_year,
                                               exclude_years = exclude_years,
                                               incl_seasons = TRUE)
  
  totalyield_stats <- calc_annual_cumulative_stats(data = data,
                                                   use_yield = TRUE,
                                                   basin_area = basin_area,
                                                   water_year = water_year,
                                                   water_year_start = water_year_start,
                                                   start_year = start_year,
                                                   end_year = end_year,
                                                   exclude_years = exclude_years,
                                                   incl_seasons = TRUE)
  
  
  timing_stats <- calc_annual_flow_timing(data = data,
                                          percent_total = timing_percent,
                                          water_year = water_year,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years)
  timing_stats <- dplyr::select(timing_stats, STATION_NUMBER, Year, dplyr::contains("DoY"))
  
  
  month_stats <- calc_monthly_stats(data = data,
                                    percentiles = monthly_percentiles,
                                    rolling_days = stats_days,
                                    rolling_align = stats_align,
                                    water_year = water_year,
                                    water_year_start = water_year_start,
                                    start_year = start_year,
                                    end_year = end_year,
                                    exclude_years = exclude_years,
                                    spread = TRUE,
                                    ignore_missing = ignore_missing)
  
  
  normals_stats <- calc_annual_outside_normal(data = data,
                                              normal_percentiles = normal_percentiles,
                                              rolling_days = stats_days,
                                              rolling_align = stats_align,
                                              water_year = water_year,
                                              water_year_start = water_year_start,
                                              start_year = start_year,
                                              end_year = end_year,
                                              exclude_years = exclude_years)
  
  ## COMBINE ALL STATS
  ## -----------------
  
  # Create the megazord
  all_stats <- merge(annual_stats, lowflow_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, totalQ_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, totalyield_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, timing_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, normals_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, month_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  
  # Gather to name all columns with CY or WY for calendar or water year
  all_stats <- tidyr::gather(all_stats, Stat, Value, 3:ncol(all_stats))
  all_stats <- dplyr::mutate(all_stats, Stat = paste0(ifelse(water_year, paste("WY_"), paste("CY_")), Stat))
  
  # Spread back using the same order
  col_order <- c("STATION_NUMBER", "Year", unique(all_stats$Stat))
  all_stats <- tidyr::spread(all_stats, Stat, Value)
  all_stats <- all_stats[, col_order]
  
  # Remove excluded years
  all_stats <- dplyr::filter(all_stats, Year >= start_year & Year <= end_year)
  all_stats[all_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    options(scipen = 999)
    
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(all_stats[-(1:2)])
    
    # Transpose the columns for rows
    all_stats <- tidyr::gather(all_stats, Statistic, Value, -STATION_NUMBER, -Year)
    all_stats <- tidyr::spread(all_stats, Year, Value)
    
    # Order the columns
    all_stats$Statistic <- factor(all_stats$Statistic, levels = stat_levels)
    all_stats <- dplyr::arrange(all_stats, STATION_NUMBER, Statistic)
  }
  
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(all_stats)[names(all_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    all_stats <- dplyr::select(all_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(all_stats)
  
  
} 

