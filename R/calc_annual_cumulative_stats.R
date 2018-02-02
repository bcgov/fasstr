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

#' @title Calculate annual (and seasonal) cumulative flows
#' 
#' @description Calculates annual and seasonal total flows, volumetric or runoff yield flows, from a streamflow dataset. Calculates 
#'    the statistics from all daily discharge values from all years, unless specified. For water year and seasonal data, the designated
#'    year is the year in which the year or season ends. Two-seasons and four-seasons per year are calculated, with each 6 and 3-month
#'    seasons starting with the first month of the year (Jan for calendar year, specified for water year). Each season is designated
#'    by the calendar or water year in which it occurs.
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
#' @param use_yield Logical value indicating whether to use yield runoff, in mm, instead of volumetric. Default \code{FALSE}.
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
#' @param incl_seasons Logical value indication whether to include seasonal yields and total discharges. Default \code{TRUE}.
#' 
#' @return A data frame with the following columns, ending with '_TotalQ_m3' or '_Yield_mm' based on selection:
#'   \item{Year}{calendar or water year selected}
#'   \item{Annual_*}{annual total flow, in m3 or mm}
#'   Default seasonal columns:
#'   \item{MMM-MMM_*}{first of two season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{second of two season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{first of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{second of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{third of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{fourth of four season total flows, in m3 or mm}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected. 
#'   
#' @examples
#' \dontrun{
#' 
#'calc_annual_cumulative_stats(data = "08NM116", 
#'                             water_year = TRUE, 
#'                             water_year_start = 8,
#'                             incl_seasons = TRUE)
#'
#' }
#' @export



calc_annual_cumulative_stats <- function(data = NULL,
                                         dates = Date,
                                         values = Value,
                                         groups = STATION_NUMBER,
                                         use_yield = FALSE, 
                                         basin_area = NA,
                                         water_year = FALSE,
                                         water_year_start = 10,
                                         start_year = 0,
                                         end_year = 9999,
                                         exclude_years = NULL, 
                                         incl_seasons = FALSE,
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
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(incl_seasons))    stop("incl_seasons argument must be logical (TRUE/FALSE).")
  
  if(!is.logical(use_yield))       stop("use_yield argument must be logical (TRUE/FALSE).")
  
  
  ## SET UP BASIN AREA
  ## -----------------
  
  suppressWarnings(flow_data <- add_basin_area(flow_data, basin_area = basin_area))
  flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_seasons(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Add cumulative flows
  if (use_yield){
    flow_data <- add_daily_yield(data = flow_data, basin_area = basin_area)
    names(flow_data)[names(flow_data) == "Yield_mm"] <- "daily_total"
  } else {
    flow_data <- add_daily_volume(data = flow_data)
    names(flow_data)[names(flow_data) == "Volume_m3"] <- "daily_total"
  }
  
  
  # Filter data FOR SELECTED YEARS FOR REMAINDER OF CALCS
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  
  
  
  ## CALCULATE STATISTICS
  ## --------------------

  
  # Calculate annual stats
  annual_stats <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                     Cumulative_total  = sum(daily_total, na.rm = FALSE))
  annual_stats <- dplyr::ungroup(annual_stats)
  names(annual_stats)[names(annual_stats) == "Cumulative_total"] <- ifelse(!use_yield,
                                                                           paste("Annual_TotalQ_m3"),
                                                                           paste("Annual_Yield_mm"))
  annual_stats <- dplyr::rename(annual_stats, Year = AnalysisYear)
  
  
  
  # Calculate seasonal stats
  
  if(incl_seasons) {
    
    # Calculate two-seasons stats
    seasons2_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, Seasons2),
                                       Cumulative_total  = sum(daily_total, na.rm = FALSE))
    seasons2_stats <- dplyr::ungroup(seasons2_stats)
    seasons2_stats <- dplyr::mutate(seasons2_stats, Seasons2 = paste0(Seasons2, "_", ifelse(!use_yield, paste("TotalQ_m3"), paste("Yield_mm"))))
    s2_order <- unique(seasons2_stats$Seasons2)
    seasons2_stats <- tidyr::spread(seasons2_stats, Seasons2, Cumulative_total)
    seasons2_stats <- dplyr::select(seasons2_stats, STATION_NUMBER, Year = AnalysisYear, s2_order)
    
    
    # Calculate four-seasons stats
    seasons4_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, Seasons4),
                                       Cumulative_total  = sum(daily_total, na.rm = FALSE))
    seasons4_stats <- dplyr::ungroup(seasons4_stats)
    seasons4_stats <- dplyr::mutate(seasons4_stats, Seasons4 = paste0(Seasons4, "_", ifelse(!use_yield, paste("TotalQ_m3"), paste("Yield_mm"))))
    s4_order <- unique(seasons4_stats$Seasons4)
    seasons4_stats <- tidyr::spread(seasons4_stats, Seasons4, Cumulative_total)
    seasons4_stats <- dplyr::select(seasons4_stats, STATION_NUMBER, Year = AnalysisYear, s4_order)
  
    
    # Merge with annual stats  
    annual_stats <- merge(annual_stats, seasons2_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
    annual_stats <- merge(annual_stats, seasons4_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
    
  }
  
  # Make an excluded years NA
  annual_stats[annual_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  # Transpose data if selected
  if(transpose){
    options(scipen = 999)
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(annual_stats[-(1:2)])
    
    annual_stats <- tidyr::gather(annual_stats, Statistic, Value, -Year, -STATION_NUMBER)
    annual_stats <- tidyr::spread(annual_stats, Year, Value)
    
    # Order the columns
    annual_stats$Statistic <- factor(annual_stats$Statistic, levels = stat_levels)
    annual_stats <- dplyr::arrange(annual_stats, STATION_NUMBER, Statistic)
    
  }
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(annual_stats)[names(annual_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    annual_stats <- dplyr::select(annual_stats, -STATION_NUMBER)
  }
  
  
  dplyr::as_tibble(annual_stats)
  
}

