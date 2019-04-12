# Copyright 2019 Province of British Columbia
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

#' @title Calculate cumulative daily flow statistics
#'
#' @description Calculate cumulative daily flow statistics for each day of the year of daily flow values from a streamflow dataset. 
#'    Calculates the statistics from all daily discharge values from all years, unless specified. Defaults to volumetric cumulative 
#'    flows, can use \code{use_yield} and \code{basin_area} to convert to runoff yield.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams calc_annual_cumulative_stats
#'    
#' @return A data frame with the following columns, default units in cubic metres, millimetres if use_yield and basin_area provided:
#'   \item{Date}{date (MMM-DD) of daily cumulative statistics}
#'   \item{DayofYear}{day of year of daily cumulative statistics}
#'   \item{Mean}{daily mean of all cumulative flows for a given day of the year}
#'   \item{Median}{daily mean of all cumulative flows for a given day of the year}
#'   \item{Maximum}{daily mean of all cumulative flows for a given day of the year}
#'   \item{Minimum}{daily mean of all cumulative flows for a given day of the year}
#'   \item{P'n'}{each daily n-th percentile selected of all cumulative flows for a given day of the year}
#'   Default percentile columns:
#'   \item{P5}{daily 5th percentile of all cumulative flows for a given day of the year}
#'   \item{P25}{daily 25th percentile of all cumulative flows for a given day of the year}
#'   \item{P75}{daily 75th percentile of all cumulative flows for a given day of the year}
#'   \item{P95}{daily 95th percentile of all cumulative flows for a given day of the year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#' # Calculate volume statistics
#' calc_daily_cumulative_stats(station_number = "08NM116") 
#' 
#' # Calculate yield statistics with default HYDAT basin area
#' calc_daily_cumulative_stats(station_number = "08NM116",
#'                             use_yield = TRUE) 
#' 
#' # Calculate yield statistics with custom basin area
#' calc_daily_cumulative_stats(station_number = "08NM116",
#'                             use_yield = TRUE,
#'                             basin_area = 800) 
#' }
#' @export



calc_daily_cumulative_stats <- function(data,
                                        dates = Date,
                                        values = Value,
                                        groups = STATION_NUMBER,
                                        station_number,
                                        percentiles = c(5,25,75,95),
                                        use_yield = FALSE, 
                                        basin_area,
                                        water_year_start = 1,
                                        start_year,
                                        end_year,
                                        exclude_years, 
                                        transpose = FALSE){
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }
  if (missing(exclude_years)) {
    exclude_years = NULL
  }
  if (missing(basin_area)) {
    basin_area = NA
  }
  
  percentiles_checks(percentiles)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  transpose_checks(transpose)
  use_yield_checks(use_yield)
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = TRUE)
  
  ## SET UP BASIN AREA
  ## -----------------
  
  if (use_yield) {
    flow_data <- add_basin_area(flow_data, basin_area = basin_area)
    flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  }  
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add WaterYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start,
                             date = TRUE)
  
  # Add cumulative flows
  if (use_yield){
    flow_data <- suppressWarnings(add_cumulative_yield(data = flow_data,
                                                       water_year_start = water_year_start, 
                                                       basin_area = basin_area))
    flow_data$Cumul_Flow <- flow_data$Cumul_Yield_mm
  } else {
    flow_data <- add_cumulative_volume(data = flow_data, water_year_start = water_year_start)
    flow_data$Cumul_Flow <- flow_data$Cumul_Volume_m3
  }
  
  # Filter for the selected and excluded years and leap year values (last day)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(WaterYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, DayofYear < 366)
  
  
  #if (all(is.na(flow_data$Cumul_Flow))) 
  #  stop("No basin_area values provided or extracted from HYDAT. Use basin_area argument to supply one.", call. = FALSE)
  
  # Warning if some of the years contained partial data
  comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                 complete_yr = ifelse(sum(!is.na(Cumul_Flow)) == length(WaterYear), TRUE, FALSE))
  if (!all(comp_years$complete_yr)) 
    warning("One or more years contained partial data and were excluded. Only years with complete data were used for calculations.", call. = FALSE)

  flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "WaterYear"))
  flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
  flow_data <- dplyr::select(flow_data, -complete_yr)

  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate basic stats
  daily_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisDate, DayofYear),
                                  Mean = mean(Cumul_Flow, na.rm = FALSE),
                                  Median = stats::median(Cumul_Flow, na.rm = FALSE),
                                  Minimum = min(Cumul_Flow, na.rm = FALSE),
                                  Maximum = max(Cumul_Flow, na.rm = FALSE))

  # Compute daily percentiles (if 10 or more years of data)
  if (!all(is.na(percentiles))){
    for (ptile in percentiles) {
      daily_stats_ptile <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisDate, DayofYear),
                                            Percentile = stats::quantile(Cumul_Flow, ptile / 100, na.rm = TRUE))
      names(daily_stats_ptile)[names(daily_stats_ptile) == "Percentile"] <- paste0("P", ptile)

      # Merge with daily_stats
      daily_stats <- merge(daily_stats, daily_stats_ptile, by = c("STATION_NUMBER", "AnalysisDate", "DayofYear"))

      # Remove percentile if mean is NA (workaround for na.rm=FALSE in quantile)
      daily_stats[, ncol(daily_stats)] <- ifelse(is.na(daily_stats$Mean), NA, daily_stats[, ncol(daily_stats)])
    }
  }

  # Final formatting
  daily_stats <- dplyr::rename(daily_stats, DayofYear = DayofYear, Date = AnalysisDate)
  daily_stats$Date <- format(as.Date(daily_stats$Date), format = "%b-%d")
  col_order <- daily_stats$Date


  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(daily_stats[-(1:2)])

    # Transpose the columns for rows
    daily_stats <- tidyr::gather(daily_stats, Statistic, Value, -STATION_NUMBER, -Date)
    daily_stats <- tidyr::spread(daily_stats, Date, Value)

    # Order the columns
    daily_stats <-  daily_stats[, c("STATION_NUMBER", "Statistic", col_order)]
    daily_stats$Statistic <- factor(daily_stats$Statistic, levels = stat_levels)
    daily_stats <- dplyr::arrange(daily_stats, STATION_NUMBER, Statistic)
  }


  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(daily_stats)[names(daily_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    daily_stats <- dplyr::select(daily_stats, -STATION_NUMBER)
  }


  logical_cols <- sapply(daily_stats, is.logical)
  daily_stats[logical_cols] <- lapply(daily_stats[logical_cols], as.numeric)


  dplyr::as_tibble(daily_stats)
  
}
