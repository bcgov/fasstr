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

#' @title Calculate cumulative monthly flow statistics
#'
#' @description Calculate cumulative monthly flow statistics for each month of the year of daily flow values from a streamflow dataset. 
#'    Calculates the statistics from all daily discharge values for each month from all years, unless specified. Defaults to volumetric 
#'    cumulative flows, can use \code{use_yield} and \code{basin_area} to convert to runoff yield.
#'
#' @inheritParams calc_annual_cumulative_stats
#' @inheritParams calc_daily_stats
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{c(5,25,75,95)}.
#'    
#' @return A tibble data frame with the following columns, default units in cubic metres, or millimetres if use_yield and basin_area provided:
#'   \item{Month}{month (MMM-DD) of cumulative statistics}
#'   \item{Mean}{monthly mean of all cumulative flows for a given month of the year}
#'   \item{Median}{monthly mean of all cumulative flows for a given month of the year}
#'   \item{Maximum}{monthly mean of all cumulative flows for a given month of the year}
#'   \item{Minimum}{monthly mean of all cumulative flows for a given month of the year}
#'   \item{P'n'}{each monthly n-th percentile selected of all cumulative flows for a given month of the year}
#'   Default percentile columns:
#'   \item{P5}{monthly 5th percentile of all cumulative flows for a given month of the year}
#'   \item{P25}{monthly 25th percentile of all cumulative flows for a given month of the year}
#'   \item{P75}{monthly 75th percentile of all cumulative flows for a given month of the year}
#'   \item{P95}{monthly 95th percentile of all cumulative flows for a given month of the year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#' # Calculate volume statistics
#' calc_monthly_cumulative_stats(station_number = "08NM116") 
#' 
#' # Calculate yield statistics with default HYDAT basin area
#' calc_monthly_cumulative_stats(station_number = "08NM116",
#'                               use_yield = TRUE) 
#' 
#' # Calculate yield statistics with custom basin area
#' calc_monthly_cumulative_stats(station_number = "08NM116",
#'                               use_yield = TRUE,
#'                               basin_area = 800) 
#' }
#' @export



calc_monthly_cumulative_stats <- function(data,
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
  
  if (use_yield){
    flow_data <- add_basin_area(flow_data, basin_area = basin_area)
    flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  }  
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add WaterYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  
  # Add cumulative flows
  if (use_yield){
    flow_data <- suppressWarnings(add_cumulative_yield(data = flow_data, 
                                                       water_year_start = water_year_start, 
                                                       basin_area = basin_area))
    names(flow_data)[names(flow_data) == "Cumul_Yield_mm"] <- paste("Cumul_Total")
  } else {
    flow_data <- add_cumulative_volume(data = flow_data, water_year_start = water_year_start)
    names(flow_data)[names(flow_data) == "Cumul_Volume_m3"] <- paste("Cumul_Total")
  }
  
  # Filter for the selected and excluded years and leap year values (last day)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(WaterYear %in% exclude_years))
  
  # if (all(is.na(flow_data$Cumul_Total))) 
  #   stop("No basin_area values provided or extracted from HYDAT. Use basin_area argument to supply one.", call. = FALSE)

  # Warning if some of the years contained partial data
  comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                 complete_yr = ifelse(sum(!is.na(Value)) == length(WaterYear), TRUE, FALSE))
  if (!all(comp_years$complete_yr)) 
    warning("One or more years contained partial data and were excluded. Only years with complete data were used for calculations.", call. = FALSE)
  
  flow_data <- merge(flow_data, comp_years, by = c("STATION_NUMBER", "WaterYear"))
  if (all(!flow_data$complete_yr)) {
  } else {
    flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
  }
  flow_data <- dplyr::select(flow_data, -complete_yr)
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate monthly totals for all years
  monthly_data <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear, MonthName),
                                   Monthly_Total = max(Cumul_Total, na.rm = FALSE))
  
  # Calculate the monthly and longterm stats
  monthly_cumul <- dplyr::summarize(dplyr::group_by(monthly_data, STATION_NUMBER, MonthName),
                                    Mean = mean(Monthly_Total, na.rm = FALSE),
                                    Median = stats::median(Monthly_Total, na.rm = FALSE),
                                    Maximum = max(Monthly_Total, na.rm = FALSE),
                                    Minimum = min(Monthly_Total, na.rm = FALSE))

  # Compute daily percentiles
  if (!all(is.na(percentiles))){
    for (ptile in percentiles) {
      monthly_ptile <- dplyr::summarise(dplyr::group_by(monthly_data, STATION_NUMBER, MonthName),
                                        Percentile = ifelse(!is.na(mean(Monthly_Total, na.rm = TRUE)),
                                                            stats::quantile(Monthly_Total, ptile / 100, na.rm = TRUE), NA))

      names(monthly_ptile)[names(monthly_ptile) == "Percentile"] <- paste0("P", ptile)

      # Merge with monthly_cumul
      monthly_cumul <- merge(monthly_cumul, monthly_ptile, by = c("STATION_NUMBER", "MonthName"))
    }
  }

  # Rename Month column and reorder to proper levels (set in add_date_vars)
  monthly_cumul <- dplyr::rename(monthly_cumul, Month = MonthName)
  monthly_cumul <- with(monthly_cumul, monthly_cumul[order(STATION_NUMBER, Month),])
  row.names(monthly_cumul) <- c(1:nrow(monthly_cumul))


  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(monthly_cumul[-(1:2)])

    # Transpose the columns for rows
    monthly_cumul <- tidyr::gather(monthly_cumul, Statistic, Value, -STATION_NUMBER, -Month)
    monthly_cumul <- tidyr::spread(monthly_cumul, Month, Value)

    # Order the columns
    monthly_cumul$Statistic <- factor(monthly_cumul$Statistic, levels = stat_levels)
    monthly_cumul <- dplyr::arrange(monthly_cumul, STATION_NUMBER, Statistic)
  }

  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(monthly_cumul)[names(monthly_cumul) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    monthly_cumul <- dplyr::select(monthly_cumul, -STATION_NUMBER)
  }


  dplyr::as_tibble(monthly_cumul)


}
