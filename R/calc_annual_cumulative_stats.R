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
#' @inheritParams calc_annual_stats
#' @inheritParams add_basin_area
#' @param use_yield Logical value indicating whether to use yield runoff, in mm, instead of volumetric. Default \code{FALSE}.
#' @param incl_seasons Logical value indication whether to include seasonal yields and total discharges. Default \code{TRUE}.
#' 
#' @return A tibble data frame with the following columns, ending with '_TotalQ_m3' or '_Yield_mm' based on selection:
#'   \item{Year}{calendar or water year selected}
#'   \item{Annual_*}{annual total flow, in m3 or mm}
#'   Default seasonal columns:
#'   \item{MMM-MMM_*}{first of two season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{second of two season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{first of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{second of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{third of four season total flows, in m3 or mm}
#'   \item{MMM-MMM_*}{fourth of four season total flows, in m3 or mm}
#'   Transposing data creates a column of 'Statistics' and subsequent columns for each year selected. 
#'   
#' @examples
#' \dontrun{
#' 
#' calc_annual_cumulative_stats(station_number = "08NM116", 
#'                              water_year = TRUE, 
#'                              water_year_start = 8,
#'                              incl_seasons = TRUE)
#'
#' }
#' @export



calc_annual_cumulative_stats <- function(data = NULL,
                                         dates = Date,
                                         values = Value,
                                         groups = STATION_NUMBER,
                                         station_number = NULL,
                                         use_yield = FALSE, 
                                         basin_area = NA,
                                         water_year = FALSE,
                                         water_year_start = 10,
                                         start_year = 0,
                                         end_year = 9999,
                                         exclude_years = NULL, 
                                         incl_seasons = FALSE,
                                         transpose = FALSE){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------

  use_yield_checks(use_yield)
  water_year_checks(water_year, water_year_start)
  years_checks(start_year, end_year, exclude_years)
  transpose_checks(transpose)
  incl_seasons_checks(incl_seasons)
  
  
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
  
  # Fill missing dates, add date variables, and add AnalysisYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year = water_year, 
                             water_year_start = water_year_start,
                             year = TRUE)
  flow_data <- add_seasons(data = flow_data, water_year = water_year, water_year_start = water_year_start)

  
  # Add cumulative flows
  if (use_yield){
    flow_data <- suppressWarnings(add_daily_yield(data = flow_data, basin_area = basin_area))
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
                                     Cumulative_total = sum(daily_total, na.rm = FALSE))
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
  
  # Give warning if any NA values
  missing_complete_yr_warning(annual_stats[, 3:ncol(annual_stats)])
  
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(annual_stats)[names(annual_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    annual_stats <- dplyr::select(annual_stats, -STATION_NUMBER)
  }
  
  
  dplyr::as_tibble(annual_stats)
  
}

