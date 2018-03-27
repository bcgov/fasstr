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
#' @inheritParams calc_annual_stats
#' @param normal_percentiles Numeric vector of two values, lower and upper percentiles, respectively indicating the limits of the 
#'    normal range. Default \code{c(25,75)}.
#' 
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Days_Below_Normal}{number of days per year below the daily normal (default 25th percentile)}
#'   \item{Days_Above_Normal}{number of days per year above the daily normal (default 75th percentile)}
#'   \item{Days_Outside_Normal}{number of days per year below and above the daily normal (default 25/75th percentile)}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#' calc_annual_outside_normal(station_number = "08NM116", 
#'                            water_year = TRUE, 
#'                            water_year_start = 8)
#'
#' }
#' @export



calc_annual_outside_normal <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       groups = STATION_NUMBER,
                                       station_number = NULL,
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
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  rolling_days_checks(roll_days, roll_align, multiple = FALSE)
  water_year_checks(water_year, water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  transpose_checks(transpose)
  normal_percentiles_checks(normal_percentiles)
  sort(normal_percentiles)
    
  
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

  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add AnalysisYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year = water_year, 
                             water_year_start = water_year_start,
                             year = TRUE,
                             doy = TRUE)
  
  # Add rolling means to end of dataframe
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
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
  
  # Give warning if any NA values
  missing_complete_yr_warning(normals_stats[, 3:ncol(normals_stats)])


  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(normals_stats)[names(normals_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    normals_stats <- dplyr::select(normals_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(normals_stats)
  
  
}

