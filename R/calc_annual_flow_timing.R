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

#' @title Calculate annual flow timing
#'
#' @description Calculates annual the timing (day of year) and date of occurrence of portions of total annual flow of daily flow 
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years with complete annual
#'    data, unless specified.
#'
#' @inheritParams calc_annual_stats
#' @param percent_total Numeric vector of percents of total annual flows to determine dates. Default \code{c(25,33.3,50,75)}.
#' 
#' @references 
#' \itemize{
#'  \item{Barnett, T.P., Pierce, D.W., Hidalgo, H.G., Bonfils, C., Santer, B.D., Das, T., Bala, G., Wood, A.W.,
#'        Nozawa, T., Mirin, A.A., Cayan, D.R., Dettinger, M.D., 2008. Human-Induced Clanges in the Hydrology of 
#'        the Western United States. Science 319, 1080-1083.}
#'        }
#' 
#' 
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{DoY_'n'pct_TotalQ}{day of year for each n-percent of total volumetric discharge}
#'   \item{Date_'n'pct_TotalQ}{date (YYYY-MM-DD) for each n-percent of total volumetric discharge}
#'   Default columns:
#'   \item{DoY_25pct_TotalQ}{day of year of 25-percent of total volumetric discharge}
#'   \item{Date_25pct_TotalQ}{date (YYYY-MM-DD) of 25-percent of total volumetric discharge}
#'   \item{DoY_33.3pct_TotalQ}{day of year of 33.3-percent of total volumetric discharge}
#'   \item{Date_33.3pct_TotalQ}{date (YYYY-MM-DD) of 33.3-percent of total volumetric discharge}
#'   \item{DoY_50pct_TotalQ}{day of year of 50-percent of total volumetric discharge}
#'   \item{Date_50pct_TotalQ}{date (YYYY-MM-DD) of 50-percent of total volumetric discharge}
#'   \item{DoY_75pct_TotalQ}{day of year of 75-percent of total volumetric discharge}
#'   \item{Date_75pct_TotalQ}{date (YYYY-MM-DD) of 75-percent of total volumetric discharge}
#'   Transposing data creates a column of 'Statistics' (just DoY, not Date values) and subsequent columns for each year selected.
#' 
#' @examples
#' \dontrun{
#' 
#' # Calculate statistics with default percent totals
#' calc_annual_flow_timing(station_number = "08NM116") 
#' 
#' # Calculate statistics with custom percent totals
#' calc_annual_cumulative_stats(station_number = "08NM116",
#'                              percent_total = 50)
#' }
#' @export


calc_annual_flow_timing <- function(data,
                                    dates = Date,
                                    values = Value,
                                    groups = STATION_NUMBER,
                                    station_number,
                                    percent_total = c(25, 33.3, 50, 75),
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
  
  percent_total_checks(percent_total)
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
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  # Fill missing dates, add date variables, and add WaterYear and DOY
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  flow_data <- add_cumulative_volume(flow_data, water_year_start = water_year_start)
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Loop through percents
  timing_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear))
  
  for (percent in percent_total) {
    timing_pcnt <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                    TOTALQ_DAY  =  DayofYear[ match(TRUE, Cumul_Volume_m3 > percent / 100 * 
                                                                        ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))],
                                    TOTALQ_DATE =  Date[ match(TRUE, Cumul_Volume_m3 > percent / 100 *
                                                                 ((mean(Value, na.rm = TRUE)) * length(Value) * 60 * 60 * 24))])
    names(timing_pcnt)[names(timing_pcnt) == "TOTALQ_DAY"] <- paste0("DoY_", percent, "pct_TotalQ")
    names(timing_pcnt)[names(timing_pcnt) == "TOTALQ_DATE"] <- paste0("Date_", percent, "pct_TotalQ")
    timing_stats <- merge(timing_stats, timing_pcnt, by = c("STATION_NUMBER", "WaterYear"), all = TRUE)
    
  }
  timing_stats <-   dplyr::rename(timing_stats, Year = WaterYear)
  
  
  # Make excluded years data NA
  if(as.character(substitute(groups)) %in% orig_cols) {
    timing_stats[timing_stats$Year %in% exclude_years, -(1:2)] <- NA
  } else {
    timing_stats[timing_stats$Year %in% exclude_years, -1] <- NA
  }
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    timing_stats <- dplyr::select(timing_stats, STATION_NUMBER, Year, dplyr::contains("DoY"))
    col_names <- names(timing_stats[-(1:2)])
    timing_stats <- tidyr::gather(timing_stats, Statistic, Value, -Year, -STATION_NUMBER)
    timing_stats <- tidyr::spread(timing_stats, Year, Value)
  }
  
  # Give warning if any NA values
  if (!transpose) {
    missing_test <- dplyr::filter(timing_stats, !(Year %in% exclude_years))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  } else {
    missing_test <- dplyr::select(timing_stats, -dplyr::one_of(as.character(exclude_years)))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  }
  
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(timing_stats)[names(timing_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    timing_stats <- dplyr::select(timing_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(timing_stats)
  
}

