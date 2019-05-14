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

#' @title Add a column of seasons
#' 
#' @description Adds a column of seasons identifiers to a data frame with a column of dates called 'Date'. The length of seasons,
#'    in months, is provided using the seasons_length argument. As seasons are grouped by months the length of the seasons must be
#'    divisible into 12 with one of the following season lengths: 1, 2, 3, 4, 6, or 12 months. The start of the first season 
#'    coincides with the start month of each year; 'Jan-Jun' for 6-month seasons starting with calendar years or 'Dec-Feb' for 3-month
#'    seasons starting with water year starting in December.
#'
#' @inheritParams calc_annual_stats
#' @param seasons_length Numeric value indicating the desired length of seasons in months, divisible into 12. Required.
#' 
#' @return A tibble data frame of the source data with additional column:
#'   \item{Season}{season identifier labelled by the start and end month of the season}
#'
#' @examples
#' \dontrun{
#' 
#' # Four seasons starting in January
#' add_seasons(data = "08NM116",
#'             seasons_length = 4)
#' 
#' # Two seasons starting in October
#' add_seasons(data = "08NM116", 
#'             water_year_start = 10,
#'             seasons_length = 6)
#' }
#' @export


add_seasons <- function(data,
                        dates = Date,
                        station_number,
                        water_year_start = 1,
                        seasons_length){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  
  water_year_checks(water_year_start)
  if (missing(seasons_length))       stop("seasons_length argument (number of months per season) is required.", call. = FALSE)
  if (!is.numeric(seasons_length))   stop("seasons_length argument must be a number divisible into 12.", call. = FALSE)
  if (length(seasons_length)>1)      stop("seasons_length argument must be a number divisible into 12.", call. = FALSE)
  if (!12%%seasons_length==0)        stop("seasons_length argument must be a number divisible into 12.", call. = FALSE)
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  if ("Season" %in% orig_cols) stop("Seasons column already exists, please rename or remove to add a new seasons column.",
                                    call. = FALSE)
  
  # Get groups of flow_data to return after
  flow_data_groups <- dplyr::group_vars(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <-   format_dates_col(data = flow_data, dates = as.character(substitute(dates)))
  
  
  
  ## ADD SEASONS VARIABLES
  ## ----------------------
  
  # Add dates
  flow_data <- add_date_variables(data = flow_data, water_year_start = water_year_start)
  
  # Add water months (utils.R function)
  flow_data <- add_water_months(flow_data, water_year_start)
  
  # Create the order of months list
  month_list <- dplyr::group_by(flow_data, MonthName)
  month_list <- dplyr::summarise(month_list, Value = min(AnalysisMonth))
  month_list <- dplyr::pull(month_list, 1)
  
  # Loop through each season to add a temporary label
  for (i in sort(1:(12/seasons_length), decreasing = TRUE)){
    flow_data <- dplyr::mutate(flow_data,
                               Seasons_temp = ifelse(AnalysisMonth < (seasons_length*i + 1),
                                                     paste0("S", i),
                                                     Seasons_temp))
  }
  
  # Create temp data frame to gather the MonthNames for labelling the seasons (if > 1 season per year)
  if (seasons_length == 1) {
    flow_data <- dplyr::mutate(flow_data, Season = MonthName)
    
  } else {
    season_name <- dplyr::group_by(flow_data, Seasons_temp)
    season_name <- dplyr::summarise(season_name,
                                    Season = paste(month_list[min(AnalysisMonth)],
                                                   month_list[max(AnalysisMonth)], 
                                                   sep = "-"))
    
    flow_data <- dplyr::left_join(flow_data, season_name, by = "Seasons_temp", all = TRUE)
  }
  
  flow_data <- dplyr::select(flow_data, -Seasons_temp, -AnalysisMonth)
  flow_data$Season <- factor(flow_data$Season, levels = dplyr::pull(season_name, 2))
  
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the Date column
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  
  # Return columns to original order plus new column
  if(all(c("Season") %in% orig_cols)){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, "Season")]
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(flow_data_groups))
  
  
  dplyr::as_tibble(flow_data)
  
  
}
