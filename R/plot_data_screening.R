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


#' @title Plot annual summary statistics for data screening
#'
#' @description Plots the mean, median, maximum, minimum, standard deviation of annual flows. Plots the statistics from all daily 
#'    discharge values from all years, unless specified. Data calculated using screen_flow_data() function.
#'
#' @inheritParams screen_flow_data
#'
#' @return A ggplot2 object with the following plots:
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{StandardDeviation}{annual 1 standard deviation of all daily flows for a given year}
#'
#' @examples
#' \dontrun{
#' 
#' plot_data_screening(station_number = "08NM116", 
#'                     water_year = TRUE, 
#'                     water_year_start = 8)
#'
#' }
#' @export



plot_data_screening <- function(data = NULL,
                                dates = Date,
                                values = Value,
                                station_number = NULL,
                                roll_days = 1,
                                roll_align = "right",
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = 0,
                                end_year = 9999){ 
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  one_station_number_stop(station_number)
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_plot_cols(data = flow_data, 
                                dates = as.character(substitute(dates)),
                                values = as.character(substitute(values)))
  
  
  ## CALC STATS
  ## ----------
  
  flow_summary <- fasstr::screen_flow_data(data = flow_data,
                                           roll_days = roll_days,
                                           roll_align = roll_align,
                                           water_year = water_year,
                                           water_year_start = water_year_start,
                                           start_year = start_year,
                                           end_year = end_year)
  
  flow_summary <- dplyr::select(flow_summary, Year, Minimum, Maximum, Mean, StandardDeviation)
  flow_summary <- tidyr::gather(flow_summary, Statistic, Value, 2:5)
  
  
  ## PLOT STATS
  ## ----------
  
  suppressWarnings(
    ggplot2::ggplot(data = flow_summary, ggplot2::aes(x = Year, y = Value)) +
      ggplot2::geom_line(colour = "dodgerblue4") +
      ggplot2::geom_point(colour = "firebrick3") +
      ggplot2::facet_wrap(~Statistic, ncol = 2, scales = "free_y") +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::ylab("Discharge (cms)") +
      ggplot2::xlab("Year") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
  )
  
  
  
}

