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

#' @title Plot annual days above and below normal
#'
#' @description Plots the number of days per year outside of the 'normal' range (typically between 25 and 75th percentiles) for
#'    each day of the year. Upper and lower-range percentiles are calcuated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. All days above or below the normal range are included. Calculates the statistics 
#'    from all daily discharge values from all years, unless specified. Data calculated using calc_annual_outside_normal() function.
#'
#' @inheritParams calc_annual_outside_normal
#' 
#' @return A ggplot2 object with the following plots:
#'   \item{Days_Below_Normal}{number of days per year below the daily normal (default 25th percentile)}
#'   \item{Days_Above_Normal}{number of days per year above the daily normal (default 75th percentile)}
#'   \item{Days_Outside_Normal}{number of days per year below and above the daily normal (default 25/75th percentile)}
#'
#' @examples
#' \dontrun{
#' 
#' plot_annual_outside_normal(station_number = "08NM116",
#'                            water_year = TRUE, 
#'                            water_year_start = 8)
#'
#' }
#' @export



plot_annual_outside_normal <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       station_number = NULL,
                                       normal_percentiles = c(25, 75),
                                       roll_days = 1,
                                       roll_align = "right",
                                       water_year = FALSE,
                                       water_year_start = 10,
                                       start_year = 0,
                                       end_year = 9999,
                                       exclude_years = NULL, 
                                       months = 1:12){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  #None  
  
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
  
  normal_data <- fasstr::calc_annual_outside_normal(data = flow_data,
                                                    normal_percentiles = normal_percentiles,
                                                    roll_days = roll_days,
                                                    roll_align = roll_align,
                                                    water_year = water_year,
                                                    water_year_start = water_year_start,
                                                    start_year = start_year,
                                                    end_year = end_year,
                                                    exclude_years = exclude_years, 
                                                    months = months)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(normal_data)) {
    normal_data <- dplyr::ungroup(normal_data)
    normal_data <- dplyr::select(normal_data, -STATION_NUMBER)
  }
  
  normal_data <- tidyr::gather(normal_data, Statistic, Value, -1)
  normal_data <- dplyr::mutate(normal_data, Statistic = substr(Statistic, 6, nchar(Statistic)))
  normal_data <- dplyr::mutate(normal_data, Statistic = gsub("_", " ", Statistic))
  
  
  
  ## PLOT STATS
  ## ----------
  
  suppressWarnings(print(
    ggplot2::ggplot(data = normal_data, ggplot2::aes(x = Year, y = Value, colour = Statistic))+
      ggplot2::geom_line(alpha = 0.5)+
      ggplot2::geom_point()+
      ggplot2::facet_wrap(~Statistic, scales="free_x", ncol = 1, strip.position = "right")+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      ggplot2::ylab("Number of Days")+
      ggplot2::xlab("Year")+
      ggplot2::guides(colour = FALSE)+
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
  ))
  
}

