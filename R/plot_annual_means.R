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


#' @title Plot annual means compared to the long-term mean
#'
#' @description Plot annual means using the long-term annual mean as the point of reference to compare wet vs. dry years. Plots the
#'    statistics from all daily discharge values from all years, unless specified. Data calculated using calc_annual_stats() function.
#'
#' @inheritParams calc_annual_stats
#' @param include_title Logical value to indicate adding the group/station number to the plot, if provided. Default \code{FALSE}.
#'
#' @return A list of ggplot2 objects for with the following plots for each station provided:
#'   \item{Annual_Means}{a plot that contains annual means with the long-term mean as the x-axis intercept}
#'   
#' @seealso \code{\link{calc_annual_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' plot_annual_means(station_number = "08NM116", 
#'                   water_year = TRUE, 
#'                   water_year_start = 8)
#'
#' plot_annual_means(station_number = "08NM116", 
#'                   months = 7:9)
#'
#' }
#' @export


plot_annual_means <- function(data = NULL,
                              dates = Date,
                              values = Value,
                              groups = STATION_NUMBER,
                              station_number = NULL,
                              roll_days = 1,
                              roll_align = "right",
                              water_year = FALSE,
                              water_year_start = 10,
                              start_year = 0,
                              end_year = 9999,
                              exclude_years = NULL,
                              months = 1:12,
                              ignore_missing = FALSE,
                              include_title = FALSE){ 
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  include_title_checks(include_title)

    
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = TRUE)
  
  
  ## CALC STATS
  ## ----------
  
  annual_stats <- calc_annual_stats(data = flow_data,
                                    roll_days = roll_days,
                                    roll_align = roll_align,
                                    water_year = water_year,
                                    water_year_start = water_year_start,
                                    start_year = start_year,
                                    end_year = end_year,
                                    exclude_years = exclude_years, 
                                    months = months,
                                    ignore_missing = ignore_missing)
  
  annual_stats <- dplyr::select(annual_stats, STATION_NUMBER, Year, Mean)
  
  lt_mad <- dplyr::group_by(annual_stats, STATION_NUMBER)
  lt_mad <- dplyr::summarise(lt_mad, LTMAD = mean(Mean, na.rm = TRUE))
  
  annual_stats <- dplyr::left_join(annual_stats, lt_mad, by = "STATION_NUMBER")
  annual_stats <- dplyr::mutate(annual_stats, 
                                MAD_diff = Mean - LTMAD)
  annual_stats <- annual_stats[complete.cases(annual_stats$Mean), ]
  
  
  ## PLOT STATS
  ## ----------
  
  # Create plots for each STATION_NUMBER in a tibble (see: http://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/)
  tidy_plots <- dplyr::group_by(annual_stats, STATION_NUMBER)
  tidy_plots <- tidyr::nest(tidy_plots)
  tidy_plots <- dplyr::mutate(tidy_plots,
    plot = purrr::map2(data, STATION_NUMBER,
     ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = MAD_diff)) +
       ggplot2::geom_bar(stat = "identity", fill = "cornflowerblue") +
       ggplot2::geom_hline(yintercept = 0, size = 0.1) +
       ggplot2::scale_y_continuous(labels = function(x) round(x + unique(.$LTMAD),3),
                                   breaks = scales::pretty_breaks(n = 10)) +
       ggplot2::scale_x_continuous(breaks = function(x, n = 10) pretty(x, n)[pretty(x, n) %% 1 == 0] ) +
       ggplot2::ylab("Annual Discharge (cms)") +
       ggplot2::theme_bw() +
       ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                      panel.grid = ggplot2::element_line(size = .2),
                      axis.title = ggplot2::element_text(size = 12),
                      axis.text = ggplot2::element_text(size = 10))
    ))

  # Create a list of named plots extracted from the tibble
  plots <- tidy_plots$plot
  if (nrow(tidy_plots) == 1) {
    names(plots) <- "Annual_Means"
  } else {
    names(plots) <- paste0(tidy_plots$STATION_NUMBER, "_Annual_Means")
  }

  plots

}
