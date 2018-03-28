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


#' @title Plot annual summary statistics
#'
#' @description Plot annual monthly mean, median, maximum, minimum, and percentiles of daily flow values from a streamflow 
#'    dataset. Plots the statistics from all daily discharge values from all years, unless specified. Data 
#'    calculated using calc_annual_stats() function.
#'
#' @inheritParams calc_annual_stats
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{FALSE}.
#'
#' @return A ggplot2 object with the following plots (percentile plots optional):
#'   \item{Mean}{annual mean of all daily flows}
#'   \item{Median}{annual median of all daily flows}
#'   \item{Maximum}{annual maximum of all daily flows}
#'   \item{Minimum}{annual minimum of all daily flows}
#' 
#' @examples
#' \dontrun{
#' 
#' plot_annual_stats(data = flow_data)
#' 
#' plot_annual_stats(station_number = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' plot_annual_stats(station_number = "08NM116", months = 7:9)
#'
#' }
#' @export


plot_annual_stats <- function(data = NULL,
                              dates = Date,
                              values = Value,
                              station_number = NULL,
                              percentiles = NA,
                              roll_days = 1,
                              roll_align = "right",
                              water_year = FALSE,
                              water_year_start = 10,
                              start_year = 0,
                              end_year = 9999,
                              exclude_years = NULL,
                              months = 1:12,
                              ignore_missing = FALSE,
                              log_discharge = FALSE){ 
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  log_discharge_checks(log_discharge) 
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
  
  annual_stats <- calc_annual_stats(data = flow_data,
                                    percentiles = percentiles,
                                    roll_days = roll_days,
                                    roll_align = roll_align,
                                    water_year = water_year,
                                    water_year_start = water_year_start,
                                    start_year = start_year,
                                    end_year = end_year,
                                    exclude_years = exclude_years, 
                                    months = months,
                                    ignore_missing = ignore_missing)
  
  
  annual_stats <- tidyr::gather(annual_stats, Statistic, Value, -1)
  
  
  ## PLOT STATS
  ## ----------
  
  suppressWarnings(print(
    ggplot2::ggplot(data = annual_stats, ggplot2::aes(x = Year, y = Value, color = Statistic)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point() +
      {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(annual_stats$Value, na.rm = T) * 1.05))}+
      {if(log_discharge) ggplot2::expand_limits(y = c(min(annual_stats$Value, na.rm = T) * .95, max(annual_stats$Value, na.rm = T) * 1.05))} +
      {if(log_discharge) ggplot2::scale_y_log10(expand = c(0,0))} +
      {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0))} +
      {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"), 
                                                      mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
      ggplot2::expand_limits(y = 0) +
      ggplot2::ylab("Discharge (cms)")+
      ggplot2::xlab("Year") +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::labs(color='Annual Statistics') +    
      ggplot2::theme(legend.position = "right", 
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.justification = "top",
                     legend.text = ggplot2::element_text(size = 9),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
  ))    
    
}

