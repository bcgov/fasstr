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

#' @title Plot the long-term and long-term monthly summary statistics
#'
#' @description Plots the long-term and long-term monthly mean, median, maximum, minimum, and 5, 25, 75, and 95 percentiles of daily 
#'    flow values from a single streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. 
#'    Data calculated using calc_longterm_stats() function.
#'
#' @inheritParams calc_longterm_stats
#' @inheritParams plot_annual_stats
#'
#' @return A ggplot2 object with the following plots:
#'   \item{Long-term Mean}{mean of all daily flows over all years}
#'   \item{Long-term Median}{median of all daily flows over all years}
#'   \item{Monthly Mean}{mean of all daily flows for each month over all years}
#'   \item{Monthly Median}{median of all daily flows for each month over all years}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the monthly 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the monthly minimum and maximums}
#'   
#' @examples
#' \dontrun{
#' 
#' plot_longterm_stats(station_number = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


plot_longterm_stats <- function(data = NULL,
                                dates = Date,
                                values = Value,
                                station_number = NULL,
                                roll_days = 1,
                                roll_align = "right",
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = 0,
                                end_year = 9999,
                                exclude_years = NULL,
                                complete_years = FALSE,
                                ignore_missing = FALSE,
                                log_discharge = TRUE){
  
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
  
  longterm_stats <- fasstr::calc_longterm_stats(data = flow_data,
                                                percentiles = c(5,25,75,95),
                                                roll_days = roll_days,
                                                roll_align = roll_align,
                                                water_year = water_year,
                                                water_year_start = water_year_start,
                                                start_year = start_year,
                                                end_year = end_year,
                                                exclude_years = exclude_years,
                                                complete_years = complete_years,
                                                ignore_missing = ignore_missing)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(longterm_stats)) {
    longterm_stats <- dplyr::ungroup(longterm_stats)
    longterm_stats <- dplyr::select(longterm_stats, -STATION_NUMBER)
  }
  
  ## PLOT STATS
  ## ----------
  
  longterm_stats_months <- dplyr::filter(longterm_stats, Month != "Long-term")
  longterm_stats_longterm <- dplyr::filter(longterm_stats, Month == "Long-term")
  
  suppressMessages(
  suppressWarnings(
    ggplot2::ggplot(longterm_stats_months, ggplot2::aes(group = 1)) +
      ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = Minimum, ymax = Maximum, fill = "Minimum-Maxium")) +
      ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = P5, ymax = P95, fill = "5-95 Percentiles")) +
      ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = P25, ymax = P75, fill = "25-75 Percentiles")) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = longterm_stats_longterm$Mean, colour = "Long-term Mean"), size = 0.6, linetype = 2) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = longterm_stats_longterm$Median, colour = "Long-term Median"), size = 0.6, linetype = 2) +
      ggplot2::geom_line(ggplot2::aes(x = Month, y = Mean, color = "Monthly Mean"), size = 0.6) +
      ggplot2::geom_line(ggplot2::aes(x = Month, y = Median, color = "Monthly Median"), size = 0.6) +
      ggplot2::geom_point(ggplot2::aes(x = Month, y = Mean, color = "Monthly Mean"), size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = Month, y = Median, color = "Monthly Median"), size = 2) +
      ggplot2::scale_color_manual(values = c("Monthly Mean" = "skyblue2", "Monthly Median" = "dodgerblue4",
                                             "Long-term Mean" = "forestgreen", "Long-term Median" = "darkorchid4")) +
      ggplot2::scale_fill_manual(values = c("25-75 Percentiles" = "lightblue4", "5-95 Percentiles" = "lightblue3",
                                            "Minimum-Maxium" = "lightblue2")) +
      {if(log_discharge) ggplot2::scale_y_log10(expand = c(0,0))} +
      {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0))} +
      {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(0.07, "cm"),
                                                      mid = ggplot2::unit(0.15, "cm"), long = ggplot2::unit(0.2, "cm"))} +
      ggplot2::scale_x_discrete(expand = c(0.01,0.01)) +
      ggplot2::ylab("Discharge (cms)") +
      ggplot2::xlab(NULL) +
      ggplot2::theme_bw()+
      ggplot2::labs(color = 'Long-term Statistics', fill = "Monthly Ranges") +  
      # ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::theme(legend.position = "right",
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.justification = "top",
                     legend.text = ggplot2::element_text(size = 9),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10)) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linetype = c(2,2,1,1), shape = c(NA,NA,16,16))))
  )
  )
  
  
} 
