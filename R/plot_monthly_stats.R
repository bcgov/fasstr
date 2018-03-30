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

#' @title Plot monthly summary statistics
#'
#' @description Plots monthly mean, median, maximum, minimum, and percentiles for each month of all years of daily flow values 
#'    from a streamflow dataset. Plots the statistics from all daily discharge values from all years, unless specified. Data
#'    calculated using the calc_monthly_stats() function.
#'
#' @inheritParams calc_monthly_stats
#' @inheritParams plot_annual_stats
#' 
#' @return A list of ggplot2 objects for each monthly statistic:
#'   \item{Monthly Mean Flows}{mean of all daily flows for a given month and year}
#'   \item{Monthly Median Flows}{median of all daily flows for a given month and year}
#'   \item{Monthly Maximum Flows}{maximum of all daily flows for a given month and year}
#'   \item{Monthly Minimum Flows}{minimum of all daily flows for a given month and year}
#'   \item{Monthly P'n' Flows}{each n-th percentile selected for a given month and year}
#'   Default percentile plots:
#'   \item{Monthly P10 Flows}{10th percentile of all daily flows for a given month and year}
#'   \item{Monthl P20 Flows}{20th percentile of all daily flows for a given month and year}
#'   
#' @examples
#' \dontrun{
#' 
#' plot_monthly_stats(station_number = "08NM116", 
#'                    water_year = TRUE, 
#'                    water_year_start = 8, 
#'                    percentiles = c(1:10))
#'
#' plot_monthly_stats(station_number = "08NM116", 
#'                    months = 7:9)
#'
#' }
#' @export



plot_monthly_stats <- function(data = NULL,
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
  ## others will be check in calc_ function
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
  
  monthly_data <- calc_monthly_stats(data = flow_data,
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
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(monthly_data)) {
    monthly_data <- dplyr::ungroup(monthly_data)
    monthly_data <- dplyr::select(monthly_data, -STATION_NUMBER)
  }
  
  monthly_data <- tidyr::gather(monthly_data, Statistic, Value, -(1:2))
  
  
  ## PLOT STATS
  ## ----------
  
  # Create list to add each plot to
  monthly_stats_plots <- list()
  
  # Cycle through each stat and add to list
  for (stat in unique(monthly_data$Statistic)) {
    monthly_data_plot <- dplyr::filter(monthly_data, Statistic == stat)
    monthly_plot <- ggplot2::ggplot(data = monthly_data_plot, ggplot2::aes(x = Year, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(colour = Month), alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(colour = Month)) +
      ggplot2::facet_wrap(~Month, scales = "free_x") +
      ggplot2::ggtitle(paste0("Monthly ", stat, " Flows")) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      {if(!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))} +
      {if(log_discharge) ggplot2::scale_y_log10()} +
      {if(log_discharge) ggplot2::annotation_logticks(base = 10, "left", colour = "grey25", size = 0.3,
                                                      short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"), 
                                                      long = ggplot2::unit(.2, "cm"))} +
      ggplot2::ylab("Discharge (cms)") +
      ggplot2::guides(colour = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10)) +
      ggplot2::scale_colour_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                              "Apr" = "forestgreen", "May" = "limegreen", "Jun" = "gold",
                                              "Jul" = "orange", "Aug" = "red", "Sep" = "darkred",
                                              "Oct" = "orchid", "Nov" = "purple3", "Dec" = "midnightblue"))
    
    monthly_stats_plots[[paste0("Monthly_",stat)]] <- monthly_plot

  }
  
  
  suppressWarnings(
    monthly_stats_plots
  )
  
}

