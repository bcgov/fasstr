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

#' @title Plot the long-term and long-term monthly summary statistics
#'
#' @description Plots the long-term and long-term monthly mean, median, maximum, minimum, and 5, 25, 75, and 95 percentiles of daily 
#'    flow values from a single streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. 
#'    Data calculated using calc_longterm_stats() function.
#'
#' @inheritParams calc_longterm_stats
#' @inheritParams plot_annual_stats
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Long-term_Stats}{a plot that contains long-term flow statistics}
#'   Default plots on each object:  
#'   \item{Long-term Mean}{mean of all daily flows over all years}
#'   \item{Long-term Median}{median of all daily flows over all years}
#'   \item{Monthly Mean}{mean of all daily flows for each month over all years}
#'   \item{Monthly Median}{median of all daily flows for each month over all years}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the monthly 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the monthly minimum and maximums}
#'   
#' @seealso \code{\link{calc_longterm_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_longterm_daily_stats(data = flow_data,
#'                           start_year = 1980)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980)
#' 
#' # Plot statistics regardless if there is missing data for a given year
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           ignore_missing = TRUE)
#'                   
#' # Plot statistics for water years starting in October
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980,
#'                           end_year = 2010,
#'                           water_year_start = 10)
#'                   
#' # Plot statistics with custom years
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1981,
#'                           end_year = 2010,
#'                           exclude_years = c(1991,1993:1995))
#' 
#' # Plot statistics without a log-scale Discharge axis
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1981,
#'                           end_year = 2010,
#'                           log_discharge = FALSE)
#' }
#' @export


plot_longterm_daily_stats <- function(data,
                                      dates = Date,
                                      values = Value,
                                      groups = STATION_NUMBER,
                                      station_number,
                                      roll_days = 1,
                                      roll_align = "right",
                                      water_year_start = 1,
                                      start_year,
                                      end_year,
                                      exclude_years,
                                      complete_years = FALSE,
                                      ignore_missing = FALSE,
                                      log_discharge = TRUE,
                                      include_title = FALSE){
  
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
  
  log_discharge_checks(log_discharge)
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
  
  longterm_stats <- calc_longterm_daily_stats(data = flow_data,
                                              percentiles = c(5,25,75,95),
                                              roll_days = roll_days,
                                              roll_align = roll_align,
                                              water_year_start = water_year_start,
                                              start_year = start_year,
                                              end_year = end_year,
                                              exclude_years = exclude_years,
                                              complete_years = complete_years,
                                              ignore_missing = ignore_missing)
  
  
  ## PLOT STATS
  ## ----------
  
  # Make longterm mean and median their own columns
  longterm_stats_months <- dplyr::filter(longterm_stats, Month != "Long-term")
  longterm_stats_longterm <- dplyr::filter(longterm_stats, Month == "Long-term")
  longterm_stats_longterm <- dplyr::select(longterm_stats_longterm, STATION_NUMBER, "LT_Mean" = Mean, "LT_Med" = Median)
  longterm_stats <- dplyr::left_join(longterm_stats_months, longterm_stats_longterm, by = "STATION_NUMBER")
  
  if (all(sapply(longterm_stats[3:ncol(longterm_stats)], function(x)all(is.na(x))))) {
    longterm_stats[is.na(longterm_stats)] <- 1
  }
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(Value)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(Value)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  # Plot
  lt_plots <- dplyr::group_by(longterm_stats, STATION_NUMBER)
  lt_plots <- tidyr::nest(lt_plots)
  lt_plots <- dplyr::mutate(lt_plots,
                            plot = purrr::map2(data, STATION_NUMBER, 
                                               ~ggplot2::ggplot(data = ., ggplot2::aes(x = Month, group = 1)) +
                                                 ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = TRUE) +
                                                 ggplot2::geom_ribbon(ggplot2::aes(ymin = P5, ymax = P95, fill = "5-95 Percentiles"), na.rm = TRUE) +
                                                 ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "25-75 Percentiles"), na.rm = TRUE) +
                                                 ggplot2::geom_line(ggplot2::aes(y = LT_Mean, colour = "Long-term Mean"), size = 0.7, linetype = 2, na.rm = TRUE) +
                                                 ggplot2::geom_line(ggplot2::aes(y = LT_Med, colour = "Long-term Median"), size = 0.7, linetype = 2, na.rm = TRUE) +
                                                 ggplot2::geom_line(ggplot2::aes(y = Mean, color = "Monthly Mean"), size = 0.7, na.rm = TRUE) +
                                                 ggplot2::geom_line(ggplot2::aes(y = Median, color = "Monthly Median"), size = 0.7, na.rm = TRUE) +
                                                 ggplot2::geom_point(ggplot2::aes(y = Mean, color = "Monthly Mean"), size = 2, na.rm = TRUE) +
                                                 ggplot2::geom_point(ggplot2::aes(y = Median, color = "Monthly Median"), size = 2, na.rm = TRUE) +
                                                 ggplot2::scale_color_manual(values = c("Monthly Mean" = "skyblue2", "Monthly Median" = "dodgerblue4",
                                                                                        "Long-term Mean" = "forestgreen", "Long-term Median" = "darkorchid4")) +
                                                 ggplot2::scale_fill_manual(values = c("25-75 Percentiles" = "lightblue4", "5-95 Percentiles" = "lightblue3",
                                                                                       "Minimum-Maximum" = "lightblue2")) +
                                                                                       {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 8))}+
                                                                                       {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
                                                                                       {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(0.07, "cm"),
                                                                                                                                       mid = ggplot2::unit(0.15, "cm"), long = ggplot2::unit(0.2, "cm"))} +
                                                 ggplot2::scale_x_discrete(expand = c(0.01,0.01)) +
                                                 ggplot2::ylab(y_axis_title) +
                                                 ggplot2::xlab(NULL) +
                                                 ggplot2::theme_bw()+
                                                 ggplot2::labs(color = 'Long-term Statistics', fill = "Monthly Ranges") + 
                                                 {if (include_title & unique(.y) != "XXXXXXX") ggplot2::labs(color = 'Long-term Statistics', fill = paste0(.y,'\n \nMonthly Ranges')) } +    
                                                 # ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
                                                 ggplot2::theme(legend.position = "right",
                                                                legend.spacing = ggplot2::unit(0, "cm"),
                                                                legend.justification = "top",
                                                                legend.text = ggplot2::element_text(size = 9),
                                                                panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                                                                panel.grid = ggplot2::element_line(size = .2),
                                                                axis.title = ggplot2::element_text(size = 12),
                                                                axis.text = ggplot2::element_text(size = 10)) +
                                                 ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linetype = c(2,2,1,1), shape = c(NA,NA,16,16), order = 1)),
                                                                 fill = ggplot2::guide_legend(order = 2))
                            ))
  
  
  # Create a list of named plots extracted from the tibble
  plots <- lt_plots$plot
  if (nrow(lt_plots) == 1) {
    names(plots) <- "Long-term_Daily_Statistics"
  } else {
    names(plots) <- paste0(lt_plots$STATION_NUMBER, "_Long-term_Daily_Statistics")
  }
  
  plots
  
} 
