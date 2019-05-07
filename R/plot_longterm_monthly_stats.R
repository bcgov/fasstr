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

#' @title Plot long-term summary statistics from annual monthly mean flows
#'
#' @description Plots the long-term and long-term monthly mean, median, maximum, minimum, and 5, 25, 75, and 95 percentiles of  
#'    annual monthly mean flow values from a single streamflow dataset. Plots statistics from discharge values from all 
#'    years, unless specified. Data calculated using calc_longterm_monthly_stats() function.
#'
#' @inheritParams calc_longterm_monthly_stats
#' @inheritParams plot_annual_stats
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{NA}.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Long-term_Monthly_Statistics}{a plot that contains long-term flow statistics}
#'   Default plots on each object:  
#'   \item{Monthly Mean}{mean of all annual monthly means for a given month over all years}
#'   \item{Monthly Median}{median of all annual monthly means for a given month over all years}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the monthly 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the monthly minimum and maximums}
#'   
#' @seealso \code{\link{calc_longterm_monthly_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_longterm_monthly_stats(data = flow_data,
#'                             start_year = 1980)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_longterm_monthly_stats(station_number = "08NM116",
#'                             start_year = 1980)
#' 
#' # Plot statistics regardless if there is missing data for a given year
#' plot_longterm_monthly_stats(station_number = "08NM116",
#'                             ignore_missing = TRUE)
#'                   
#' # Plot statistics for water years starting in October
#' plot_longterm_monthly_stats(station_number = "08NM116",
#'                             start_year = 1980,
#'                             end_year = 2010,
#'                             water_year_start = 10)
#'                   
#' # Plot statistics with custom years
#' plot_longterm_monthly_stats(station_number = "08NM116",
#'                             start_year = 1981,
#'                             end_year = 2010,
#'                             exclude_years = c(1991,1993:1995))
#' 
#' # Plot statistics without a log-scale Discharge axis
#' plot_longterm_monthly_stats(station_number = "08NM116",
#'                             start_year = 1981,
#'                             end_year = 2010,
#'                             log_discharge = FALSE)
#' }
#' @export


plot_longterm_monthly_stats <- function(data,
                                        dates = Date,
                                        values = Value,
                                        groups = STATION_NUMBER,
                                        station_number,
                                        percentiles,
                                        roll_days = 1,
                                        roll_align = "right",
                                        water_year_start = 1,
                                        start_year,
                                        end_year,
                                        exclude_years,
                                        complete_years = FALSE,
                                        ignore_missing = FALSE,
                                        log_discharge = FALSE,
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
  if (missing(percentiles)) {
    percentiles = NA
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
  
  longterm_stats <- calc_longterm_monthly_stats(data = flow_data,
                                                percentiles = percentiles,
                                                roll_days = roll_days,
                                                roll_align = roll_align,
                                                water_year_start = water_year_start,
                                                start_year = start_year,
                                                end_year = end_year,
                                                exclude_years = exclude_years,
                                                complete_years = complete_years,
                                                ignore_missing = ignore_missing)
  
  longterm_stats <- dplyr::filter(longterm_stats, Month != "Annual")
  longterm_stats_plot <- tidyr::gather(longterm_stats, Statistic, Value, -Month, -STATION_NUMBER)
  longterm_stats_plot <- dplyr::mutate(longterm_stats_plot, 
                                       Statistic = factor(Statistic, levels = colnames(longterm_stats[-(1:2)])))
  
  
  ## PLOT STATS
  ## ----------

  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(Value)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(Value)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  
  # Plot
  tidy_plots <- dplyr::group_by(longterm_stats_plot, STATION_NUMBER)
  tidy_plots <- tidyr::nest(tidy_plots)
  tidy_plots <- dplyr::mutate(
    tidy_plots,
    plot = purrr::map2(data, STATION_NUMBER, 
                       ~ggplot2::ggplot(data = ., ggplot2::aes(x = Month, y = Value, color = Statistic, group = Statistic)) +
                         ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
                         ggplot2::geom_line(alpha = 0.5, na.rm = TRUE) +
                         ggplot2::geom_point(na.rm = TRUE) +
                         {if(!log_discharge) ggplot2::expand_limits(y = c(0, suppressWarnings(max(.$Value, na.rm = T)) * 1.05))}+
                         {if(log_discharge) ggplot2::expand_limits(y = c(min(.$Value, na.rm = T) * .95, max(.$Value, na.rm = T) * 1.05))} +
                         {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 7))} +
                         {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 7, base = 10))} +
                         {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"), 
                                                                         mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
                         ggplot2::scale_x_discrete(expand = c(0.01,0.01))+
                         ggplot2::expand_limits(y = 0) +
                         ggplot2::ylab(y_axis_title)+
                         ggplot2::xlab("Year") +
                         # ggplot2::scale_color_brewer(palette = "Set1") +
                         ggplot2::theme_bw() +
                         ggplot2::labs(color = 'Monthly Statistics') +    
                         {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nMonthly Statistics')) }+    
                         ggplot2::theme(legend.position = "right", 
                                        legend.spacing = ggplot2::unit(0, "cm"),
                                        legend.justification = "top",
                                        legend.text = ggplot2::element_text(size = 9),
                                        panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                                        panel.grid = ggplot2::element_line(size = .2),
                                        axis.title = ggplot2::element_text(size = 12),
                                        axis.text = ggplot2::element_text(size = 10))
    ))
  

  # Create a list of named plots extracted from the tibble
  plots <- tidy_plots$plot
  if (nrow(tidy_plots) == 1) {
    names(plots) <- "Long-term_Monthly_Statistics"
  } else {
    names(plots) <- paste0(tidy_plots$STATION_NUMBER, "_Long-term_Monthly_Statistics")
  }
  
  plots
  
} 
