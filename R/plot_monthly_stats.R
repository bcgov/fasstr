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

#' @title Plot monthly summary statistics
#'
#' @description Plots monthly mean, median, maximum, minimum, and percentiles for each month of all years of daily flow values 
#'    from a streamflow dataset. Plots the statistics from all daily discharge values from all years, unless specified. Data
#'    calculated using the calc_monthly_stats() function.
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{NA}.
#' 
#' @inheritParams calc_monthly_stats
#' @inheritParams plot_annual_stats
#' 
#' @return A list of ggplot2 objects for each monthly statistic for each station provided that contain:
#'   \item{Monthly Mean Flows}{mean of all daily flows for a given month and year}
#'   \item{Monthly Median Flows}{median of all daily flows for a given month and year}
#'   \item{Monthly Maximum Flows}{maximum of all daily flows for a given month and year}
#'   \item{Monthly Minimum Flows}{minimum of all daily flows for a given month and year}
#'   \item{Monthly P'n' Flows}{(optional) each n-th percentile selected for a given month and year}
#'   
#' @seealso \code{\link{calc_monthly_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_monthly_stats(data = flow_data,
#'                    start_year = 1980)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1980)
#' 
#' # Plot statistics regardless if there is missing data for a given day of year
#' plot_monthly_stats(station_number = "08NM116",
#'                    ignore_missing = TRUE)
#'                   
#' # Plot statistics for water years starting in October
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1980,
#'                    end_year = 2010,
#'                    water_year_start = 10)
#'                  
#' # Plot statistics with custom years
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1981,
#'                    end_year = 2010,
#'                    exclude_years = c(1991,1993:1995))
#'                   
#' # Plot statistics for 7-day flows for July-September months only, and the 10th percentiles
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1980,
#'                    roll_days = 7,
#'                    months = 7:9,
#'                    percentiles = 10)
#' 
#' # Plot statistics with a log-scale Discharge axis
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1981,
#'                    end_year = 2010,
#'                    log_discharge = TRUE)
#' 
#' # Plot statistics and include the title with the statistic
#' plot_monthly_stats(station_number = "08NM116",
#'                    start_year = 1981,
#'                    end_year = 2010,
#'                    include_title = TRUE)
#' }
#' @export



plot_monthly_stats <- function(data,
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
                               months = 1:12,
                               ignore_missing = FALSE,
                               log_discharge = FALSE,
                               include_title = FALSE){
  
  
  ## ARGUMENT CHECKS 
  ## others will be check in calc_ function
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
  
  monthly_data <- calc_monthly_stats(data = flow_data,
                                     percentiles = percentiles,
                                     roll_days = roll_days,
                                     roll_align = roll_align,
                                     water_year_start = water_year_start,
                                     start_year = start_year,
                                     end_year = end_year,
                                     exclude_years = exclude_years, 
                                     months = months,
                                     ignore_missing = ignore_missing)
  

  monthly_data <- tidyr::gather(monthly_data, Statistic, Value, -(1:3))
  monthly_data <- dplyr::mutate(monthly_data, Stat2 = Statistic)
    
  # monthly_data
  ## PLOT STATS
  ## ----------

  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  
  # Create the daily stats plots
  monthly_plots <- dplyr::group_by(monthly_data, STATION_NUMBER, Statistic)
  monthly_plots <- tidyr::nest(monthly_plots)
  monthly_plots <- dplyr::mutate(monthly_plots,
                              plot = purrr::map2(data, STATION_NUMBER,
          ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, colour = Month)) +
            ggplot2::geom_line(alpha = 0.5, na.rm = TRUE) +
            ggplot2::geom_point(na.rm = TRUE) +
            ggplot2::facet_wrap(~Month, scales = "fixed", strip.position = "top") +
            #ggplot2::ggtitle(paste0("Monthly ", stat, " Flows")) +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
            {if(length(unique(monthly_data$Year)) < 6) ggplot2::scale_x_continuous(breaks = unique(monthly_data$Year))}+
            {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 6))} +
            {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
            {if(log_discharge) ggplot2::annotation_logticks(base = 10, "left", colour = "grey25", size = 0.3,
                                                            short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                            long = ggplot2::unit(.2, "cm"))} +
            ggplot2::ylab(y_axis_title) +
            ggplot2::guides(colour = FALSE) +
            ggplot2::theme_bw() +
            {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y, unique(.$Stat2))) } +
            {if (include_title & .y == "XXXXXXX") ggplot2::ggtitle(paste(unique(.$Stat2))) } +
            ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                           panel.grid = ggplot2::element_line(size = .2),
                           axis.title = ggplot2::element_text(size = 12),
                           axis.text = ggplot2::element_text(size = 10),
                           plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                           strip.background = ggplot2::element_blank(),
                           strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 10)) +
            ggplot2::scale_colour_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                                    "Apr" = "forestgreen", "May" = "limegreen", "Jun" = "gold",
                                                    "Jul" = "orange", "Aug" = "red", "Sep" = "darkred",
                                                    "Oct" = "orchid", "Nov" = "purple3", "Dec" = "midnightblue"))
                              ))


  # Create a list of named plots extracted from the tibble
  plots <- monthly_plots$plot
  if (length(unique(monthly_plots$STATION_NUMBER)) == 1) {
    names(plots) <- paste0(monthly_plots$Statistic, "_Monthly_Statistics")
  } else {
    names(plots) <- paste0(monthly_plots$STATION_NUMBER, "_", monthly_plots$Statistic, "_Monthly_Statistics")
  }

  plots

  
}

