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


#' @title Plot annual summary statistics for data screening
#'
#' @description Plots the mean, median, maximum, minimum, standard deviation of annual flows. Plots the statistics from all daily 
#'    discharge values from all years, unless specified. Data calculated using screen_flow_data() function.
#'
#' @inheritParams screen_flow_data
#' @inheritParams plot_annual_stats
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Data_Screening}{a plot that contains annual summary statistics for screening}
#'   Default plots on each object:  
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{StandardDeviation}{annual 1 standard deviation of all daily flows for a given year}
#'   
#' @seealso \code{\link{screen_flow_data}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot screening statistics using a data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_data_screening(data = flow_data)
#' 
#' # Plot screening statistics using station_number argument with defaults
#' plot_data_screening(station_number = "08NM116")
#'                   
#' # Plot screening statistics for water years starting in October
#' plot_data_screening(station_number = "08NM116",
#'                  water_year_start = 10)
#'                   
#' # Plot screening statistics for 7-day flows for July-September months only
#' plot_data_screening(station_number = "08NM116",
#'                  roll_days = 7,
#'                  months = 7:9)
#'                  
#' }
#' @export



plot_data_screening <- function(data,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                station_number,
                                roll_days = 1,
                                roll_align = "right",
                                water_year_start = 1,
                                months = 1:12,
                                start_year,
                                end_year,
                                include_title = FALSE){ 
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(start_year)) {
    start_year <- 0
  }
  if (missing(end_year)) {
    end_year <- 9999
  }

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
  
  flow_summary <- screen_flow_data(data = flow_data,
                                   roll_days = roll_days,
                                   roll_align = roll_align,
                                   water_year_start = water_year_start,
                                   start_year = start_year,
                                   end_year = end_year,
                                   months = months)
  
  flow_summary <- dplyr::select(flow_summary, STATION_NUMBER, Year, Minimum, Maximum, Mean, "Standard Deviation" = StandardDeviation)
  flow_summary <- tidyr::gather(flow_summary, Statistic, Value, 3:6)
  
  
  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", expression(Volume~(m^3)),
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)", 
                                expression(Discharge~(m^3/s))))
  
  # Plot
  sum_plots <- dplyr::group_by(flow_summary, STATION_NUMBER)
  sum_plots <- tidyr::nest(sum_plots)
  sum_plots <- dplyr::mutate(sum_plots,
                                plot = purrr::map2(data, STATION_NUMBER, 
         ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value)) +
           ggplot2::geom_line(colour = "dodgerblue4", na.rm = TRUE) +
           ggplot2::geom_point(colour = "firebrick3", na.rm = TRUE) +
           ggplot2::facet_wrap(~Statistic, ncol = 2, scales = "free_y", strip.position = "top") +
           ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
           {if(length(unique(flow_summary$Year)) < 5) ggplot2::scale_x_continuous(breaks = unique(flow_summary$Year))}+
           ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
           ggplot2::expand_limits(y = 0) +
           ggplot2::ylab(y_axis_title) +
           ggplot2::xlab("Year") +
           ggplot2::theme_bw() +
           {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
           ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                          panel.grid = ggplot2::element_line(size = .2),
                          axis.title = ggplot2::element_text(size = 12),
                          axis.text = ggplot2::element_text(size = 10),
                          plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                          strip.background = ggplot2::element_blank(),
                          strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 10))
                                ))
  
  # Create a list of named plots extracted from the tibble
  plots <- sum_plots$plot
  if (nrow(sum_plots) == 1) {
    names(plots) <- "Data_Screening"
  } else {
    names(plots) <- paste0(sum_plots$STATION_NUMBER, "_Data_Screening")
  }
  
  plots
  
}

