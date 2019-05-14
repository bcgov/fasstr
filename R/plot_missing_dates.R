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


#' @title Plot annual and monthly missing dates
#'
#' @description Plots the number of missing data for each month of each year. Calculates the statistics from all daily discharge 
#'    values from all years, unless specified. Data calculated using screen_flow_data() function.
#'
#' @inheritParams screen_flow_data
#' @inheritParams plot_annual_stats
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Missing_Dates}{a plot that contains the number of missing dates for each year and month}
#'   
#' @seealso \code{\link{screen_flow_data}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_missing_dates(data = flow_data)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_missing_dates(station_number = "08NM116")
#'                   
#' # Plot statistics for water years starting in October
#' plot_missing_dates(station_number = "08NM116",
#'                    water_year_start = 9)
#'                   
#' # Plot statistics for 7-day flows for July-September months only
#' plot_missing_dates(station_number = "08NM116",
#'                    roll_days = 7,
#'                    months = 7:9)
#' }
#' @export




plot_missing_dates <- function(data,
                               dates = Date,
                               values = Value,
                               groups = STATION_NUMBER,
                               station_number,
                               roll_days = 1,
                               roll_align = "right",
                               water_year_start = 1,
                               start_year,
                               end_year,
                               months = 1:12,
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
  

  missing_plotdata <- flow_summary[,c(1,2,11:ncol(flow_summary))]
  missing_plotdata <- tidyr::gather(missing_plotdata, Month, Value, 3:ncol(missing_plotdata))
  
  missing_plotdata <- dplyr::mutate(missing_plotdata, Month = substr(Month, 1, 3))
  
  
  # Set the levels for plot ordering
  missing_plotdata$Month <- factor(missing_plotdata$Month, levels = month.abb[c(water_year_start:12, 1:water_year_start-1)])
  

  ## PLOT STATS
  ## ----------
  
  miss_plots <- dplyr::group_by(missing_plotdata, STATION_NUMBER)
  miss_plots <- tidyr::nest(miss_plots)
  miss_plots <- dplyr::mutate(miss_plots,
                             plot = purrr::map2(data, STATION_NUMBER, 
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value)) +
        ggplot2::geom_bar(colour = "cornflowerblue", fill = "cornflowerblue", na.rm = TRUE, stat = "identity") +
        ggplot2::facet_wrap(~Month, ncol = 3, scales = "fixed", strip.position = "top") +
        ggplot2::ylab("Missing Days") +
        ggplot2::xlab("Year") +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous(limits = c(0, 32)) +
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
  plots <- miss_plots$plot
  if (nrow(miss_plots) == 1) {
    names(plots) <- "Missing_Dates"
  } else {
    names(plots) <- paste0(miss_plots$STATION_NUMBER, "_Missing_Dates")
  }
  
  plots
      
}
