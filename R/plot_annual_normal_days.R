# Copyright 2022 Province of British Columbia
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

#' @title Plot annual count of normal days and days above and below normal
#'
#' @description Plots the number of days per year within, above and below the 'normal' range (typically between 25 and 75th percentiles) for
#'    each day of the year. Upper and lower-range percentiles are calculated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. Calculates statistics from all 
#'    values from complete years, unless specified. Data calculated using \code{calc_annual_normal_days()}
#'    function. Returns a list of plots.
#'
#' @inheritParams calc_annual_normal_days
#' @inheritParams plot_annual_stats
#'      
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Normal_Days}{a plot that contains the number of days outside normal}
#'   Default plots on each object:  
#'   \item{Normal_Days}{number of days per year below and above the daily normal (default 25/75th percentile)}
#'   \item{Below_Normal_Days}{number of days per year below the daily normal (default 25th percentile)}
#'   \item{Above_Normal_Days}{number of days per year above the daily normal (default 75th percentile)}
#'   
#' @seealso \code{\link{calc_annual_normal_days}}
#' 
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot annual statistics with default limits of normal (25 and 75th percentiles)
#' plot_annual_normal_days(station_number = "08NM116") 
#' 
#' # Plot annual statistics with custom limits of normal
#' plot_annual_normal_days(station_number = "08NM116",
#'                         normal_percentiles = c(10,90))
#'                            
#' }
#' @export



plot_annual_normal_days <- function(data,
                                    dates = Date,
                                    values = Value,
                                    groups = STATION_NUMBER,
                                    station_number,
                                    normal_percentiles = c(25,75),
                                    roll_days = 1,
                                    roll_align = "right",
                                    water_year_start = 1,
                                    start_year,
                                    end_year,
                                    exclude_years, 
                                    months = 1:12,
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
  if (missing(exclude_years)) {
    exclude_years <- NULL
  }
  
  logical_arg_check(include_title)
  
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
  
  normal_data <- calc_annual_normal_days(data = flow_data,
                                         normal_percentiles = normal_percentiles,
                                         roll_days = roll_days,
                                         roll_align = roll_align,
                                         water_year_start = water_year_start,
                                         start_year = start_year,
                                         end_year = end_year,
                                         exclude_years = exclude_years, 
                                         months = months)
  
  # Remove all leading NA years
  normal_data <- dplyr::filter(dplyr::group_by(normal_data, STATION_NUMBER),
                               Year >= Year[min(which(!is.na(.data[[names(normal_data)[3]]])))])
  normal_data <- tidyr::gather(normal_data, Statistic, Value, -STATION_NUMBER, -Year)
  normal_data <- dplyr::mutate(normal_data, Statistic = gsub("_", " ", Statistic))
  normal_data <- dplyr::mutate(normal_data, Statistic = gsub(" Days", "", Statistic))
  normal_data <- dplyr::mutate(normal_data, Statistic = factor(Statistic, 
                                                               levels = c("Above Normal", "Normal", "Below Normal")))

  # Create plots for each STATION_NUMBER in a tibble
  normal_plots <- dplyr::group_by(normal_data, STATION_NUMBER)
  normal_plots <- tidyr::nest(normal_plots)
  normal_plots <- dplyr::mutate(
    normal_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, fill = Statistic)) +
        ggplot2::geom_bar(position = "stack", stat = "identity", na.rm = TRUE, width=1, colour = "black") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                                    expand = c(0,0))+
        {if(length(unique(normal_data$Year)) < 8) ggplot2::scale_x_continuous(breaks = unique(normal_data$Year))}+
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                                    expand = c(0,0)) +
        ggplot2::ylab("Number of Days") +
        ggplot2::xlab(ifelse(water_year_start ==1, "Year", "Water Year"))+
      #  ggplot2::scale_fill_manual(values = colour_list, name = "Normal Category") +
        ggplot2::scale_fill_viridis_d(name = "Category") +
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
  plots <- normal_plots$plot
  if (nrow(normal_plots) == 1) {
    names(plots) <- "Annual_Normal_Days"
  } else {
    names(plots) <- paste0(normal_plots$STATION_NUMBER, "_Annual_Normal_Days")
  }

  plots

}

