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



#' @title Plot flow duration curves
#'
#' @description Plots flow duration curves, percent time a flow value is equalled or exceeded, for a streamflow dataset. Plots 
#'    statistics from all daily discharge values from all years, unless specified. Data calculated using calc_longterm_stats() 
#'    function then converted for plotting.
#'
#' @inheritParams calc_longterm_stats
#' @inheritParams plot_annual_stats
#' @param months Numeric vector of month curves to plot. NA if no months required. Default \code{1:12}.
#' @param include_longterm Logical value indicating whether to include longterm curve of all data. Default \code{TRUE}.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Flow_Duration}{a plot that contains flow duration curves for each month, long-term, and (option) customized months}
#'   
#' @seealso \code{\link{calc_longterm_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_flow_duration(data = flow_data,
#'                     start_year = 1980)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_flow_duration(station_number = "08NM116",
#'                    start_year = 1980)
#' 
#' # Plot statistics regardless if there is missing data for a given year
#' plot_flow_duration(station_number = "08NM116",
#'                    ignore_missing = TRUE)
#'                   
#' # Plot statistics for water years starting in October
#' plot_flow_duration(station_number = "08NM116",
#'                    start_year = 1980,
#'                    end_year = 2010,
#'                    water_year_start = 10)
#'                   
#' # Plot statistics with custom years
#' plot_flow_duration(station_number = "08NM116",
#'                    start_year = 1981,
#'                    end_year = 2010,
#'                    exclude_years = c(1991,1993:1995))
#' 
#' # Plot statistics and add custom stats for July-September
#' plot_flow_duration(station_number = "08NM116",
#'                    start_year = 1980,
#'                    custom_months = 7:9,
#'                    custom_months_label = "Summer")  
#' 
#' # Plot statistics for just July-September
#' plot_flow_duration(station_number = "08NM116",
#'                    start_year = 1980,
#'                    months = 7:9,
#'                    include_longterm = FALSE)
#' }
#' @export



plot_flow_duration <- function(data,
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
                               custom_months,
                               custom_months_label,
                               ignore_missing = FALSE,
                               months = 1:12,
                               include_longterm = TRUE,
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
  if (missing(custom_months)) {
    custom_months = NULL
  }
  if (missing(custom_months_label)) {
    custom_months_label = "Custom-Months"
  }
  
  log_discharge_checks(log_discharge)
  custom_months_checks(custom_months, custom_months_label)
  include_title_checks(include_title)
  include_longterm_checks(include_longterm)
    
  
  
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
  
  percentiles_data <- calc_longterm_daily_stats(data = flow_data,
                                                percentiles = c(.01,.1,.2:9.8,10:90,90.2:99.8,99.9,99.99),
                                                roll_days = roll_days,
                                                roll_align = roll_align,
                                                water_year_start = water_year_start,
                                                start_year = start_year,
                                                end_year = end_year,
                                                exclude_years = exclude_years,
                                                complete_years = complete_years,
                                                custom_months = custom_months,
                                                ignore_missing = ignore_missing)
  

  
  # Setup and calculate the probabilites
  percentiles_data <- dplyr::select(percentiles_data, -Mean, -Median, -Maximum, -Minimum)
  percentiles_data <- tidyr::gather(percentiles_data, Percentile, Value, -STATION_NUMBER, -Month)
  percentiles_data <- dplyr::mutate(percentiles_data, Percentile = 100 - as.numeric(gsub("P", "", Percentile)))
  
  # Filter for months and longterm selected to plot
  include <- month.abb[months]
  if (include_longterm) { include <- c(include, "Long-term") }
  if (!is.null(custom_months)) { include <- c(include, "Custom-Months") }
  percentiles_data <- dplyr::filter(percentiles_data, Month %in% include)
  
  # Rename the custom months
  if (!is.null(custom_months)) { 
    levels(percentiles_data$Month) <- c(levels(percentiles_data$Month), custom_months_label)    
    percentiles_data <- dplyr::mutate(percentiles_data,
                                      Month = replace(Month, Month == "Custom-Months", custom_months_label))
  }
  
  # Create list of colours for plot, and add custom_months if necessary
  colour_list <-  c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                    "Apr" = "forestgreen", "May" = "limegreen", "Jun" = "gold", "Jul" = "orange",
                    "Aug" = "red", "Sep" = "darkred", "Oct" = "orchid", "Nov" = "purple3",
                    "Dec" = "midnightblue", "Long-term" = "black")
  if (!is.null(custom_months)) { 
    colour_list[[ custom_months_label ]] = "grey60"
  }

  if (all(is.na(percentiles_data$Value))) {
    percentiles_data[is.na(percentiles_data)] <- 1
  }
  
  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Daily Volume (m3)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Daily Yield (mm)", 
                                "Daily Discharge (cms)"))
  
  flow_plots <- dplyr::group_by(percentiles_data, STATION_NUMBER)
  flow_plots <- tidyr::nest(flow_plots)
  flow_plots <- dplyr::mutate(flow_plots,
                            plot = purrr::map2(data, STATION_NUMBER,
    ~ggplot2::ggplot(data = ., ggplot2::aes(x = Percentile, y = Value, colour = Month)) +
      ggplot2::geom_line(na.rm = TRUE) +
      {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
      {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0))} +
      ggplot2::scale_x_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::ylab(y_axis_title) +
      ggplot2::xlab("% Time flow equalled or exceeded") +
      ggplot2::scale_color_manual(values = colour_list) +
      ggplot2:: annotation_logticks(sides = "l", base = 10, colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"),
                                    mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))+
      ggplot2::labs(color = 'Period') +
      {if (include_title & unique(.y) != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nPeriod')) } +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     legend.justification = "top",
                     axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                     axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                     legend.text = ggplot2::element_text(size = 9, colour = "grey25"))
                            ))

  # Create a list of named plots extracted from the tibble
  plots <- flow_plots$plot
  if (nrow(flow_plots) == 1) {
    names(plots) <- "Flow_Duration"
  } else {
    names(plots) <- paste0(flow_plots$STATION_NUMBER, "_Flow_Duration")
  }

  plots

}
