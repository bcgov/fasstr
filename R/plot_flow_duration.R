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



#' @title Plot flow duration curves
#'
#' @description Plots flow duration curves, percent time a flow value is equalled or exceeded, for a streamflow dataset. Plots 
#'    statistics from all daily discharge values from all years, unless specified. Data calculated using calc_longterm_stats() 
#'    function then converted for plotting.
#'
#' @inheritParams calc_longterm_stats
#' @inheritParams plot_annual_stats
#' @param months Numeric vector of month curves to plot. NA if no months required. Default \code{1:12}.
#' @param incl_longterm Logical value indicating whether to include longterm curve of all data. Default \code{TRUE}.
#'
#' @return A ggplot2 object with plots for each month, long-term, and custom months showing the percentage of time that 
#'    flows are likely equal or exceeded for each time period.
#'   
#' @examples
#' \dontrun{
#' 
#' plot_flow_duration(station_number = "08NM116", 
#'                    water_year = TRUE, 
#'                    water_year_start = 8)
#'
#' }
#' @export



plot_flow_duration <- function(data = NULL,
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
                               custom_months = NULL,
                               custom_months_label = "Custom-Months",
                               ignore_missing = FALSE,
                               months = 1:12,
                               incl_longterm = TRUE,
                               log_discharge = TRUE){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  log_discharge_checks(log_discharge)
  custom_months_checks(custom_months, custom_months_label)
  one_station_number_stop(station_number)
  
  
  if (length(incl_longterm) > 1)   stop("Only one incl_longterm logical value can be listed.", call. = FALSE)
  if (!is.logical(incl_longterm))  stop("incl_longterm argument must be logical (TRUE/FALSE).", call. = FALSE)
  
  
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
  
  percentiles_data <- calc_longterm_stats(data = flow_data,
                                          percentiles = c(.01,.1,.2:9.8,10:90,90.2:99.8,99.9,99.99),
                                          roll_days = roll_days,
                                          roll_align = roll_align,
                                          water_year = water_year,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years,
                                          complete_years = complete_years,
                                          custom_months = custom_months,
                                          ignore_missing = ignore_missing)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(percentiles_data)) {
    percentiles_data <- dplyr::ungroup(percentiles_data)
    percentiles_data <- dplyr::select(percentiles_data, -STATION_NUMBER)
  }
  
  # Setup and calculate the probabilites
  percentiles_data <- dplyr::select(percentiles_data, -Mean, -Median, -Maximum, -Minimum)
  percentiles_data <- tidyr::gather(percentiles_data, Percentile, Value, -1)
  percentiles_data <- dplyr::mutate(percentiles_data, Percentile = 100 - as.numeric(gsub("P", "", Percentile)))
  
  # Filter for months and longterm selected to plot
  include <- month.abb[months]
  if (incl_longterm) { include <- c(include, "Long-term") }
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
  
  
  ## PLOT STATS
  ## ----------
  suppressWarnings(
    ggplot2::ggplot(percentiles_data, ggplot2::aes(x = Percentile, y = Value, colour = Month)) +
      ggplot2::geom_line() +
      {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))} +
      {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0, 0))} +
      ggplot2::scale_x_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::ylab("Discharge (cms)") +
      ggplot2::xlab("% Time flow equalled or exceeded") +
      ggplot2::scale_color_manual(values = colour_list) +
      ggplot2:: annotation_logticks(sides = "l", base = 10, colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"),
                                    mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))+
      ggplot2::labs(color = 'Period') +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     legend.justification = "top",
                     axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                     axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                     legend.text = ggplot2::element_text(size = 9, colour = "grey25"))
  )
  
  
}
