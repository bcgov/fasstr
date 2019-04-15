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

#' @title Plot daily mean streamflow
#'
#' @description Plot the daily mean flow values from a streamflow dataset. Plots the statistics from all daily discharge values from all 
#'    years, unless specified. Can choose specific dates to start and end plotting. Can choose to plot out each year separately. Data 
#'    calculated using calc_daily_stats() function. Multiple groups/stations can be plotted if provided with the \code{groups} argument.
#'
#' @inheritParams calc_annual_stats
#' @inheritParams plot_annual_stats
#' @param start_date Date (YYYY-MM-DD) of first date to consider for plotting. Leave blank if all years are required.
#' @param end_date  Date (YYYY-MM-DD) of last date to consider for plotting. Leave blank if all years are required.
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#' @param plot_by_year Logical value to indicate whether to plot each year of data individually. Default \code{FALSE}.
#' @param one_plot Logical value to indicate whether to plot all groups/stations on one plot. Default \code{FALSE}.
#' 
#' @return A ggplot2 object of daily flows from flow_data or HYDAT flow data provided
#'
#' @examples
#' \dontrun{
#' 
#' # Plot data from a data frame
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_flow_data(data = flow_data)
#' 
#' # Plot data directly from HYDAT
#' plot_flow_data(station_number = "08NM116")
#' 
#' # Plot statistics with custom years
#' plot_flow_data(station_number = "08NM116",
#'                start_year = 1981,
#'                end_year = 2010,
#'                exclude_years = c(1991,1993:1995))
#'                  
#' # Plot data between specific dates
#' plot_flow_data(station_number = "08NM116",
#'                start_date = "1990-01-01",
#'                end_date = "1990-06-01")
#' 
#' # Plot data multiple groups on one plot
#' plot_flow_data(station_number = c("08NM241", "08NM242"),
#'                one_plot = TRUE) 
#' }
#' @export



plot_flow_data <- function(data,
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
                           start_date,
                           end_date,
                           log_discharge = FALSE,
                           plot_by_year = FALSE,
                           one_plot = FALSE,
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
  if (missing(start_date)) {
    start_date = "0000-01-01"
  }
  if (missing(end_date)) {
    end_date = "3000-12-31"
  }
  
  rolling_days_checks(roll_days, roll_align)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years = NULL)
  log_discharge_checks(log_discharge)
  include_title_checks(include_title)
  
  if (class(try(as.Date(start_date))) == "try-error") stop("start_date must be a date formatted YYYY-MM-DD.", call. = FALSE)
  if (class(try(as.Date(end_date))) == "try-error")   stop("end_date must be a date formatted YYYY-MM-DD.", call. = FALSE)
  if (start_date >= end_date)                         stop("start_date must be less than end_date.", call. = FALSE)
  
  if(!is.logical(plot_by_year))  stop("plot_by_year argument must be logical (TRUE/FALSE).")
  if(!is.logical(one_plot))  stop("one_plot argument must be logical (TRUE/FALSE).")
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = TRUE)
  
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add WaterYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  
  # Add rolling means to end of dataframe
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  flow_data <- dplyr::rename(flow_data, Value_orig = Value)
  colnames(flow_data)[ncol(flow_data)] <- "Value"
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)

  # Filter for specific dates, if selected
  flow_data <- dplyr::filter(flow_data, Date >= start_date)
  flow_data <- dplyr::filter(flow_data, Date <= end_date)
  
  # Remove selected excluded years
  flow_data <- dplyr::mutate(flow_data, Value = replace(Value, WaterYear %in% exclude_years, NA))
  
  if (anyNA(flow_data$Value)) 
    warning(paste0("Did not plot ", sum(is.na(flow_data$Value)),
                   " missing or excluded values between ", min(flow_data$Date), " and ", max(flow_data$Date),"."), 
            call. = FALSE)
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  
  # Plot each individual station on their own
  if (!one_plot) {
    flow_plots <- dplyr::group_by(flow_data, STATION_NUMBER)
    flow_plots <- tidyr::nest(flow_plots)
    flow_plots <- dplyr::mutate(flow_plots,
                                plot = purrr::map2(data, STATION_NUMBER, 
          ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date, y = Value)) +
            ggplot2::geom_line(colour = "dodgerblue4", na.rm = TRUE) +
            ggplot2::ylab(y_axis_title) +
            {if(plot_by_year) ggplot2::facet_wrap(~WaterYear, scales = "free_x")} +
            {if(!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0, 0))} +
            {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
            {if(plot_by_year) ggplot2::scale_x_date(date_labels = "%b", expand = c(0,0))} +
            {if(!plot_by_year) ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 12))} +
            {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(.$Value) * 1.05))} +
            {if(log_discharge) ggplot2::expand_limits(y = c(min(.$Value) * .95, max(.$Value) * 1.05))} +
            {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(.y) } +    
            ggplot2::theme_bw() +
            ggplot2::labs(color = 'Station') +    
            ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                           legend.position = "right", 
                           legend.spacing = ggplot2::unit(0, "cm"),
                           legend.justification = "top",
                           legend.text = ggplot2::element_text(size = 9),
                           panel.grid = ggplot2::element_line(size = .2),
                           axis.title = ggplot2::element_text(size = 12),
                           axis.text = ggplot2::element_text(size = 10),
                           plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"))
                                ))
    
    # Create a list of named plots extracted from the tibble
    plots <- flow_plots$plot
    if (nrow(flow_plots) == 1) {
      names(plots) <- ifelse(plot_by_year, "Annual_Daily_Flows","Daily_Flows")
    } else {
      names(plots) <- paste0(flow_plots$STATION_NUMBER, ifelse(plot_by_year, "_Annual_Daily_Flows","_Daily_Flows"))
    }
    
    
    
  # Plot all stations together
  } else {
    plots <- list()
    
    plot <- ggplot2::ggplot(data = flow_data, ggplot2::aes(x = Date, y = Value, colour = STATION_NUMBER)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::ylab(y_axis_title) +
      {if(plot_by_year) ggplot2::facet_wrap(~WaterYear, scales = "free_x")} +
      {if(!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0, 0))} +
      {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
      {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                      short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                      long = ggplot2::unit(.2, "cm"))} +
      {if(plot_by_year) ggplot2::scale_x_date(date_labels = "%b", expand = c(0,0))} +
      {if(!plot_by_year) ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 12))} +
      {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(flow_data$Value) * 1.05))} +
      {if(log_discharge) ggplot2::expand_limits(y = c(min(flow_data$Value) * .95, max(flow_data$Value) * 1.05))} +
      ggplot2::theme_bw() +
      ggplot2::labs(color = 'Station') +    
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     legend.position = "right", 
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.justification = "top",
                     legend.text = ggplot2::element_text(size = 9),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
    plots[[ paste(ifelse(plot_by_year, "Annual_Daily_Flows","Daily_Flows")) ]] <- plot
    
  }  
  
  
  plots
  
} 
