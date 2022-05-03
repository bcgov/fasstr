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


#' @title Plot daily streamflow data with their symbols
#'
#' @description Plots data symbols for a daily data set. A column of symbols is required, default \code{symbols = 'Symbol'}. 
#'     For HYDAT data, symbols include: 'E' Estimate, 'A' Partial Day, 'B' Ice Conditions, 'D' Dry, and 'R' Revised. 
#'     Other symbols or categories may be used to colour points of plot.
#'     Returns a list of plots.
#'
#' @inheritParams plot_flow_data
#' @param symbols Name of column in \code{data} that contains symbols. Only required if symbols column name is not 
#'    'Symbol' (default). Leave blank or set to \code{NULL} if using \code{station_number} argument.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Flow_Data_Symbols}{a plot that contains the flow data with symbol categories}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot data and symbols from a data frame and data argument
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_flow_data_symbols(data = flow_data)
#' 
#' # Plot data and symbols using station_number argument with defaults
#' plot_flow_data_symbols(station_number = "08NM116")
#'                   
#' }
#' @export

plot_flow_data_symbols <- function(data,
                                   dates = Date,
                                   values = Value,
                                   groups = STATION_NUMBER,
                                   symbols = Symbol,
                                   station_number,
                                   water_year_start = 1,
                                   start_year,
                                   end_year,
                                   exclude_years,
                                   months = 1:12,
                                   start_date,
                                   end_date,
                                   log_discharge = FALSE,
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
  if (missing(start_date)) {
    start_date <- "0000-01-01"
  }
  if (missing(end_date)) {
    end_date <- "3000-12-31"
  }

  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years = NULL)
  logical_arg_check(log_discharge)
  logical_arg_check(include_title)
  months_checks(months)

  if (start_date >= end_date)                         stop("start_date must be less than end_date.", call. = FALSE)
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               symbols = as.character(substitute(symbols)),
                               rm_other_cols = TRUE,
                               keep_symbols = TRUE)
  
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  
  # Filter for specific dates, if selected
  flow_data <- dplyr::filter(flow_data, Date >= start_date)
  flow_data <- dplyr::filter(flow_data, Date <= end_date)
  
  # Remove selected excluded years
  flow_data <- dplyr::mutate(flow_data, Value = replace(Value, WaterYear %in% exclude_years, NA))
  flow_data <- dplyr::mutate(flow_data, Value = replace(Value, !Month %in% months, NA))
  
  if (anyNA(flow_data$Value)) 
    message(paste0("Note: Did not plot ", sum(is.na(flow_data$Value)),
                   " missing or excluded values between ", min(flow_data$Date), " and ", max(flow_data$Date),"."))
  
  flow_data <- dplyr::mutate(flow_data,
                             Symbol = ifelse(is.na(Symbol), "No Symbol", Symbol),
                             Symbol = factor(Symbol, levels = c("No Symbol",unique(Symbol)[which(unique(Symbol) != "No Symbol")])))

# Plot the data
sym_plots <- dplyr::group_by(flow_data, STATION_NUMBER)
sym_plots <- tidyr::nest(sym_plots)
sym_plots <- dplyr::mutate(
  sym_plots,
  plot = purrr::map2(
    data, STATION_NUMBER,
    ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date, y = Value)) +
      ggplot2::geom_line(colour = "dodgerblue4", size = 0.2, na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(color = Symbol), size = 1.5, na.rm = TRUE)+
      ggplot2::ylab("Daily Discharge (cms)") +
      {if(!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0, 0))} +
      {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
      {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(.$Value) * 1.05))} +
      {if(log_discharge) ggplot2::expand_limits(y = c(min(.$Value) * .95, max(.$Value) * 1.05))} +
      {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(.y) } +
      ggplot2::scale_color_viridis_d()+    
      ggplot2::theme_bw() +
      ggplot2::labs(color = 'Symbol') +    
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     legend.position = "right", 
                     legend.spacing = ggplot2::unit(0, "cm"),
                     legend.text = ggplot2::element_text(size = 9),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10),
                     plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"))
  ))


# Create a list of named plots extracted from the tibble
plots <- sym_plots$plot
if (nrow(sym_plots) == 1) {
  names(plots) <- "Flow_Data_Symbols"
} else {
  names(plots) <- paste0(sym_plots$STATION_NUMBER, "_Flow_Data_Symbols")
}

plots

}
