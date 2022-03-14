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
#'     For HYDAT data, symbols include: 'E' Estimate, 'A' Partial Day, 'C' Ice Conditions, 'D' Dry, and 'R' Revised. 
#'     Other symbols or categories may be used to colour points of plot.
#'     Returns a list of plots.
#'
#' @inheritParams plot_flow_data_symbols
#' @param plot_percent Logical.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Symbols}{a plot that contains the data availability for each year and month}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot annual symbol counts from a data frame and data argument
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_annual_symbols(data = flow_data)
#' 
#' # Plot annual symbol counts using station_number argument with defaults
#' plot_annual_symbols(station_number = "08NM116")
#' 
#' # Plot annual symbol percentages using station_number argument with defaults
#' plot_annual_symbols(station_number = "08NM116",
#'                     plot_percent = TRUE)
#'                   
#' }
#' @export

plot_annual_symbols <- function(data,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                symbols = Symbol,
                                station_number,
                                water_year_start = 1,
                                start_year,
                                end_year,
                                months = 1:12,
                                include_title = FALSE,
                                plot_percent = FALSE){           
  
  
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
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years = NULL)
  include_title_checks(include_title)
  months_checks(months)
  
  if (length(plot_percent) > 1)        stop("Only one plot_percent logical value can be listed.", call. = FALSE)
  if (!is.logical(plot_percent))       stop("plot_percent argument must be logical (TRUE/FALSE).", call. = FALSE)
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------

  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  flow_data$Symbols_fasstr <- dplyr::pull(flow_data[as.character(substitute(symbols))])
  
  symbol_data <- screen_flow_data(data = flow_data,
                                  symbols = "Symbols_fasstr",
                                  water_year_start = water_year_start,
                                  start_year = start_year,
                                  end_year = end_year,
                                  months = months,
                                  include_symbols = TRUE)
  
  
  symbol_data <- symbol_data[,1:which(names(symbol_data)=="Minimum")-1]
  symbol_data <- tidyr::pivot_longer(symbol_data, cols = 5:ncol(symbol_data), names_to = "Symbol", values_to = "Count")
  symbol_data <- dplyr::ungroup(symbol_data)
  symbol_data <- dplyr::mutate(symbol_data,
                               Percent = Count / n_days * 100,
                               Symbol = sub("\\_.*", "", Symbol),
                               Symbol = dplyr::case_when(Symbol == "n" ~ "Missing",
                                                         Symbol == "No" ~ "No Symbol",
                                                         TRUE ~ Symbol))
  
  y_title <- ifelse(!plot_percent, paste0("Number of Days"), paste0("Percent of Days"))
  
  # Plot the data
  sym_plots <- dplyr::group_by(symbol_data, STATION_NUMBER)
  sym_plots <- tidyr::nest(sym_plots)
  sym_plots <- dplyr::mutate(
    sym_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, fill = Symbol)) +
        {if (!plot_percent) ggplot2::geom_bar(mapping = ggplot2::aes(y = Count), position = "stack", stat = "identity", width=1, colour = "black") }+
        {if (plot_percent) ggplot2::geom_bar(mapping = ggplot2::aes(y = Percent), position = "stack", stat = "identity", width=1, colour = "black") }+
        ggplot2::ylab(y_title)+
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(.y) } +
        ggplot2::theme_bw()+
        ggplot2::scale_y_continuous(expand = c(0,0))+
        ggplot2::scale_x_continuous(expand = c(0,0))+
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       legend.position = "right",
                       legend.spacing = ggplot2::unit(0, "cm"),
                       legend.text = ggplot2::element_text(size = 9),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25")) +
        ggplot2::scale_fill_viridis_d(begin = 1, end = 0)
    ))
  
  
  # Create a list of named plots extracted from the tibble
  plots <- sym_plots$plot
  if (nrow(sym_plots) == 1) {
    names(plots) <- "Annual_Symbols"
  } else {
    names(plots) <- paste0(sym_plots$STATION_NUMBER, "_Annual_Symbols")
  }
  
  plots
  
}
