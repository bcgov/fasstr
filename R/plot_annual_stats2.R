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


#' @title Plot annual summary statistics (as ribbons)
#'
#' @description Plots means, medians, maximums, minimums, and percentiles as ribbons for each year from all years of a daily streamflow 
#'    data set. Calculates statistics from all values, unless specified. Data calculated using \code{calc_annual_stats()} function.
#'    Returns a list of plots.
#'
#' @inheritParams plot_annual_stats
#' @inheritParams plot_daily_stats
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{FALSE}.
#' @param log_ticks Logical value to indicate plotting logarithmic scale ticks when \code{log_discharge = TRUE}. Ticks will not
#'    appear when \code{log_discharge = FALSE}. Default to \code{TRUE} when \code{log_discharge = TRUE}.
#' @param include_title Logical value to indicate adding the group/station number to the plot, if provided. Default \code{FALSE}.
#' 
#'
#' @return A list of ggplot2 objects for with the following plots (percentile plots optional) for each station provided:
#'   \item{Annual_Stats}{a plot that contains annual statistics}
#'   Default plots on each object:  
#'   \item{Mean}{annual mean}
#'   \item{Median}{annual median}
#'   \item{25-75 Percentiles}{a ribbon showing the range of data between the annual 25th and 75th percentiles}
#'   \item{5-95 Percentiles}{a ribbon showing the range of data between the annual 5th and 95th percentiles}
#'   \item{Minimum-Maximum}{a ribbon showing the range of data between the annual minimum and maximums}
#'   
#' @seealso \code{\link{calc_annual_stats}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot annual statistics using a data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_annual_stats2(data = flow_data)
#' 
#' # Plot annual statistics using station_number argument with defaults
#' plot_annual_stats2(station_number = "08NM116")
#' 
#' # Plot annual statistics regardless if there is missing data for a given year
#' plot_annual_stats2(station_number = "08NM116",
#'                    ignore_missing = TRUE)
#'                   
#' # Plot annual statistics for water years starting in October
#' plot_annual_stats2(station_number = "08NM116",
#'                    water_year_start = 10) 
#'                   
#' }
#' @export


plot_annual_stats2 <- function(data,
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
                               months = 1:12,
                               ignore_missing = FALSE,
                               allowed_missing = ifelse(ignore_missing,100,0),
                               include_extremes = TRUE,
                               inner_percentiles = c(25,75),
                               outer_percentiles = c(5,95),
                               log_discharge = TRUE,
                               log_ticks = ifelse(log_discharge, TRUE, FALSE),
                               include_title = FALSE){ 
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(exclude_years)) {
    exclude_years <- NULL
  }
  if (missing(start_year)) {
    start_year <- 0
  }
  if (missing(end_year)) {
    end_year <- 9999
  }
  
  log_discharge_checks(log_discharge) 
  log_ticks_checks(log_ticks, log_discharge)
  include_title_checks(include_title)
  ptile_ribbons_checks(inner_percentiles, outer_percentiles)
  
  
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
  
  annual_stats_plot <- calc_annual_stats(data = flow_data,
                                         percentiles = c(inner_percentiles, outer_percentiles),
                                         roll_days = roll_days,
                                         roll_align = roll_align,
                                         water_year_start = water_year_start,
                                         start_year = start_year,
                                         end_year = end_year,
                                         exclude_years = exclude_years, 
                                         months = months,
                                         ignore_missing = ignore_missing,
                                         allowed_missing = allowed_missing)
  
  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)", #expression(Volume~(m^3))
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)", 
                                "Discharge (cms)")) #expression(Discharge~(m^3/s))
  fill_manual_list <- c()
  if (include_extremes) {
    fill_manual_list <- c(fill_manual_list, "lightblue2")
    names(fill_manual_list) <- c(names(fill_manual_list), "Minimum-Maximum")
  }
  
  if (is.numeric(outer_percentiles)) {
    fill_manual_list <- c(fill_manual_list, "lightblue3")
    outer_name <- paste0(min(outer_percentiles),"-",max(outer_percentiles), " Percentiles")
    names(fill_manual_list) <- c(names(fill_manual_list)[1:(length(fill_manual_list)-1)], outer_name)
  }
  
  if (is.numeric(inner_percentiles)) {
    fill_manual_list <- c(fill_manual_list, "lightblue4")
    inner_name <- paste0(min(inner_percentiles),"-",max(inner_percentiles), " Percentiles")
    names(fill_manual_list) <- c(names(fill_manual_list)[1:(length(fill_manual_list)-1)], inner_name)
  }
  
  colour_manual_list <- c("Mean" = "paleturquoise", "Median" = "dodgerblue4")
  colour_manual_labels <- c("Mean", "Median")
  
  # Create plots for each STATION_NUMBER in a tibble (see: http://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/)
  tidy_plots <- dplyr::group_by(annual_stats_plot, STATION_NUMBER)
  tidy_plots <- tidyr::nest(tidy_plots)
  tidy_plots <- dplyr::mutate(
    tidy_plots,
    plot = purrr::map2(
      data, STATION_NUMBER, 
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year)) +
        {if(include_extremes) ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = FALSE)} +
        {if(is.numeric(outer_percentiles)) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(outer_percentiles)),
                                                                                    ymax = paste0("P",max(outer_percentiles)),
                                                                                    fill = paste0("'",outer_name,"'")), na.rm = FALSE)} +
        {if(is.numeric(inner_percentiles)) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(inner_percentiles)),
                                                                                    ymax = paste0("P",max(inner_percentiles)),
                                                                                    fill = paste0("'",inner_name,"'")), na.rm = FALSE)} +
        ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = 1, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = 1, na.rm = TRUE) +
        ggplot2::scale_x_continuous(expand = c(0,0))+
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 8),
                                                        labels = scales::label_number(scale_cut = scales::cut_short_scale()))} +
        {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10),
                                                  labels = scales::label_number(scale_cut = scales::cut_short_scale()))} +
        {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                        short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                        long = ggplot2::unit(.2, "cm")) }+
        ggplot2::xlab(ifelse(water_year_start ==1, "Year", "Water Year"))+
        ggplot2::ylab("Discharge (cms)")+
        ggplot2::theme_bw()+
        ggplot2::labs(color = 'Annual Statistics') +
        {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nAnnual Statistics')) } +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                       axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                       axis.ticks = ggplot2::element_line(size = .1, colour = "grey25"),
                       axis.ticks.length = ggplot2::unit(0.05, "cm"),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,0,0,0)),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(size = .1),
                       legend.text = ggplot2::element_text(size = 9, colour = "grey25"),
                       legend.box = "vertical",
                       legend.justification = "top",
                       legend.key.size = ggplot2::unit(0.4, "cm"),
                       legend.spacing = ggplot2::unit(-0.4, "cm"),
                       legend.background = ggplot2::element_blank()) +
        ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2, title = NULL)) +
        ggplot2::scale_fill_manual(values = fill_manual_list) +
        ggplot2::scale_color_manual(values = colour_manual_list, labels = colour_manual_labels)
    ))
  
  
  # Create a list of named plots extracted from the tibble
  plots <- tidy_plots$plot
  if (nrow(tidy_plots) == 1) {
    names(plots) <- "Annual_Statistics"
  } else {
    names(plots) <- paste0(tidy_plots$STATION_NUMBER, "_Annual_Statistics")
  }
  
  plots
  
}