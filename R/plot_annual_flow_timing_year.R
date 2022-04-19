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

#' @title Plot annual timing of flows for a specific year
#'
#' @description Plots an annual hydrograph for a specific year with the dates of flow timing of portions of total annual flow identified.
#'    The 'normal' range of percentiles also plotted for reference and are calculated from only years of complete data. 
#'    Shows the dates of flow timing for a specific year from the counts from the \code{plot_annual_flow_timing()} function. 
#'    Returns a list of plots.
#'
#' @inheritParams calc_annual_flow_timing
#' @inheritParams plot_annual_stats
#' @inheritParams plot_annual_outside_normal_year
#' @param plot_vlines Logical value indicating whether to plot the vertical lines indicating dates of flow timing. Default \code{TRUE}.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Normal_Days_Year}{a plot that contains the above, below, and normal colour daily flow points}
#'   
#' @seealso \code{\link{calc_annual_flow_timing}}
#' @seealso \code{\link{plot_annual_flow_timing}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot the year 2000 and change the flow timing percent totals        
#' plot_annual_flow_timing_year(station_number = "08NM116",
#'                              percent_total = 50,
#'                              year_to_plot = 2000)
#'                  
#' }
#' @export


plot_annual_flow_timing_year <- function(data,
                                         dates = Date,
                                         values = Value,
                                         groups = STATION_NUMBER,
                                         station_number,
                                         percent_total = c(25, 33.3, 50, 75),
                                         year_to_plot = NA,
                                         roll_days = 1,
                                         roll_align = "right",
                                         water_year_start = 1,
                                         start_year,
                                         end_year,
                                         exclude_years,
                                         months = 1:12,
                                         log_discharge = TRUE,
                                         log_ticks = FALSE,
                                         include_title = FALSE,
                                         plot_vlines = TRUE,
                                         plot_normal_percentiles = TRUE,
                                         normal_percentiles = c(25,75)){
  
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
  
  logical_arg_check(log_discharge) 
  log_ticks_checks(log_ticks, log_discharge)
  logical_arg_check(include_title)
  logical_arg_check(plot_vlines)
  logical_arg_check(plot_normal_percentiles)
  numeric_range_checks(normal_percentiles)
  sort(normal_percentiles)
  
  
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
  
  
  # Create origin date to apply to flow_data and Q_daily later on
  origin_date <- get_origin_date(water_year_start)
  
  
  ## CALC STATS
  ## ----------
  
  flow_data_year <- add_date_variables(flow_data, water_year_start = water_year_start)
  flow_data_year <- dplyr::filter(flow_data_year, WaterYear ==  year_to_plot)
  flow_data_year <- dplyr::select(flow_data_year, STATION_NUMBER, Flow_Date = Date, DayofYear, Value)
  # 
  daily_stats <- calc_daily_stats(data = flow_data,
                                  percentiles = normal_percentiles,
                                  roll_days = roll_days,
                                  roll_align = roll_align,
                                  water_year_start = water_year_start,
                                  start_year = start_year,
                                  end_year = end_year,
                                  exclude_years = exclude_years, 
                                  complete_years = TRUE,
                                  months = months)
  names(daily_stats)[names(daily_stats) == paste0("P",min(normal_percentiles))] <- "MIN"
  names(daily_stats)[names(daily_stats) == paste0("P",max(normal_percentiles))] <- "MAX"
  daily_stats <- dplyr::mutate(daily_stats, Date = as.Date(DayofYear, origin = origin_date))
  daily_stats <- dplyr::mutate(daily_stats, AnalysisDate = Date)
  daily_stats
  daily_stats <- dplyr::left_join(daily_stats, flow_data_year, by = c("STATION_NUMBER", "DayofYear"))
  
  flow_timing <- calc_annual_flow_timing(data = flow_data,
                                         water_year_start = water_year_start,
                                         start_year = start_year,
                                         end_year = end_year,
                                         exclude_years = exclude_years, 
                                         months = months,
                                         percent_total = percent_total)
  flow_timing <- dplyr::filter(flow_timing, Year == year_to_plot)
  flow_timing <- dplyr::select(flow_timing, STATION_NUMBER, Year, dplyr::contains("DoY"))
  flow_timing <- tidyr::pivot_longer(flow_timing,
                                     cols = -1, values_to = "DayofYear", names_to = "Percent")
  flow_timing <- dplyr::left_join(daily_stats,
                                  flow_timing, by = c("STATION_NUMBER", "DayofYear"))
  flow_timing <- dplyr::mutate(flow_timing, 
                               Value2 = ifelse(is.na(Percent), NA, Value),
                               Percent = paste0(substr(Percent, 5, nchar(Percent)-10),"%"),# Total Flow"),
                               Percent = ifelse(is.na(Value2), NA, Percent),)
  
  ## PLOT STATS
  ## ----------
  
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)",
                                "Discharge (cms)"))
  
  # Create the daily stats plots
  timing_plots <- dplyr::group_by(flow_timing, STATION_NUMBER)
  timing_plots <- tidyr::nest(timing_plots)
  timing_plots <- dplyr::mutate(
    timing_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date)) +
        {if(plot_normal_percentiles) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "MIN", ymax = "MAX"),
                                                          alpha = 0.4, colour = "lightblue2", fill = "lightblue2", na.rm = FALSE) } +
        ggplot2::geom_line(ggplot2::aes(y = Value), size = 0.2, colour = "#264b96") +
        {if (plot_vlines) ggplot2::geom_vline(data = dplyr::filter(., !is.na(Percent)),
                                              ggplot2::aes(xintercept = Date, colour=Percent), linetype = 5)}+
        ggplot2::geom_point(data = dplyr::filter(., !is.na(Percent)),
                            ggplot2::aes(y = Value2, fill = Percent), size = 3.5, na.rm = TRUE, shape = 21) +
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                                                        breaks = scales::pretty_breaks(n = 8),
                                                        labels = scales::label_number(scale_cut = scales::cut_short_scale()))}+
        {if(log_discharge) ggplot2::scale_y_log10(breaks = scales::log_breaks(n = 8, base = 10),
                                                  labels = scales::label_number(scale_cut = scales::cut_short_scale()))} +
        {if(log_discharge & log_ticks) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                                    short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                                    long = ggplot2::unit(.2, "cm"))} +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month",
                              limits = as.Date(c(as.character(min(daily_stats$AnalysisDate, na.rm = TRUE)),
                                                 as.character(max(daily_stats$AnalysisDate, na.rm = TRUE)))),
                              expand = c(0,0)) +
        ggplot2::scale_fill_viridis_d()+
        ggplot2::scale_colour_viridis_d()+
        ggplot2::guides(colour = "none")+
        ggplot2::labs(fill = paste0("Percent of Annual\nFlow Date for\n",
                                    ifelse(water_year_start == 1,"Year ","Water Year "),
                                    year_to_plot)) +
        ggplot2::xlab("Day of Year") +
        ggplot2::ylab(y_axis_title) +
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                       axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                       axis.ticks = ggplot2::element_line(size = .1, colour = "grey25"),
                       axis.ticks.length = ggplot2::unit(0.05, "cm"),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,0,0,0)),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(size = .1),
                       legend.text = ggplot2::element_text(size = 9, colour = "grey25"),
                       legend.key.size = ggplot2::unit(0.4, "cm"),
                       legend.spacing = ggplot2::unit(-0.4, "cm"),
                       legend.background = ggplot2::element_blank())
    ))
  
  
  
  # Create a list of named plots extracted from the tibble
  plots <- timing_plots$plot
  if (nrow(timing_plots) == 1) {
    names(plots) <- "Annual_Flow_Timing_Year"
  } else {
    names(plots) <- paste0(timing_plots$STATION_NUMBER, "_Annual_Flow_Timing_Year")
  }
  
  plots
  
}
