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

#' @title Plot days above normal, below normal and normal for a specific year
#'
#' @description Plots an annual hydrograph for a specific year with daily flow values coloured by whether the daily values are normal,
#'    above normal, or below normal, overlaying the normals range. The normal range is typically between 25 and 75th percentiles for
#'    each day of the year. Upper and lower-range percentiles are calculated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. Normals calculated from only years of complete data, although incomplete years can be 
#'    plotted. Shows the annual values for a specific year from the counts from the \code{plot_annual_outside_normal()} function. 
#'    Returns a list of plots.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams plot_annual_stats
#' @inheritParams calc_annual_outside_normal
#' @param year_to_plot Numeric value indicating the year/water year to plot flow data with normal category colours. Default \code{NA}.
#' @param plot_flow_line Logical value indicating whether to connect flow data coloured points with lines. Default \code{TRUE}.
#' @param plot_normal_percentiles Logical value indicating whether to plot the normal percentiles ribbon. Default \code{TRUE}.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Normal_Days_Year}{a plot that contains the above, below, and normal colour daily flow points}
#'   
#' @seealso \code{\link{calc_annual_outside_normal}}
#' @seealso \code{\link{plot_annual_outside_normal}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot the year 2000 using a data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_annual_outside_normal_year(data = flow_data,
#'                                 year_to_plot = 2000)
#'                   
#' # Plot the year 2000 using the station_number argument
#' plot_annual_outside_normal_year(station_number = "08NM116",
#'                                 year_to_plot = 2000)
#'  
#' # Plot the year 2000 and change the normal percentiles range          
#' plot_annual_outside_normal_year(station_number = "08NM116",
#'                                 normal_percentiles = c(20,80),
#'                                 year_to_plot = 2000)
#'                  
#' }
#' @export


plot_annual_outside_normal_year <- function(data,
                                            dates = Date,
                                            values = Value,
                                            groups = STATION_NUMBER,
                                            station_number,
                                            normal_percentiles = c(25,75),
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
                                            plot_flow_line = TRUE,
                                            plot_normal_percentiles = TRUE){
  
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
  logical_arg_check(plot_flow_line)
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
  
  
  origin_date <- get_origin_date(water_year_start)
  
  
  ## CALC STATS
  ## ----------
  
  flow_data_year <- add_date_variables(flow_data, water_year_start = water_year_start)
  flow_data_year <- dplyr::filter(flow_data_year, WaterYear ==  year_to_plot)
  flow_data_year <- dplyr::select(flow_data_year, STATION_NUMBER, Flow_Date = Date, DayofYear, Value)
  
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
  daily_stats <- dplyr::left_join(daily_stats, flow_data_year, by = c("STATION_NUMBER", "DayofYear"))
  daily_stats <- dplyr::mutate(daily_stats, 
                               Normal = dplyr::case_when(Value < MIN ~ "Below Normal",
                                                         Value > MAX ~ "Above Normal",
                                                         TRUE ~ "Normal"),
                               Normal = factor(Normal, levels = c("Above Normal","Normal","Below Normal")))
  
  if (all(sapply(daily_stats[4:ncol(daily_stats)], function(x)all(is.na(x))))) {
    daily_stats[is.na(daily_stats)] <- 1
  }
  ## PLOT STATS
  ## ----------
  
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)", #expression(Volume~(m^3))
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)",
                                "Discharge (cms)")) #expression(Discharge~(m^3/s))
  
  daily_stats <- fill_missing_dates(daily_stats, water_year_start = water_year_start)
  daily_stats <- add_date_variables(daily_stats, water_year_start = water_year_start)
  daily_stats <- dplyr::select(daily_stats, -c("CalendarYear", "Month", "MonthName", "WaterYear"))
  
  colour_list <- c("Above Normal" = "#264b96",
                   "Normal" = "#27b376",
                   "Below Normal" = "#bf212f") #f9a73e
  
  
  # Create the daily stats plots
  daily_plots <- dplyr::group_by(daily_stats, STATION_NUMBER)
  daily_plots <- tidyr::nest(daily_plots)
  daily_plots <- dplyr::mutate(
    daily_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date)) +
        {if(plot_normal_percentiles) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "MIN", ymax = "MAX"),
                                                          alpha = 0.3, colour = "lightblue2", fill = "lightblue2", na.rm = FALSE) } +
        {if(plot_flow_line) ggplot2::geom_line(ggplot2::aes(y = Value), , size = 0.1, colour = "#264b96") }+ 
        ggplot2::geom_point(ggplot2::aes(y = Value, colour = Normal), size = 3) + 
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
        ggplot2::scale_color_manual(values = colour_list, name = paste0("Normal Category\nfor ",
                                                                        ifelse(water_year_start == 1,"Year ","Water Year "),
                                                                        year_to_plot )) +
        ggplot2::xlab("Day of Year") +
        ggplot2::ylab(y_axis_title) +
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
        ggplot2::theme_bw() +
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
  plots <- daily_plots$plot
  if (nrow(daily_plots) == 1) {
    names(plots) <- "Annual_Normal_Days_Year"
  } else {
    names(plots) <- paste0(daily_plots$STATION_NUMBER, "_Annual_Normal_Days_Year")
  }
  
  plots
  
}
