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

#' @title Plot long-term summary statistics from daily mean flows
#'
#' @description Plots the long-term mean, median, maximum, minimum, and percentiles of daily flow values for over all months and 
#'    all data (Long-term) from a daily streamflow data set. Calculates statistics from all values, unless specified. 
#'    The Maximum-Minimum band can be removed using the \code{plot_extremes} argument and the percentile bands can be
#'    customized using the \code{inner_percentiles} and \code{outer_percentiles} arguments. Data calculated using the 
#'    \code{calc_longterm_daily_stats()} function. Returns a list of plots.
#'
#' @inheritParams calc_longterm_daily_stats
#' @inheritParams plot_annual_stats
#' @inheritParams plot_daily_stats
#' @param add_year Numeric value indicating a year of daily flows to add to the daily statistics plot. Leave blank
#'    or set to \code{NULL} for no years.
#' @param plot_extremes Logical value to indicate plotting a ribbon with the range of daily minimum and maximum flows. 
#'    Default \code{TRUE}.
#' @param inner_percentiles Numeric vector of two percentile values indicating the lower and upper limits of the 
#'    inner percentiles ribbon for plotting. Default \code{c(25,75)}, set to \code{NULL} for no inner ribbon.
#' @param outer_percentiles Numeric vector of two percentile values indicating the lower and upper limits of the 
#'    outer percentiles ribbon for plotting. Default \code{c(5,95)}, set to \code{NULL} for no outer ribbon.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Long-term_Monthly_Statistics}{a plot that contains long-term flow statistics}
#'   Default plots on each object:  
#'   \item{Monthly Mean}{mean of all annual monthly means for a given month over all years}
#'   \item{Monthly Median}{median of all annual monthly means for a given month over all years}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the monthly 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the monthly minimum and maximums}
#'   
#' @seealso \code{\link{calc_longterm_daily_stats}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot longterm daily statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_longterm_daily_stats(data = flow_data,
#'                           start_year = 1980)
#'                   
#' # Plot longterm daily statistics for water years starting in October
#' plot_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980,
#'                           end_year = 2010,
#'                           water_year_start = 10)
#'                           
#' }
#' @export


plot_longterm_daily_stats <- function(data,
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
                                      complete_years = FALSE,
                                      ignore_missing = FALSE,
                                      plot_extremes = TRUE,
                                      plot_inner_percentiles = TRUE,
                                      plot_outer_percentiles = TRUE,
                                      inner_percentiles = c(25,75),
                                      outer_percentiles = c(5,95),
                                      add_year,
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
  if (missing(start_year)) {
    start_year <- 0
  }
  if (missing(end_year)) {
    end_year <- 9999
  }
  if (missing(exclude_years)) {
    exclude_years <- NULL
  }
  if (missing(add_year)) {
    add_year <- NULL
  }
  
  logical_arg_check(log_discharge)
  log_ticks_checks(log_ticks, log_discharge)
  logical_arg_check(include_title)  
  ptile_ribbons_checks(inner_percentiles, outer_percentiles)
  logical_arg_check(plot_extremes)
  logical_arg_check(plot_inner_percentiles)
  logical_arg_check(plot_outer_percentiles)
  
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
  longterm_stats_all <- suppressWarnings(
    calc_longterm_daily_stats(data = flow_data,
                              water_year_start = water_year_start,
                              start_year = start_year,
                              end_year = end_year))
  longterm_stats_all <- longterm_stats_all[,1:2]
  
  longterm_stats <- calc_longterm_daily_stats(data = flow_data,
                                              percentiles = c(inner_percentiles, outer_percentiles),
                                              roll_days = roll_days,
                                              roll_align = roll_align,
                                              water_year_start = water_year_start,
                                              start_year = start_year,
                                              end_year = end_year,
                                              exclude_years = exclude_years,
                                              complete_years = complete_years,
                                              ignore_missing = ignore_missing,
                                              months = months)
  longterm_stats <- dplyr::left_join(longterm_stats_all, longterm_stats, 
                                     by = c("STATION_NUMBER", "Month"))
  
  ## PLOT STATS
  ## ----------
  
  # Make longterm mean and median their own columns
  longterm_stats_months <- dplyr::filter(longterm_stats, Month != "Long-term")
  
  # remove NA's from start and end for plotting
  longterm_stats_months <- longterm_stats_months[cumsum(stats::complete.cases(longterm_stats_months)) != 0, ]
  longterm_stats_months <- dplyr::arrange(longterm_stats_months, dplyr::desc(Month))
  longterm_stats_months <- longterm_stats_months[cumsum(stats::complete.cases(longterm_stats_months)) != 0, ]
  longterm_stats_months <- dplyr::arrange(longterm_stats_months, Month)
  
  longterm_stats_longterm <- dplyr::filter(longterm_stats, Month == "Long-term")
  longterm_stats_longterm <- dplyr::select(longterm_stats_longterm, STATION_NUMBER, "LT_Mean" = Mean, "LT_Med" = Median)
  longterm_stats <- dplyr::left_join(longterm_stats_months, longterm_stats_longterm, by = "STATION_NUMBER")
  
  ## ADD YEAR IF SELECTED
  ## --------------------
  
  if(!is.null(add_year)){
    
    # data for testing if year is in flow_data
    flow_data_year <- add_date_variables(data = flow_data, water_year_start = water_year_start)
    flow_data_year <- dplyr::filter(flow_data_year, WaterYear %in% start_year:end_year)
    
    # if year is in data and not excluded, calculate those values
    if (add_year %in% min(flow_data_year$WaterYear):max(flow_data_year$WaterYear) & !(add_year %in% exclude_years)) {
      
      year_data <- suppressWarnings(calc_monthly_stats(data = flow_data,
                                                       roll_days = roll_days,
                                                       roll_align = roll_align,
                                                       water_year_start = water_year_start,
                                                       start_year = start_year,
                                                       end_year = end_year,
                                                       exclude_years = exclude_years,
                                                       ignore_missing = ignore_missing))
      year_data <- dplyr::filter(year_data, Year == add_year)
      year_data <- dplyr::mutate(year_data, Month = factor(Month, levels = c(month.abb, "Long-term")))
      year_data <- dplyr::select(year_data, STATION_NUMBER, Month, Year_mean = Mean)
      
      # Warning if all daily values are NA from the add_year
      for (stn in unique(year_data$STATION_NUMBER)) {
        year_test <- dplyr::filter(year_data, STATION_NUMBER == stn)
        
        if(all(is.na(year_test$Year_mean))) {
          warning("Monthly data does not exist for the year listed in add_year and was not plotted.", call. = FALSE)
          add_year <- NULL
        }
      }
      
      
      if(!all(is.na(year_data$Year_mean))) {
        longterm_stats <- dplyr::left_join(longterm_stats, year_data, by = c("STATION_NUMBER", "Month"))
      }
    } else {
      warning("Monthly data does not exist for the year listed in add_year and was not plotted.", call. = FALSE)
      add_year <- NULL
    }
  }
  
  
  if (all(sapply(longterm_stats[3:ncol(longterm_stats)], function(x)all(is.na(x))))) {
    longterm_stats[is.na(longterm_stats)] <- 1
  }
  
  # Create manual colour and fill options
  
  fill_manual_list <- c()
  if (plot_extremes) {
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
  if (is.numeric(add_year)) {
    colour_manual_list <- c(colour_manual_list, "yr.colour" = "red")
    colour_manual_labels <- c(colour_manual_labels, paste0(add_year, " Mean"))
  }
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)", #expression(Volume~(m^3))
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)",
                                "Discharge (cms)")) #expression(Discharge~(m^3/s))
  
  # Plot
  lt_plots <- dplyr::group_by(longterm_stats, STATION_NUMBER)
  lt_plots <- tidyr::nest(lt_plots)
  lt_plots <- dplyr::mutate(
    lt_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Month, group = 1)) +
        {if(plot_extremes) ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = FALSE)} +
        {if(is.numeric(outer_percentiles) & plot_outer_percentiles) 
          ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(outer_percentiles)),
                                                   ymax = paste0("P",max(outer_percentiles)),
                                                   fill = paste0("'",outer_name,"'")), na.rm = FALSE)} +
        {if(is.numeric(inner_percentiles) & plot_inner_percentiles) 
          ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(inner_percentiles)),
                                                   ymax = paste0("P",max(inner_percentiles)),
                                                   fill = paste0("'",inner_name,"'")), na.rm = FALSE)} +
        ggplot2::geom_line(ggplot2::aes(y = Mean, color = "Mean"), size = .9, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Median, color = "Median"), size = .9, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(y = Mean), size = 2, na.rm = TRUE, colour  = "paleturquoise") +
        ggplot2::geom_point(ggplot2::aes(y = Median), size = 2, na.rm = TRUE, colour = "dodgerblue4") +
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 8),
                                                        labels = scales::label_number(scale_cut = scales::cut_short_scale()))}+
        {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10),
                                                  labels = scales::label_number(scale_cut = scales::cut_short_scale()))} +
        {if(log_discharge & log_ticks) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(0.07, "cm"),
                                                                    mid = ggplot2::unit(0.15, "cm"), long = ggplot2::unit(0.2, "cm"))} +
        ggplot2::scale_x_discrete(expand = c(0.01,0.01)) +
        ggplot2::ylab(y_axis_title) +
        ggplot2::xlab(NULL) +
        ggplot2::theme_bw()+
        ggplot2::labs(colour = 'Daily Statistics') +
        {if (include_title & unique(.y) != "XXXXXXX") ggplot2::labs(colour = paste0(.y,'\n \nDaily Statistics')) } +
        ggplot2::theme(legend.position = "right",
                       legend.justification = "right",
                       legend.text = ggplot2::element_text(size = 9),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       legend.spacing = ggplot2::unit(-0.4, "cm"),
                       legend.background = ggplot2::element_blank()) +
        ggplot2::scale_fill_manual(values = fill_manual_list) +
        ggplot2::scale_color_manual(values = colour_manual_list, labels = colour_manual_labels) +
        {if (is.numeric(add_year)) ggplot2::geom_line(ggplot2::aes(x= Month, y = Year_mean, colour = "yr.colour"), size = 0.9, na.rm = TRUE) } +
        {if (is.numeric(add_year)) ggplot2::geom_point(ggplot2::aes(y = Year_mean), size = 2, na.rm = TRUE, colour = "red") } +
        ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2, title = NULL))
    ))
  
  
  # Create a list of named plots extracted from the tibble
  plots <- lt_plots$plot
  if (nrow(lt_plots) == 1) {
    names(plots) <- "Long-term_Daily_Statistics"
  } else {
    names(plots) <- paste0(lt_plots$STATION_NUMBER, "_Long-term_Daily_Statistics")
  }
  
  plots
  
} 
