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

#' @title Plot annual high and low flows for a specific year
#'
#' @description Plots an annual hydrograph for a specific year with the values and timing of annual n-day low and high flows.
#'    The 'normal' range of percentiles also plotted for reference and are calculated from only years of complete data. 
#'    Shows the values and dates of max/mins for a specific year from the \code{calc_annual_extremes()} and
#'    \code{plot_annual_extremes()} functions. Can remove either low or high flows using \code{plot_lowflow = FALSE()} or 
#'    \code{plot_highflow = FALSE()}, respectively. Returns a list of plots.
#'
#' @inheritParams calc_annual_extremes
#' @inheritParams plot_annual_stats
#' @inheritParams plot_annual_normal_days_year
#' @param plot_lowflow Logical value indicating whether to plot annual low flows. Default \code{TRUE}.
#' @param plot_highflow Logical value indicating whether to plot annual high flows. Default \code{TRUE}.
#' @param months Numeric vector of specific months to plot. For example, \code{3} for March, \code{6:8} for Jun-Aug.
#'    Will be overridden for low or high flow statistics if \code{months_low} or \code{months_high} set, but will still 
#'    define the date limits on the x-axis. Default plots all months (\code{1:12}).
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Extremes_Year}{a plot that contains the an annual hydrograph and identified low and high flow periods}
#'   
#' @seealso \code{\link{calc_annual_extremes}}
#' @seealso \code{\link{plot_annual_extremes}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot the year 2000 with the annual maximum and minimums       
#' plot_annual_extremes_year(station_number = "08NM116",
#'                           roll_days_high = 3,
#'                           roll_days_low = 7,
#'                           year_to_plot = 2001)
#'                  
#' }
#' @export


plot_annual_extremes_year <- function(data,
                                      dates = Date,
                                      values = Value,
                                      groups = STATION_NUMBER,
                                      station_number,
                                      year_to_plot = NA,
                                      roll_days = 1,
                                      roll_days_low = NA,
                                      roll_days_high = NA,
                                      roll_align = "right",
                                      water_year_start = 1,
                                      start_year,
                                      end_year,
                                      exclude_years,
                                      months = 1:12,
                                      months_low = NA,
                                      months_high = NA,
                                      log_discharge = TRUE,
                                      log_ticks = FALSE,
                                      include_title = FALSE,
                                      plot_normal_percentiles = TRUE,
                                      normal_percentiles = c(25,75),
                                      plot_lowflow = TRUE,
                                      plot_highflow = TRUE,
                                      complete_years = FALSE,
                                      ignore_missing = FALSE,
                                      allowed_missing = ifelse(ignore_missing,100,0)){
  
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
  if (is.na(roll_days_low)) {
    roll_days_low <- roll_days
  }
  if (is.na(roll_days_high)) {
    roll_days_high <- roll_days
  }
  
  logical_arg_check(log_discharge) 
  log_ticks_checks(log_ticks, log_discharge)
  logical_arg_check(include_title)
  logical_arg_check(plot_normal_percentiles)
  numeric_range_checks(normal_percentiles)
  sort(normal_percentiles)
  logical_arg_check(plot_lowflow)
  logical_arg_check(plot_highflow)
  if (!plot_lowflow & !plot_highflow) stop("Both plot_lowflow and plot_highflow cannot be FALSE.", call. = FALSE)
  
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
  daily_stats <- suppressMessages(
    calc_daily_stats(data = flow_data,
                     percentiles = normal_percentiles,
                     water_year_start = water_year_start,
                     start_year = start_year,
                     end_year = end_year,
                     exclude_years = exclude_years, 
                     complete_years = TRUE,
                     months = months))
  names(daily_stats)[names(daily_stats) == paste0("P",min(normal_percentiles))] <- "MIN"
  names(daily_stats)[names(daily_stats) == paste0("P",max(normal_percentiles))] <- "MAX"
  daily_stats <- dplyr::mutate(daily_stats, Date = as.Date(DayofYear, origin = origin_date))
  daily_stats <- dplyr::mutate(daily_stats, AnalysisDate = Date)
  daily_stats
  daily_stats <- dplyr::left_join(daily_stats, flow_data_year, by = c("STATION_NUMBER", "DayofYear"))
  
  ann_peaks <- calc_annual_extremes(data = flow_data,
                                    water_year_start = water_year_start,
                                    start_year = start_year,
                                    end_year = end_year,
                                    exclude_years = exclude_years, 
                                    months = months,
                                    months_low = months_low,
                                    months_high = months_high,
                                    roll_days = roll_days,
                                    roll_days_low = roll_days_low,
                                    roll_days_high = roll_days_high,
                                    roll_align = roll_align,
                                    complete_years = complete_years,
                                    ignore_missing = ignore_missing,
                                    allowed_missing = allowed_missing)
  names(ann_peaks) <- c("STATION_NUMBER", "Year", "Min_Value", "Min_Doy", 
                        "Min_Date", "Max_Value", "Max_Doy", "Max_Date")
  ann_peaks_min <- dplyr::select(ann_peaks, STATION_NUMBER, Year, DayofYear = Min_Doy, dplyr::contains("Min"))
  ann_peaks_min <- dplyr::mutate(ann_peaks_min,
                                 Min_Start = dplyr::case_when(roll_align == "left" ~ DayofYear,
                                                              roll_align == "right" ~ DayofYear - roll_days_low + 1,
                                                              roll_align == "center" & roll_days_low %% 2 == 0 ~ DayofYear - (roll_days_low/2) + 1,
                                                              roll_align == "center" & roll_days_low %% 2 != 0 ~ DayofYear - ((roll_days_low - 1)/2)),
                                 Min_Start = ifelse(Min_Start < min(daily_stats$DayofYear), min(daily_stats$DayofYear), Min_Start),
                                 Min_Start = as.Date(Min_Start, origin = origin_date),
                                 Min_End = dplyr::case_when(roll_align == "left" ~ DayofYear + roll_days_low - 1,
                                                            roll_align == "right" ~ DayofYear,
                                                            roll_align == "center" & roll_days_low %% 2 == 0 ~ DayofYear + (roll_days_low/2),
                                                            roll_align == "center" & roll_days_low %% 2 != 0 ~ DayofYear + ((roll_days_low - 1)/2)),
                                 Min_End = ifelse(Min_End > max(daily_stats$DayofYear), max(daily_stats$DayofYear), Min_End),
                                 Min_End = as.Date(Min_End, origin = origin_date))
  ann_peaks_min <- dplyr::filter(ann_peaks_min, Year == year_to_plot)
  ann_peaks_min <- dplyr::select(ann_peaks_min, -Year)
  ann_peaks_max <- dplyr::select(ann_peaks, STATION_NUMBER, Year, DayofYear = Max_Doy, dplyr::contains("Max"))
  ann_peaks_max <- dplyr::mutate(ann_peaks_max,
                                 Max_Start = dplyr::case_when(roll_align == "left" ~ DayofYear,
                                                              roll_align == "right" ~ DayofYear - roll_days_high + 1,
                                                              roll_align == "center" & roll_days_high %% 2 == 0 ~ DayofYear - (roll_days_high/2) + 1,
                                                              roll_align == "center" & roll_days_high %% 2 != 0 ~ DayofYear - ((roll_days_high - 1)/2)),
                                 Max_Start = ifelse(Max_Start < min(daily_stats$DayofYear), min(daily_stats$DayofYear), Max_Start),
                                 Max_Start = as.Date(Max_Start, origin = origin_date),
                                 Max_End = dplyr::case_when(roll_align == "left" ~ DayofYear + roll_days_high - 1,
                                                            roll_align == "right" ~ DayofYear,
                                                            roll_align == "center" & roll_days_high %% 2 == 0 ~ DayofYear + (roll_days_high/2),
                                                            roll_align == "center" & roll_days_high %% 2 != 0 ~ DayofYear + ((roll_days_high - 1)/2)),
                                 Max_End = ifelse(Max_End > max(daily_stats$DayofYear), max(daily_stats$DayofYear), Max_End),
                                 Max_End = as.Date(Max_End, origin = origin_date))
  ann_peaks_max <- dplyr::filter(ann_peaks_max, Year == year_to_plot)
  ann_peaks_max <- dplyr::select(ann_peaks_max, -Year)
  
  
  ann_peaks <- dplyr::left_join(daily_stats, ann_peaks_min, by = c("STATION_NUMBER", "DayofYear"))
  ann_peaks <- dplyr::left_join(ann_peaks, ann_peaks_max, by = c("STATION_NUMBER", "DayofYear"))
  
  
  ## PLOT STATS
  ## ----------
  
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)",
                                "Discharge (cms)"))
  
  high_col <- "dodgerblue2" #"#440154FF" #
  low_col <- "orange" #"#FDE725FF" #
  high_lab <- paste0(roll_days_high,"-day Maximum") #"#440154FF" #
  low_lab <- paste0(roll_days_low,"-day Minimum") #"#440154FF" #
  if (plot_lowflow & plot_highflow) {
    fils <- c(high_col,low_col)
    shp <- c(21, 21)
    colors <- c("black", "black")
    names(fils) <- c(high_lab, low_lab)
  } else if (!plot_lowflow & plot_highflow) {
    fils <- c(high_col)
    shp <- c(21)
    colors <- c("black")
    names(fils) <- c(high_lab)
  } else if (plot_lowflow & !plot_highflow) {
    fils <- c(low_col)
    shp <- c(21)
    colors <- c("black")
    names(fils) <- c(low_lab)
  }
  if (plot_normal_percentiles) {
    names <- c(names(fils),paste0("Historic Daily\nP",normal_percentiles[1],"-P",normal_percentiles[2]))
    fils <- c(fils, "lightblue2")
    shp <- c(shp, 22)
    colors <- c(colors, "lightblue2")
    names(fils) <- names
  }
  disch_name <- paste0("Daily Discharge")
  
  # Create the daily stats plots
  timing_plots <- dplyr::group_by(ann_peaks, STATION_NUMBER)
  timing_plots <- tidyr::nest(timing_plots)
  timing_plots <- dplyr::mutate(
    timing_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date)) +
        {if(plot_normal_percentiles) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "MIN", ymax = "MAX"),
                                                          alpha = 0.4, colour = "lightblue2", fill = "lightblue2", na.rm = FALSE) } +
        ggplot2::geom_line(ggplot2::aes(y = Value, colour = disch_name), size = 0.2, na.rm = TRUE) +
        {if(plot_lowflow & roll_days_low > 1) ggplot2::geom_rect(ggplot2::aes(xmin = Min_Start, xmax = Min_End, ymax =Inf, ymin=0), 
                                                                 fill = low_col, alpha = 0.2, na.rm = TRUE) }+
        {if(plot_highflow & roll_days_high > 1) ggplot2::geom_rect(ggplot2::aes(xmin = Max_Start, xmax = Max_End, ymax =Inf, ymin=0), 
                                                                   fill = high_col, alpha = 0.2, na.rm = TRUE) }+
        {if(plot_lowflow & roll_days_low > 1) ggplot2::geom_segment(ggplot2::aes(x = Min_Start, xend = Min_End, y = Min_Value, yend=Min_Value), 
                                                                    colour = low_col, size = 1, na.rm = TRUE)}+
        {if(plot_highflow & roll_days_high > 1) ggplot2::geom_segment(ggplot2::aes(x = Max_Start, xend = Max_End, y = Max_Value, yend=Max_Value), 
                                                                      colour = high_col, size = 1, na.rm = TRUE)}+
        {if(plot_lowflow) ggplot2::geom_vline(data = dplyr::filter(., !is.na(Min_Value)), ggplot2::aes(xintercept = Min_Start), colour = low_col, size = 1)}+
        {if(plot_lowflow & roll_days_low > 1) ggplot2::geom_vline(data = dplyr::filter(., !is.na(Min_Value)), ggplot2::aes(xintercept = Min_End), colour = low_col, size = 1)}+
        {if(plot_highflow) ggplot2::geom_vline(data = dplyr::filter(., !is.na(Max_Value)), ggplot2::aes(xintercept = Max_Start), colour = high_col, size = 1)}+
        {if(plot_highflow & roll_days_high > 1) ggplot2::geom_vline(data = dplyr::filter(., !is.na(Max_Value)) ,ggplot2::aes(xintercept = Max_End), colour = high_col, size = 1)}+
        {if(plot_lowflow) ggplot2::geom_point(ggplot2::aes(x= Date, y = Min_Value, fill = low_lab), size = 3.5, na.rm = TRUE, shape = 21) }+
        {if(plot_highflow) ggplot2::geom_point(ggplot2::aes(x= Date, y = Max_Value, fill = high_lab), size = 3.5, na.rm = TRUE, shape = 21) }+
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
        ggplot2::scale_fill_manual(values = fils, name = paste0("Annual Extremes\nfor ",
                                                                ifelse(water_year_start == 1,"Year ","Water Year "),
                                                                year_to_plot ))+
        ggplot2::scale_colour_manual(values = stats::setNames("#264b96", disch_name), name = NULL)+
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = shp, colour = colors),
                                                     order = 1) )+
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
    names(plots) <- "Annual_Extremes_Year"
  } else {
    names(plots) <- paste0(timing_plots$STATION_NUMBER, "_Annual_Extremes_Year")
  }
  
  plots
  
}
