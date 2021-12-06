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

#' @title Plot daily summary statistics
#'
#' @description Plots means, medians, maximums, minimums, and percentiles for each day of the year of flow values 
#'    from a daily streamflow data set. Can determine statistics of rolling mean days (e.g. 7-day flows) using the \code{roll_days} 
#'    argument. Calculates statistics from all values, unless specified. The Maximum-Minimum band can be removed using the 
#'    \code{include_extremes} argument and the percentile bands can be customized using the \code{inner_percentiles} and 
#'    \code{outer_percentiles} arguments. Data calculated using \code{calc_daily_stats()} function. Returns a list of plots.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams plot_annual_stats
#' @param add_year Numeric value indicating a year of daily flows to add to the daily statistics plot.  Leave blank
#'    or set to \code{NULL} for no years.
#' @param include_extremes Logical value to indicate plotting a ribbon with the range of daily minimum and maximum flows. 
#'    Default \code{TRUE}.
#' @param inner_percentiles Numeric vector of two percentile values indicating the lower and upper limits of the 
#'    inner percentiles ribbon for plotting. Default \code{c(25,75)}, set to \code{NULL} for no inner ribbon.
#' @param outer_percentiles Numeric vector of two percentile values indicating the lower and upper limits of the 
#'    outer percentiles ribbon for plotting. Default \code{c(5,95)}, set to \code{NULL} for no outer ribbon.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Daily_Stats}{a plot that contains daily flow statistics}
#'   Default plots on each object:  
#'   \item{Mean}{daily mean}
#'   \item{Median}{daily median}
#'   \item{25-75 Percentiles}{a ribbon showing the range of data between the daily 25th and 75th percentiles}
#'   \item{5-95 Percentiles}{a ribbon showing the range of data between the daily 5th and 95th percentiles}
#'   \item{Minimum-Maximum}{a ribbon showing the range of data between the daily minimum and maximums}
#'   \item{'Year'}{(on annual plots) the daily flows for the designated year}
#'   
#' @seealso \code{\link{calc_daily_stats}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot daily statistics using a data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_daily_stats(data = flow_data,
#'                  start_year = 1980)
#'                   
#' # Plot daily statistics using only years with no missing data
#' plot_daily_stats(station_number = "08NM116",
#'                  complete_years = TRUE)
#'  
#' # Plot daily statistics and add a specific year's daily flows                
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1980,
#'                  add_year = 1985)                
#'                   
#' # Plot daily statistics for 7-day flows for July-September months only
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1980,
#'                  roll_days = 7,
#'                  months = 7:9)
#'                  
#' }
#' @export


plot_daily_stats <- function(data,
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
                             months = 1:12,
                             ignore_missing = FALSE,
                             include_extremes = TRUE,
                             inner_percentiles = c(25,75),
                             outer_percentiles = c(5,95),
                             add_year,
                             log_discharge = TRUE,
                             include_title = FALSE){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(add_year)) {
    add_year <- NULL
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
  add_year_checks(add_year)
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
  
  
  # Create origin date to apply to flow_data and Q_daily later on
  if (water_year_start == 1)         {origin_date <- as.Date("1899-12-31")
  } else if (water_year_start == 2)  {origin_date <- as.Date("1899-01-31")
  } else if (water_year_start == 3)  {origin_date <- as.Date("1899-02-28")
  } else if (water_year_start == 4)  {origin_date <- as.Date("1899-03-31")
  } else if (water_year_start == 5)  {origin_date <- as.Date("1899-04-30")
  } else if (water_year_start == 6)  {origin_date <- as.Date("1899-05-31")
  } else if (water_year_start == 7)  {origin_date <- as.Date("1899-06-30")
  } else if (water_year_start == 8)  {origin_date <- as.Date("1899-07-31")
  } else if (water_year_start == 9)  {origin_date <- as.Date("1899-08-31")
  } else if (water_year_start == 10) {origin_date <- as.Date("1899-09-30")
  } else if (water_year_start == 11) {origin_date <- as.Date("1899-10-31")
  } else if (water_year_start == 12) {origin_date <- as.Date("1899-11-30")
  }
  
  
  
  ## CALC STATS
  ## ----------
  
  daily_stats <- calc_daily_stats(data = flow_data,
                                  percentiles = c(inner_percentiles, outer_percentiles),
                                  roll_days = roll_days,
                                  roll_align = roll_align,
                                  water_year_start = water_year_start,
                                  start_year = start_year,
                                  end_year = end_year,
                                  exclude_years = exclude_years, 
                                  complete_years = complete_years,
                                  months = months,
                                  ignore_missing = ignore_missing)
  
  daily_stats <- dplyr::mutate(daily_stats, Date = as.Date(DayofYear, origin = origin_date))
  daily_stats <- dplyr::mutate(daily_stats, AnalysisDate = Date)
  
  if (all(sapply(daily_stats[4:ncol(daily_stats)], function(x)all(is.na(x))))) {
    daily_stats[is.na(daily_stats)] <- 1
  }
  
  ## ADD YEAR IF SELECTED
  ## --------------------
  
  if(!is.null(add_year)){
    
    year_data <- fill_missing_dates(data = flow_data, water_year_start = water_year_start)
    year_data <- add_date_variables(data = year_data, water_year_start = water_year_start)
    year_data <- add_rolling_means(data = year_data, roll_days = roll_days, roll_align = roll_align)
    colnames(year_data)[ncol(year_data)] <- "RollingValue"
    
    year_data <- dplyr::mutate(year_data, AnalysisDate = as.Date(DayofYear, origin = origin_date))
    year_data <- dplyr::filter(year_data, WaterYear >= start_year & WaterYear <= end_year)
    year_data <- dplyr::filter(year_data, !(WaterYear %in% exclude_years))
    year_data <- dplyr::filter(year_data, DayofYear < 366)
    
    year_data <- dplyr::filter(year_data, Month %in% months)
    
    year_data <- dplyr::filter(year_data, WaterYear == add_year)
    
    year_data <- dplyr::select(year_data, STATION_NUMBER, AnalysisDate, RollingValue)
    
    # Add the daily data from add_year to the daily stats
    daily_stats <- dplyr::left_join(daily_stats, year_data, by = c("STATION_NUMBER", "AnalysisDate"))
    
    # Warning if all daily values are NA from the add_year
    for (stn in unique(daily_stats$STATION_NUMBER)) {
      year_test <- dplyr::filter(daily_stats, STATION_NUMBER == stn)
      
      if(all(is.na(daily_stats$RollingValue)))
        warning("Daily data does not exist for the year listed in add_year and was not plotted.", call. = FALSE)
    }
    
  } 
  
  ## PLOT STATS
  ## ----------
  
  # Create manual colour and fill options
  
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
  if (is.numeric(add_year)) {
    colour_manual_list <- c(colour_manual_list, "yr.colour" = "red")
    colour_manual_labels <- c(colour_manual_labels, paste0(add_year))
  }
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)", #expression(Volume~(m^3))
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)", 
                                "Discharge (cms)")) #expression(Discharge~(m^3/s))
  daily_stats <- fill_missing_dates(daily_stats, water_year_start = water_year_start)
  daily_stats <- add_date_variables(daily_stats, water_year_start = water_year_start)
  daily_stats <- dplyr::select(daily_stats, -c("CalendarYear", "Month", "MonthName", "WaterYear"))
  
  # Create the daily stats plots
  daily_plots <- dplyr::group_by(daily_stats, STATION_NUMBER)
  daily_plots <- tidyr::nest(daily_plots)
  daily_plots <- dplyr::mutate(
    daily_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Date)) +
        {if(include_extremes) ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = FALSE)} +
        {if(is.numeric(outer_percentiles)) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(outer_percentiles)),
                                                                                    ymax = paste0("P",max(outer_percentiles)),
                                                                                    fill = paste0("'",outer_name,"'")), na.rm = FALSE)} +
        {if(is.numeric(inner_percentiles)) ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(inner_percentiles)),
                                                                                    ymax = paste0("P",max(inner_percentiles)),
                                                                                    fill = paste0("'",inner_name,"'")), na.rm = FALSE)} +
        ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = .5, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = .5, na.rm = TRUE) +
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 8))}+
        {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
        {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                        short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                        long = ggplot2::unit(.2, "cm"))} +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month",
                              limits = as.Date(c(as.character(min(daily_stats$AnalysisDate, na.rm = TRUE)), 
                                                 as.character(max(daily_stats$AnalysisDate, na.rm = TRUE)))), 
                              expand = c(0,0)) +
        ggplot2::xlab("Day of Year")+
        ggplot2::ylab(y_axis_title)+
        ggplot2::theme_bw()+
        ggplot2::labs(color = 'Daily Statistics') +
        {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nDaily Statistics')) } +
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
        {if (is.numeric(add_year)) ggplot2::geom_line(ggplot2::aes(y = RollingValue, colour = "yr.colour"), size = 0.5, na.rm = TRUE) } +
        ggplot2::scale_fill_manual(values = fill_manual_list) +
        ggplot2::scale_color_manual(values = colour_manual_list, labels = colour_manual_labels)
    ))
  
  
  
  # Create a list of named plots extracted from the tibble
  plots <- daily_plots$plot
  if (nrow(daily_plots) == 1) {
    names(plots) <- "Daily_Statistics"
  } else {
    names(plots) <- paste0(daily_plots$STATION_NUMBER, "_Daily_Statistics")
  }
  
  plots
  
}
