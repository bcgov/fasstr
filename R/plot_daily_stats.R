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
#' @description Plot the daily mean, median, maximum, minimum, and percentiles for each day of the year of daily flow values 
#'    from a streamflow dataset. Plots the statistics from all daily discharge values from all years, unless specified. Can determine
#'    statistics of rolling mean days (e.g. 7-day flows) using the roll_days argument. Data calculated using calc_daily_stats()
#'    function.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams plot_annual_stats
#' @param include_year A numeric value indicating a year of daily flows to add to the daily statistics plot. Leave blank for no years.
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Daily_Stats}{a plot that contains daily flow statistics}
#'   Default plots on each object:  
#'   \item{Mean}{daily mean}
#'   \item{Median}{daily median}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the daily 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the daily 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the daily minimum and maximums}
#'   \item{'Year' Flows}{(on annual plots) the daily flows for the designated year}
#'   
#' @seealso \code{\link{calc_daily_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' plot_daily_stats(data = flow_data,
#'                  start_year = 1980)
#' 
#' # Plot statistics using station_number argument with defaults
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1980)
#' 
#' # Plot statistics regardless if there is missing data for a given day of year
#' plot_daily_stats(station_number = "08NM116",
#'                  ignore_missing = TRUE)
#'                   
#' # Plot statistics using only years with no missing data
#' plot_daily_stats(station_number = "08NM116",
#'                  complete_years = TRUE)
#' 
#' # Plot statistics for water years starting in October
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1980,
#'                  end_year = 2010,
#'                  water_year_start = 10)
#'                  
#' # Plot statistics with custom years
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1981,
#'                  end_year = 2010,
#'                  exclude_years = c(1991,1993:1995))
#'                   
#' # Plot statistics for 7-day flows for July-September months only
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1980,
#'                  roll_days = 7,
#'                  months = 7:9)
#' 
#' # Plot statistics without a log-scale Discharge axis
#' plot_daily_stats(station_number = "08NM116",
#'                  start_year = 1981,
#'                  end_year = 2010,
#'                  log_discharge = FALSE)
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
                             log_discharge = TRUE,
                             include_title = FALSE,
                             include_year){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(include_year)) {
    include_year = NULL
  }
  if (missing(exclude_years)) {
    exclude_years = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }

  
  log_discharge_checks(log_discharge) 
  include_year_checks(include_year)
  include_title_checks(include_title)

  
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
  
  daily_stats <-calc_daily_stats(data = flow_data,
                                 percentiles = c(5,25,75,95),
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
  
  if (all(sapply(daily_stats[4:11], function(x)all(is.na(x))))) {
    daily_stats[is.na(daily_stats)] <- 1
  }
  
  ## ADD YEAR IF SELECTED
  ## --------------------
  
  if(!is.null(include_year)){
    
    year_data <- fill_missing_dates(data = flow_data, water_year_start = water_year_start)
    year_data <- add_date_variables(data = year_data, water_year_start = water_year_start)
    year_data <- add_rolling_means(data = year_data, roll_days = roll_days, roll_align = roll_align)
    colnames(year_data)[ncol(year_data)] <- "RollingValue"
    
    year_data <- dplyr::mutate(year_data, AnalysisDate = as.Date(DayofYear, origin = origin_date))
    year_data <- dplyr::filter(year_data, WaterYear >= start_year & WaterYear <= end_year)
    year_data <- dplyr::filter(year_data, !(WaterYear %in% exclude_years))
    year_data <- dplyr::filter(year_data, DayofYear < 366)
    
    year_data <- dplyr::filter(year_data, Month %in% months)
    
    year_data <- dplyr::filter(year_data, WaterYear == include_year)
    
    year_data <- dplyr::select(year_data, STATION_NUMBER, AnalysisDate, RollingValue)
    
    # Add the daily data from include_year to the daily stats
    daily_stats <- dplyr::left_join(daily_stats, year_data, by = c("STATION_NUMBER", "AnalysisDate"))
    
    # Warning if all daily values are NA from the include_year
    for (stn in unique(daily_stats$STATION_NUMBER)) {
      year_test <- dplyr::filter(daily_stats, STATION_NUMBER == stn)
      
      if(all(is.na(daily_stats$RollingValue)))
        warning("Daily data does not exist for the year listed in include_year and was not plotted.", call. = FALSE)
    }
    
  } 
  
  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  
  # Create the daily stats plots
  daily_plots <- dplyr::group_by(daily_stats, STATION_NUMBER)
  daily_plots <- tidyr::nest(daily_plots)
  daily_plots <- dplyr::mutate(daily_plots,
                               plot = purrr::map2(data, STATION_NUMBER, 
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = AnalysisDate)) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = TRUE) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = P5, ymax = P95, fill = "5-95 Percentiles"), na.rm = TRUE) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "25-75 Percentiles"), na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = .5, na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = .5, na.rm = TRUE) +
            ggplot2::scale_fill_manual(values = c("Minimum-Maximum" = "lightblue2", "5-95 Percentiles" = "lightblue3",
                                                  "25-75 Percentiles" = "lightblue4")) +
            ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4"), labels = c("Mean", "Median")) +
            {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 8))}+
            {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10))} +
            {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                            short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                            long = ggplot2::unit(.2, "cm"))} +
            ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month",
                                  limits = as.Date(c(NA, as.character(max(daily_stats$AnalysisDate)))), expand = c(0,0)) +
            ggplot2::xlab("Day of Year")+
            ggplot2::ylab(y_axis_title)+
            ggplot2::theme_bw()+
            ggplot2::labs(color = 'Daily Statistics', fill = "Daily Ranges") +  
            {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nDaily Statistics'), fill = "Daily Ranges") } +    
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
                           legend.spacing = ggplot2::unit(0, "cm")) +
            ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2)) +
            {if (is.numeric(include_year)) ggplot2::geom_line(ggplot2::aes(y = RollingValue, colour = "yr.colour"), size = 0.5, na.rm = TRUE) } +
            {if (is.numeric(include_year)) ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                                                       labels = c("Mean", "Median", paste0(include_year, " Flows"))) }
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
