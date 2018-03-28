# Copyright 2017 Province of British Columbia
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

#' @title Plot the daily summary statistics
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
#' @return A list of ggplot2 objects, the first the daily statistics plot containing the listed plots below, and the sebsequent plots for each
#'    year of data provided containing the first plot plus the daily flow data for each year.
#'   \item{Mean}{daily mean}
#'   \item{Median}{daily median}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the daily 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the daily 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the daily minimum and maximums}
#'   \item{'Year' Flows}{(on annual plots) the daily flows for the designated year}
#'    
#' @examples
#' \dontrun{
#' 
#' plot_daily_stats(station_number = "08NM116",
#'                  water_year = TRUE, 
#'                  water_year_start = 8)
#'
#' }
#' @export


plot_daily_stats <- function(data = NULL,
                             dates = Date,
                             values = Value,
                             station_number = NULL,
                             roll_days = 1,
                             roll_align = "right",
                             water_year = FALSE,
                             water_year_start = 10,
                             start_year = 0,
                             end_year = 9999,
                             exclude_years = NULL,
                             complete_years = FALSE,
                             months = 1:12,
                             ignore_missing = FALSE,
                             log_discharge = TRUE,
                             include_year = NULL){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  log_discharge_checks(log_discharge) 
  include_year_checks(include_year)
  one_station_number_stop(station_number)
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_plot_cols(data = flow_data, 
                                dates = as.character(substitute(dates)),
                                values = as.character(substitute(values)))
  
  
  ## CALC STATS
  ## ----------
  
  daily_stats <- fasstr::calc_daily_stats(data = flow_data,
                                          percentiles = c(5,25,75,95),
                                          roll_days = roll_days,
                                          roll_align = roll_align,
                                          water_year = water_year,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years, 
                                          complete_years = complete_years,
                                          months = months,
                                          ignore_missing = ignore_missing)
  
  if (water_year) {
    # Create origin date to apply to flow_data and Q_daily later on
    if (water_year_start==1)         {origin_date <- as.Date("1899-12-31")
    } else if (water_year_start==2)  {origin_date <- as.Date("1899-01-31")
    } else if (water_year_start==3)  {origin_date <- as.Date("1899-02-28")
    } else if (water_year_start==4)  {origin_date <- as.Date("1899-03-31")
    } else if (water_year_start==5)  {origin_date <- as.Date("1899-04-30")
    } else if (water_year_start==6)  {origin_date <- as.Date("1899-05-31")
    } else if (water_year_start==7)  {origin_date <- as.Date("1899-06-30")
    } else if (water_year_start==8)  {origin_date <- as.Date("1899-07-31")
    } else if (water_year_start==9)  {origin_date <- as.Date("1899-08-31")
    } else if (water_year_start==10) {origin_date <- as.Date("1899-09-30")
    } else if (water_year_start==11) {origin_date <- as.Date("1899-10-31")
    } else if (water_year_start==12) {origin_date <- as.Date("1899-11-30")}
  }  else {
    origin_date <- as.Date("1899-12-31")
  }
  
  daily_stats <- dplyr::mutate(daily_stats, Date = as.Date(DayofYear, origin = origin_date))
  
  
  ## PLOT STATS
  ## ----------
  
  # Create the daily stats plots
  daily_stats_plot <- ggplot2::ggplot(daily_stats, ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P5, ymax = P95, fill = "5-95 Percentiles")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "25-75 Percentiles")) +
    ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = .5) +
    ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = .5) +
    ggplot2::scale_fill_manual(values = c("Minimum-Maximum" = "lightblue2", "5-95 Percentiles" = "lightblue3",
                                          "25-75 Percentiles" = "lightblue4")) +
    ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4"), labels = c("Mean", "Median")) +
    {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0))}+
    {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))} +
    {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                     short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                     long = ggplot2::unit(.2, "cm"))} +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month",
                          limits = as.Date(c(NA, as.character(max(daily_stats$Date)))), expand = c(0,0)) +
    ggplot2::xlab("Day of Year")+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::theme_bw()+
    ggplot2::labs(color = 'Daily Statistics', fill = "Daily Ranges") +  
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
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2))
  
  
  ## ADD YEAR IF SELECTED
  ## --------------------
  
  if(!is.null(include_year)){
   
    flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
    colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
    
    if (water_year) {
      flow_data$AnalysisYear <- flow_data$WaterYear
      flow_data$AnalysisDoY <- flow_data$WaterDayofYear
    }  else {
      flow_data$AnalysisYear <- flow_data$Year
      flow_data$AnalysisDoY <- flow_data$DayofYear
    }
    flow_data <- dplyr::mutate(flow_data, AnalysisDate = as.Date(AnalysisDoY, origin = origin_date))
    flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
    flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
    flow_data <- dplyr::filter(flow_data, AnalysisDoY < 366)
    
    flow_data <- dplyr::filter(flow_data, Month %in% months)
    
    
    if(!include_year %in% flow_data$AnalysisYear) stop(paste0("Year in include_year does not exist. Please choose a year between ",
                                                              min(flow_data$AnalysisYear), " and ", max(flow_data$AnalysisYear), "."), 
                                                       call. = FALSE)
    flow_data <- dplyr::filter(flow_data, AnalysisYear == include_year)
    
    # Plot the year
    suppressMessages(
      daily_stats_plot <- daily_stats_plot +
        ggplot2::geom_line(data = flow_data, ggplot2::aes(x = AnalysisDate, y = RollingValue, colour = "yr.colour"), size = 0.5) +
        ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                    labels = c("Mean", "Median", paste0(include_year, " Flows")))
    )
  }
  
  
  suppressWarnings(print(
    daily_stats_plot  
  ))
}
