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

#' @title Plot cumulative monthly flow statistics
#'
#' @description Plot the monthly cumulative mean, median, maximum, minimum, and 5, 25, 75, 95th percentiles for each month of the year 
#'    from a streamflow dataset. Plots the statistics from all monthly cumulative values from all years, unless specified. 
#'    Data calculated using calc_monthly_cumulative_stats() function. Can plot individual years for comparision using the 
#'    include_year argument. Defaults to volumetric cumulative flows, can use \code{use_yield} and \code{basin_area} to convert to 
#'    runoff yield.
#'    
#' @inheritParams calc_monthly_cumulative_stats
#' @inheritParams plot_daily_cumulative_stats
#'    
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Monthly_Cumulative_Stats}{a plot that contains montly cumulative flow statistics}
#'   Default plots on each object:  
#'   \item{Mean}{monthly cumulative mean}
#'   \item{Median}{monthly cumulative median}
#'   \item{Min-5 Percentile Range}{a ribbon showing the range of data between the monthly cumulative minimum and 5th percentile}
#'   \item{5-25 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 5th and 25th percentiles}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 25th and 75th percentiles}
#'   \item{75-95 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 75th and 95th percentiles}
#'   \item{95 Percentile-Max Range}{a ribbon showing the range of data between the monthly cumulative 95th percentile and the maximum}
#'   \item{'Year' Flows}{(optional) the monthly cumulative flows for the designated year}
#'   
#' @seealso \code{\link{calc_monthly_cumulative_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot volume statistics
#' plot_monthly_cumulative_stats(station_number = "08NM116") 
#' 
#' # Plot yield statistics with default HYDAT basin area
#' plot_monthly_cumulative_stats(station_number = "08NM116",
#'                               use_yield = TRUE) 
#' 
#' # Plot yield statistics with custom basin area
#' plot_monthly_cumulative_stats(station_number = "08NM116",
#'                               use_yield = TRUE,
#'                               basin_area = 800) 
#' }
#' @export



plot_monthly_cumulative_stats <- function(data,
                                          dates = Date,
                                          values = Value,
                                          groups = STATION_NUMBER,
                                          station_number,
                                          use_yield = FALSE, 
                                          basin_area,
                                          water_year_start = 1,
                                          start_year,
                                          end_year,
                                          exclude_years,
                                          log_discharge = FALSE,
                                          include_title = FALSE,
                                          include_year){
  
  ## ARGUMENT CHECKS 
  ## others will be check in calc_ function
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }
  if (missing(exclude_years)) {
    exclude_years = NULL
  }
  if (missing(basin_area)) {
    basin_area = NA
  }
  if (missing(include_year)) {
    include_year = NULL
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
  
  
  ## CALC STATS
  ## ----------
  
  monthly_stats <- calc_monthly_cumulative_stats(data = flow_data,
                                                 percentiles = c(5,25,75,95),
                                                 use_yield = use_yield, 
                                                 basin_area = basin_area,
                                                 water_year_start = water_year_start,
                                                 start_year = start_year,
                                                 end_year = end_year,
                                                 exclude_years = exclude_years)
  
  
  ## ADD YEAR IF SELECTED
  ## --------------------
  
  if(!is.null(include_year)){
    
    year_data <- fill_missing_dates(data = flow_data, water_year_start = water_year_start)
    year_data <- add_date_variables(data = year_data, water_year_start = water_year_start)
    
    # Add cumulative flows
    if (use_yield){
      year_data <- add_cumulative_yield(data = year_data, water_year_start = water_year_start, basin_area = basin_area)
      year_data$Cumul_Flow <- year_data$Cumul_Yield_mm
    } else {
      year_data <- add_cumulative_volume(data = year_data, water_year_start = water_year_start)
      year_data$Cumul_Flow <- year_data$Cumul_Volume_m3
    }
    
    year_data <- dplyr::filter(year_data, WaterYear >= start_year & WaterYear <= end_year)
    year_data <- dplyr::filter(year_data, !(WaterYear %in% exclude_years))
    
   
    year_data <- dplyr::filter(year_data, WaterYear == include_year)
    
    year_data <- dplyr::summarize(dplyr::group_by(year_data, STATION_NUMBER, WaterYear, MonthName),
                                     Monthly_Total = max(Cumul_Flow, na.rm = FALSE))
    year_data <- dplyr::rename(year_data, "Month" = MonthName)
    
    # Add the daily data from include_year to the daily stats
    monthly_stats <- dplyr::left_join(monthly_stats, year_data, by = c("STATION_NUMBER", "Month"))
    
    # Warning if all daily values are NA from the include_year
    for (stn in unique(monthly_stats$STATION_NUMBER)) {
      year_test <- dplyr::filter(monthly_stats, STATION_NUMBER == stn)
      
      if(all(is.na(monthly_stats$Monthly_Total)))
        warning("Daily data does not exist for the year listed in include_year and was not plotted.", call. = FALSE)
    }
  }
    
  monthly_stats[is.na(monthly_stats)] <- 0
  
  ## PLOT STATS
  ## ----------

  # Create the daily stats plots
  monthly_plots <- dplyr::group_by(monthly_stats, STATION_NUMBER)
  monthly_plots <- tidyr::nest(monthly_plots)
  monthly_plots <- dplyr::mutate(monthly_plots,
                               plot = purrr::map2(data, STATION_NUMBER,
       ~ggplot2::ggplot(data = ., ggplot2::aes(x = Month, group = 1)) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = P5, fill = "Min-5th Percentile")) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = P5, ymax = P25, fill = "5th-25th Percentile")) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "25th-75th Percentile")) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = P75, ymax = P95, fill = "75th-95th Percentile")) +
         ggplot2::geom_ribbon(ggplot2::aes(ymin = P95, ymax = Maximum, fill = "95th Percentile-Max")) +
         ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = 0.7) +
         ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = 0.7) +
         ggplot2::scale_fill_manual(values = c("Min-5th Percentile" = "orange" , "5th-25th Percentile" = "yellow",
                                               "25th-75th Percentile" = "skyblue1", "75th-95th Percentile" = "dodgerblue2",
                                               "95th Percentile-Max" = "royalblue4")) +
         ggplot2::scale_color_manual(values = c("Median" = "purple3", "Mean" = "springgreen4")) +
         {if (!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 7))} +
         {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 8, base = 10) )} +
         {if (log_discharge) ggplot2::annotation_logticks(base= 10, sides = "l", colour = "grey25", size = 0.3,
                                                          short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                          long = ggplot2::unit(.2, "cm"))} +
         ggplot2::xlab("Month")+
         ggplot2::scale_x_discrete(expand = c(0.01,0.01)) +
         {if(!use_yield) ggplot2::ylab("Cumulative Discharge (cubic metres)")} +
         {if(use_yield) ggplot2::ylab("Cumulative Runoff Yield (mm)")} +
         ggplot2::theme_bw() +
         ggplot2::labs(color = 'Monthly Statistics', fill = "Monthly Ranges") +
         {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nDaily Statistics'), fill = "Daily Ranges") } +    
         ggplot2::theme(axis.text=ggplot2::element_text(size = 10, colour = "grey25"),
                        axis.title=ggplot2::element_text(size = 12, colour = "grey25"),
                        axis.title.y=ggplot2::element_text(margin = ggplot2::margin(0,0,0,0)),
                        axis.ticks = ggplot2::element_line(size = .1, colour = "grey25"),
                        axis.ticks.length=ggplot2::unit(0.05, "cm"),
                        panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_line(size = .1),
                        panel.background = ggplot2::element_rect(fill = "grey94"),
                        legend.text = ggplot2::element_text(size = 9, colour = "grey25"),
                        legend.box = "vertical",
                        legend.justification = "top",
                        legend.key.size = ggplot2::unit(0.4, "cm"),
                        legend.spacing = ggplot2::unit(0, "cm")) +
         ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2)) +
         {if (is.numeric(include_year)) ggplot2::geom_line(ggplot2::aes(y = Monthly_Total, colour = "yr.colour"), size = 0.7)} +
         {if (is.numeric(include_year)) ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                                                    labels = c("Mean", "Median", paste0(include_year, " Flows")))}
                               ))
                          


  # Create a list of named plots extracted from the tibble
  plots <- monthly_plots$plot
  if (nrow(monthly_plots) == 1) {
    names(plots) <- paste0(ifelse(use_yield, "Monthly_Cumulative_Yield_Stats", "Monthly_Cumulative_Volumetric_Stats"))
  } else {
    names(plots) <- paste0(monthly_plots$STATION_NUMBER, ifelse(use_yield, "_Monthly_Cumulative_Yield_Stats", "_Monthly_Cumulative_Volumetric_Stats"))
  }
  
  plots
  
}

