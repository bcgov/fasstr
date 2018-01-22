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
#'    statistics of rolling mean days (e.g. 7-day flows) using the rolling_days argument. Data calculated using calc_daily_stats()
#'    function.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groups (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{NA}.
#' @param rolling_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param rolling_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.             
#' @param complete_years Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}.          
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
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
#'plot_daily_stats(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


plot_daily_stats <- function(data = NULL,
                             dates = Date,
                             values = Value,
                             rolling_days = 1,
                             rolling_align = "right",
                             water_year = FALSE,
                             water_year_start = 10,
                             start_year = 0,
                             end_year = 9999,
                             exclude_years = NULL,
                             complete_years = FALSE,
                             ignore_missing = TRUE,
                             log_discharge = TRUE,
                             include_year = NULL){
  
  
  
  ## CHECKS ON DATA FOR CALC
  ##------------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(!is.data.frame(data) & !is.vector(data)) stop("No data provided, must provide a data frame or HYDAT station number(s).")
  
  # Check HYDAT stations
  if(is.vector(data)) {
    if(length(data) != 1)   stop("Only one HYDAT station number can be listed for this function.")
    if(!data %in% dplyr::pull(tidyhydat::allstations[1]))  stop("Station number listed in data argument does not exist in HYDAT.")
  }
  
  if(is.data.frame(data)) {
    # Get the just groups (default STATION_NUMBER), Date, and Value columns
    # This method allows the user to select the Station, Date or Value columns if the column names are different
    if(!as.character(substitute(values)) %in% names(data) & !as.character(substitute(dates)) %in% names(data)) 
      stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
    if(!as.character(substitute(dates)) %in% names(data))  
      stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
    if(!as.character(substitute(values)) %in% names(data)) 
      stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
    
    # Temporarily rename the Date and Value columns
    data <- data[,c(as.character(substitute(dates)),
                    as.character(substitute(values)))]
    colnames(data) <- c("Date","Value")
    data <- dplyr::ungroup(data)
    
    
    # Check columns are in proper formats
    if(!inherits(data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
    if(!is.numeric(data$Value))          stop("'Value' column in data frame does not contain numeric values.")   
    
    # Remove these to fix warnings?
    rm(c(dates,values))
  }
  
  if(!is.logical(log_discharge))  stop("log_discharge argument must be logical (TRUE/FALSE).")
  
  
  ## CALC STATS
  ## ----------
  
  daily_stats <- fasstr::calc_daily_stats(data = data,
                                          percentiles = c(5,25,75,95),
                                          rolling_days = rolling_days,
                                          rolling_align = rolling_align,
                                          water_year = water_year,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years, 
                                          complete_years = complete_years,
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
    
    if(length(include_year) != 1)  stop("Only one include_year numeric value can be provided.")
    if(!is.numeric(include_year))  stop("include_year argument must be numeric.")
    
    # Get and set up daily data
    if(is.vector(data)) {
      flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
    } else {
      flow_data <- data
    }
    
    flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    flow_data <- add_rolling_means(data = flow_data, days = rolling_days, align = rolling_align)
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
    
    if(!include_year %in% flow_data$AnalysisYear) stop(paste0("Year in include_year does not exist. Please choose a year between ",
                                                              min(flow_data$AnalysisYear), " and ", max(flow_data$AnalysisYear), "."))
    flow_data <- dplyr::filter(flow_data, AnalysisYear == include_year)
    
    # Plot the year
    suppressMessages(
      daily_stats_plot <- daily_stats_plot +
        ggplot2::geom_line(data = flow_data, ggplot2::aes(x = AnalysisDate, y = RollingValue, colour = "yr.colour"), size = 0.5) +
        ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                    labels = c("Mean", "Median", paste0(include_year, " Flows")))
    )
  }
  
  
  
  daily_stats_plot  
  
}
