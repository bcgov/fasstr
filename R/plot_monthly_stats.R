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

#' @title Plot monthly summary statistics
#'
#' @description Plots monthly mean, median, maximum, minimum, and percentiles for each month of all years of daily flow values 
#'    from a streamflow dataset. Plots the statistics from all daily discharge values from all years, unless specified. Data
#'    calculated using the calc_monthly_stats() function.
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
#' @param months Numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#' 
#' @return A list of ggplot2 objects for each monthly statistic:
#'   \item{Monthly Mean Flows}{mean of all daily flows for a given month and year}
#'   \item{Monthly Median Flows}{median of all daily flows for a given month and year}
#'   \item{Monthly Maximum Flows}{maximum of all daily flows for a given month and year}
#'   \item{Monthly Minimum Flows}{minimum of all daily flows for a given month and year}
#'   \item{Monthly P'n' Flows}{each n-th percentile selected for a given month and year}
#'   Default percentile plots:
#'   \item{Monthly P10 Flows}{10th percentile of all daily flows for a given month and year}
#'   \item{Monthl P20 Flows}{20th percentile of all daily flows for a given month and year}
#'   
#' @examples
#' \dontrun{
#' 
#'plot_monthly_stats(data = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'plot_monthly_stats(data = "08NM116", months = 7:9)
#'
#' }
#' @export



plot_monthly_stats <- function(data = NULL,
                               dates = Date,
                               values = Value,
                               percentiles = NA,
                               rolling_days = 1,
                               rolling_align = "right",
                               water_year = FALSE,
                               water_year_start = 10,
                               start_year = 0,
                               end_year = 9999,
                               exclude_years = NULL,
                               months = 1:12,
                               ignore_missing = TRUE,
                               log_discharge = FALSE){
  
  
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
  
  monthly_data <- calc_monthly_stats(data = data,
                                     percentiles = percentiles,
                                     rolling_days = rolling_days,
                                     rolling_align = rolling_align,
                                     water_year = water_year,
                                     water_year_start = water_year_start,
                                     start_year = start_year,
                                     end_year = end_year,
                                     exclude_years = exclude_years, 
                                     months = months,
                                     ignore_missing = ignore_missing)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(monthly_data)) {
    monthly_data <- dplyr::ungroup(monthly_data)
    monthly_data <- dplyr::select(monthly_data, -STATION_NUMBER)
  }
  
  monthly_data <- tidyr::gather(monthly_data, Statistic, Value, -(1:2))
  
  
  ## PLOT STATS
  ## ----------
  
  # Create list to add each plot to
  monthly_stats_plots <- list()
  
  # Cycle through each stat and add to list
  for (stat in unique(monthly_data$Statistic)) {
    monthly_data_plot <- dplyr::filter(monthly_data, Statistic == stat)
    monthly_plot <- ggplot2::ggplot(data = monthly_data_plot, ggplot2::aes(x = Year, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(colour = Month), alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(colour = Month)) +
      ggplot2::facet_wrap(~Month, scales = "free_x") +
      ggplot2::ggtitle(paste0("Monthly ", stat, " Flows")) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      {if(!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))} +
      {if(log_discharge) ggplot2::scale_y_log10()} +
      {if(log_discharge) ggplot2::annotation_logticks(base = 10, "left", colour = "grey25", size = 0.3,
                                                      short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"), 
                                                      long = ggplot2::unit(.2, "cm"))} +
      ggplot2::ylab("Discharge (cms)") +
      ggplot2::guides(colour = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10)) +
      ggplot2::scale_colour_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                              "Apr" = "forestgreen", "May" = "limegreen", "Jun" = "gold",
                                              "Jul" = "orange", "Aug" = "red", "Sep" = "darkred",
                                              "Oct" = "orchid", "Nov" = "purple3", "Dec" = "midnightblue"))
    
    monthly_stats_plots[[paste0("Monthly_",stat)]] <- monthly_plot

  }
  
  
  return(monthly_stats_plots)
}

