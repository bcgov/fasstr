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


#' @title Plot annual summary statistics for data screening
#'
#' @description Plots the mean, median, maximum, minimum, standard deviation of annual flows. Plots the statistics from all daily 
#'    discharge values from all years, unless specified. Data calculated using screen_flow_data() function.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates and values.
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param roll_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param roll_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#'
#' @return A ggplot2 object with the following plots:
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{StandardDeviation}{annual 1 standard deviation of all daily flows for a given year}
#'
#' @examples
#' \dontrun{
#' 
#'plot_data_screening(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



plot_data_screening <- function(data = NULL,
                                dates = Date,
                                values = Value,
                                roll_days = 1,
                                roll_align = "right",
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = 0,
                                end_year = 9999){ 
  
  
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
    

  }
  
  
  ## CALC STATS
  ## ----------
  
  flow_summary <- fasstr::screen_flow_data(data = data,
                                           roll_days = roll_days,
                                           roll_align = roll_align,
                                           water_year = water_year,
                                           water_year_start = water_year_start,
                                           start_year = start_year,
                                           end_year = end_year)
  
  flow_summary <- dplyr::select(flow_summary, Year, Minimum, Maximum, Mean, StandardDeviation)
  flow_summary <- tidyr::gather(flow_summary, Statistic, Value, 2:5)
  
  
  ## PLOT STATS
  ## ----------
  
  ggplot2::ggplot(data = flow_summary, ggplot2::aes(x = Year, y = Value)) +
    ggplot2::geom_line(colour = "dodgerblue4") +
    ggplot2::geom_point(colour = "firebrick3") +
    ggplot2::facet_wrap(~Statistic, ncol = 2, scales = "free_y") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::ylab("Discharge (cms)") +
    ggplot2::xlab("Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 10))
    
  
  
}

