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

#' @title Plot annual days above and below normal
#'
#' @description Plots the number of days per year outside of the 'normal' range (typically between 25 and 75th percentiles) for
#'    each day of the year. Upper and lower-range percentiles are calcuated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. All days above or below the normal range are included. Calculates the statistics 
#'    from all daily discharge values from all years, unless specified. Data calculated using calc_annual_outside_normal() function.
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
#' @param normal_percentiles Numeric vector of two values, lower and upper percentiles, respectively indicating the limits of the 
#'    normal range. Default \code{c(25,75)}.
#' @param roll_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param roll_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
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
#' 
#' @return A ggplot2 object with the following plots:
#'   \item{Days_Below_Normal}{number of days per year below the daily normal (default 25th percentile)}
#'   \item{Days_Above_Normal}{number of days per year above the daily normal (default 75th percentile)}
#'   \item{Days_Outside_Normal}{number of days per year below and above the daily normal (default 25/75th percentile)}
#'
#' @examples
#' \dontrun{
#' 
#' plot_annual_outside_normal(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#'
#--------------------------------------------------------------


plot_annual_outside_normal <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       normal_percentiles = c(25, 75),
                                       roll_days = 1,
                                       roll_align = "right",
                                       water_year = FALSE,
                                       water_year_start = 10,
                                       start_year = 0,
                                       end_year = 9999,
                                       exclude_years = NULL, 
                                       months = 1:12){
  
  
  
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
  
  normal_data <- fasstr::calc_annual_outside_normal(data = data,
                                                    normal_percentiles = normal_percentiles,
                                                    roll_days = roll_days,
                                                    roll_align = roll_align,
                                                    water_year = water_year,
                                                    water_year_start = water_year_start,
                                                    start_year = start_year,
                                                    end_year = end_year,
                                                    exclude_years = exclude_years, 
                                                    months = months)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(normal_data)) {
    normal_data <- dplyr::ungroup(normal_data)
    normal_data <- dplyr::select(normal_data, -STATION_NUMBER)
  }
  
  normal_data <- tidyr::gather(normal_data, Statistic, Value, -1)
  normal_data <- dplyr::mutate(normal_data, Statistic = substr(Statistic, 6, nchar(Statistic)))
  normal_data <- dplyr::mutate(normal_data, Statistic = gsub("_", " ", Statistic))
  
  
  
  ## PLOT STATS
  ## ----------
  
  normal_plot <- ggplot2::ggplot(data = normal_data, ggplot2::aes(x = Year, y = Value, colour = Statistic))+
    ggplot2::geom_line(alpha = 0.5)+
    ggplot2::geom_point()+
    ggplot2::facet_wrap(~Statistic, scales="free_x", ncol = 1, strip.position = "right")+
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::ylab("Number of Days")+
    ggplot2::xlab("Year")+
    ggplot2::guides(colour = FALSE)+
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 10))
  
  return(normal_plot)
  
}

