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


#' @title Plot annual flow timing
#'
#' @description Plots annual the timing (day of year) of occurrence of portions of total annual flow of daily flow 
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'    Data calculated using calc_annual_flow_timing() function.
#'
#' @param flow_data a data frame of daily mean flow data that contains columns of dates, flow values, and (optional) station 
#'    names/numbers. Leave blank if using \code{HYDAT} argument.
#' @param flow_dates a column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Leave blank if using \code{HYDAT} 
#'    argument. Default \code{Date}. 
#' @param flow_values a column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Leave blank if using \code{HYDAT} argument. Default \code{Value}.
#' @param HYDAT a character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the 
#'    \code{tidyhydat} package and a HYDAT database are required. Leave blank if using \code{flow_data} arguments.
#' @param percent_total a numeric vector of percents of total annual flows to determine dates. Default \code{c(25,33.3,50,75)}.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.   
#'
#' @return A ggplot2 object with the following plots:
#'   \item{DoY_'n'pct_TotalQ}{day of year for each n-percent of total volumetric discharge}
#'   Default plots:   
#'   \item{DoY_25pct_TotalQ}{day of year of 25-percent of total volumetric discharge}
#'   \item{DoY_33.3pct_TotalQ}{day of year of 33.3-percent of total volumetric discharge}
#'   \item{DoY_50pct_TotalQ}{day of year of 50-percent of total volumetric discharge}
#'   \item{DoY_75pct_TotalQ}{day of year of 75-percent of total volumetric discharge}
#'   
#' @examples
#' \dontrun{
#' 
#'plot_annual_flow_timing(flowdata = flowdata)
#' 
#'plot_annual_flow_timing(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percent_total = 50)
#'
#' }
#' @export


plot_annual_flow_timing <- function(flow_data = NULL,
                                    flow_dates = Date,
                                    flow_values = Value,
                                    HYDAT = NULL,
                                    percent_total = c(25,33.3,50,75),
                                    water_year = FALSE,
                                    water_year_start = 10,
                                    start_year = 0,
                                    end_year = 9999,
                                    exclude_years = NULL){ 
  
  ## SETUP FLOW DATA
  ## ---------------
  
  # Check if data is provided
  if(is.null(flow_data) & is.null(HYDAT))   stop("No flow data provided, must use flow_data or HYDAT arguments.")
  if(!is.null(flow_data) & !is.null(HYDAT)) stop("Only one of flow_data or HYDAT arguments can be used.")
  
  # Get HYDAT data if selected and stations exist
  if(!is.null(HYDAT)) {
    if(!all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1]))) stop("One or more stations listed in 'HYDAT' do not exist.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # Ungroup data if grouped
  flow_data <- dplyr::ungroup(flow_data)
  
  # Get the just Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Gather required columns
  flow_data <- flow_data[,c(as.character(substitute(flow_dates)),
                            as.character(substitute(flow_values)))]
  colnames(flow_data) <- c("Date","Value")
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in flow_data data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  if(length(start_year)>1)        stop("Only one start_year value can be listed")
  if(!start_year %in% c(0:9999))  stop("start_year must be an integer.")
  if(length(end_year)>1)          stop("Only one end_year value can be listed")
  if(!end_year %in% c(0:9999))    stop("end_year must be an integer.")
  if(start_year > end_year)       stop("start_year must be less than or equal to end_year.")
  
  if(!is.null(exclude_years) & !is.numeric(exclude_years)) stop("List of exclude_years must be numeric - ex. 1999 or c(1999,2000).")
  if(!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.")
  
  if(!is.numeric(percent_total))                   stop("percent_total must be numeric")
  if(!all(percent_total > 0 & percent_total < 100)) stop("percent_total must be >0 and <100)")
  
  
  ## CALC STATS
  ## ----------
  
  flowtiming_data <- fasstr::calc_annual_flow_timing(flow_data = flow_data,
                                                     flow_dates = Date,
                                                     flow_values = Value,
                                                     HYDAT = NULL,
                                                     percent_total = percent_total,
                                                     water_year = water_year,
                                                     water_year_start = water_year_start,
                                                     start_year = start_year,
                                                     end_year = end_year,
                                                     exclude_years = exclude_years)
  
  flowtiming_data <- dplyr::select(flowtiming_data, Year, dplyr::contains("TotalQ"), -dplyr::contains("Date"))
  flowtiming_data <- tidyr::gather(flowtiming_data, Statistic, Value, -1)
  flowtiming_data <- dplyr::mutate(flowtiming_data, Statistic = substr(Statistic, 5, nchar(Statistic)))
  
  
  ## PLOT STATS
  ## ----------
  
  flowtiming_plot <- ggplot2::ggplot(data = flowtiming_data, ggplot2::aes(x = Year, y = Value)) +
    ggplot2::geom_line(ggplot2::aes(colour = Statistic)) +
    ggplot2::geom_point(ggplot2::aes(colour = Statistic)) +
    ggplot2::facet_wrap(~Statistic, scales = "free_x", ncol = 1, strip.position = "right") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    ggplot2::ylab("Day of Year") +
    ggplot2::xlab("Year") +
    ggplot2::guides(colour = FALSE) +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey80", fill = NA, size = .1),
                   panel.grid = ggplot2::element_line(size = .2))
  
  
  flowtiming_plot
  
}

