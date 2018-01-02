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


#' @title Plot annual summary statistics
#'
#' @description Plots annual monthly mean, median, maximum, minimum, and percentiles of daily flow values from a streamflow 
#'    dataset. Plots the statistics from all daily discharge values from all years, unless specified. Data 
#'    calculated using calc_annual_stats() function.
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
#' @param percentiles a numeric vector of percentiles to plot. Set to NA if none required. Default \code{c(10,90)}.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.   
#' @param months a numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).    
#' @param log_discharge a logical value to indicate plotting thedischarge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#' @param ignore_missing a logical value indicating whether dates with missing flow values should be included in the calculation for
#'    plotting. If \code{TRUE} then a statistic will be plotted regardless of missing dates. If \code{FALSE} then only statistics 
#'    from time periods with no missing dates will be returned. Default \code{TRUE}.
#'
#' @return A ggplot2 object with the following plots (percentile plots optional):
#'   \item{Mean}{annual mean of all daily flows}
#'   \item{Median}{annual median of all daily flows}
#'   \item{Maximum}{annual maximum of all daily flows}
#'   \item{Minimum}{annual minimum of all daily flows}
#' 
#' @examples
#' \dontrun{
#' 
#'plot_annual_stats(flowdata = flowdata)
#' 
#'plot_annual_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#'plot_annual_stats(HYDAT = "08NM116", months = 7:9)
#'
#' }
#' @export


plot_annual_stats <- function(flow_data = NULL,
                              flow_dates = Date,
                              flow_values = Value,
                              HYDAT = NULL,
                              percentiles = c(10,90),
                              rolling_days = 1,
                              rolling_align = "right",
                              water_year = FALSE,
                              water_year_start = 10,
                              start_year = 0,
                              end_year = 9999,
                              exclude_years = NULL,
                              months = 1:12,
                              log_discharge = FALSE,
                              ignore_missing = TRUE){ 
  
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
  if(!all(exclude_years %in% c(0:9999)))  stop("Years listed in exclude_years must be integers.")
  
  if(!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  
  if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
  if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  
  if(!is.logical(log_discharge))  stop("log_discharge argument must be logical (TRUE/FALSE).")
  
  if(!is.numeric(rolling_days))                        stop("rolling_days argument must be numeric")
  if(!all(rolling_days %in% c(1:180)))                 stop("rolling_days argument must be integers > 0 and <= 180)")
  if(!rolling_align %in% c("right", "left", "center")) stop("rolling_align argument must be 'right', 'left', or 'center'")
  
  
  ## CALC STATS
  ## ----------
  
  annual_stats <- fasstr::calc_annual_stats(flow_data = flow_data,
                                            flow_dates = Date,
                                            flow_values = Value,
                                            HYDAT = NULL,
                                            percentiles = percentiles,
                                            rolling_days = rolling_days,
                                            rolling_align = rolling_align,
                                            water_year = water_year, 
                                            water_year_start = water_year_start,
                                            start_year = start_year,
                                            end_year = end_year,
                                            exclude_years = exclude_years, 
                                            months = months,
                                            ignore_missing = TRUE)

  annual_stats <- tidyr::gather(annual_stats,Statistic,Value,-1)
  
  
  ## PLOT STATS
  ## ----------
  
  annual_plot <- ggplot2::ggplot(data = annual_stats, ggplot2::aes(x = Year, y = Value, color = Statistic)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(annual_stats$Value, na.rm = T) * 1.05))}+
    {if(log_discharge) ggplot2::expand_limits(y = c(min(annual_stats$Value, na.rm = T) * .95, max(annual_stats$Value, na.rm = T) * 1.05))} +
    {if(log_discharge) ggplot2::scale_y_log10(expand = c(0,0))} +
    {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0))} +
    {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"), 
                                                     mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
    ggplot2::expand_limits(y = 0) +
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("Year") +
    ggplot2::theme(legend.position = "right", 
                   legend.title = ggplot2::element_blank(),
                   legend.spacing = ggplot2::unit(0, "cm"),
                   legend.justification = "top",
                   panel.border = ggplot2::element_rect(colour = "grey80", fill = NA, size = .1),
                   plot.title = ggplot2::element_text(size = 12, colour = "grey25", face = "italic"),
                   panel.grid = ggplot2::element_line(size = .2))

  
  annual_plot
  
}

