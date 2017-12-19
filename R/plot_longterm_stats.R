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

#' @title Plot the long-term and long-term monthly summary statistics
#'
#' @description Plots the long-term and long-term monthly mean, median, maximum, minimum, and 5, 25, 75, and 95 percentiles of daily 
#'    flow values from a single streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. 
#'    Data calculated using calc_longterm_stats() function.
#'
#' @param flow_data a data frame of daily mean flow data that contains columns of dates, flow values, and (optional) station 
#'    names/numbers. Leave blank if using \code{HYDAT} argument.
#' @param flow_dates a column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Leave blank if using \code{HYDAT} 
#'    argument. Default \code{Date}. 
#' @param flow_values a column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Leave blank if using \code{HYDAT} argument. Default \code{Value}.
#' @param flow_stations a column in flow_data that contains station identifiers for each flow data set.
#'    Removing 'STATION_NUMBER' column in flow_data or incorrectly identifying will calculate statistics on all flow values from all stations.
#'    If using \code{HYDAT} argument, setting \code{flow_stations} to anything besides \code{STATION_NUMBER} will have similar effects.
#'    Default \code{STATION_NUMBER}. Must select just one station to plot using \code{plot_station} argument.
#' @param HYDAT a character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the 
#'    \code{tidyhydat} package and a HYDAT database are required. Leave blank if using \code{flow_data} arguments.
#' @param plot_station a character string identifying the station listed in the flow data/HYDAT data to plot if multiple stations are 
#'    provided.
#' @param water_year a logical value indicating whether to use water years to group flow data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start a numeric value indicating the month of the start of the water year. Used if \code{water_year=TRUE}. 
#'    Default \code{10}.
#' @param start_year a numeric value of the first year to consider for analysis. Leave blank if all years are required.
#' @param end_year a numeric value of the last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years a numeric vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param log_discharge a logical value to indicate plotting thedischarge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#'
#' @return A ggplot2 object with the following plots:
#'   \item{Long-term Mean}{mean of all daily flows over all years}
#'   \item{Long-term Median}{median of all daily flows over all years}
#'   \item{Monthly Mean}{mean of all daily flows for each month over all years}
#'   \item{Monthly Median}{median of all daily flows for each month over all years}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the monthly 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the monthly minimum and maximums}
#'   
#' @examples
#' \dontrun{
#' 
#'plot_longterm_stats(flow_data = flow_data)
#' 
#'plot_longterm_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------


plot_longterm_stats <- function(flow_data=NULL,
                                flow_dates=Date,
                                flow_values=Value,
                                flow_stations=STATION_NUMBER,
                                HYDAT=NULL,
                                plot_station=NULL,
                                water_year=FALSE,
                                water_year_start=10,
                                start_year=0,
                                end_year=9999,
                                exclude_years=NULL,
                                log_discharge=TRUE,
                                ignore_missing=TRUE){
  
  
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
  
  # Save the original columns (to check for STATION_NUMBER later) and ungroup
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for station grouping)
  if(!as.character(substitute(flow_stations)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(flow_stations))] <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Gather required columns
  flow_data <- flow_data[,c(as.character(substitute(flow_stations)),
                            as.character(substitute(flow_dates)),
                            as.character(substitute(flow_values)))]
  colnames(flow_data) <- c("STATION_NUMBER","Date","Value")
  
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

  ## Check for multiple stations in flow_data and filter for select station if required
  if(is.null(plot_station) & length(unique(flow_data$STATION_NUMBER)) > 1) 
    stop("Cannot plot multiple stations. Have just one station in the flow_data/HYDAT or identify the station using the 'plot_station' argument.")
  if(!is.null(plot_station)){
    if(!plot_station %in% unique(flow_data$STATION_NUMBER)) 
      stop("Station in plot_station argument is not listed in flow_data stations. Select one listed in flow_data.")
    flow_data <- dplyr::filter(flow_data, STATION_NUMBER==plot_station)
  }

  
  ## CALC STATS
  ## ----------
  
  longterm_stats <- fasstr::calc_longterm_stats(flow_data=flow_data,
                                                flow_dates=Date,
                                                flow_values=Value,
                                                flow_stations=STATION_NUMBER,
                                                HYDAT=NULL,
                                                water_year=water_year, 
                                                water_year_start=water_year_start,
                                                start_year=start_year,
                                                end_year=end_year,
                                                exclude_years=exclude_years, 
                                                percentiles=c(5,25,75,95),
                                                ignore_missing = TRUE)
  


  
  
  ## PLOT STATS
  ## ----------

  longterm_stats_months <- dplyr::filter(longterm_stats, Month != "Long-term")
  longterm_stats_longterm <- dplyr::filter(longterm_stats, Month == "Long-term")

  longterm_plot <- ggplot2::ggplot(longterm_stats_months, ggplot2::aes(group = 1)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = Minimum, ymax = Maximum, fill = "Max-Min Range")) +
    ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = P5, ymax = P95, fill = "5-95 Percentiles")) +
    ggplot2::geom_ribbon(ggplot2::aes(x = Month, ymin = P25, ymax = P75, fill = "25-75 Percentiles")) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = longterm_stats_longterm$Mean, colour = "Long-term Mean"), size = 0.6, linetype = 2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = longterm_stats_longterm$Median, colour = "Long-term Median"), size = 0.6, linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x = Month, y = Mean, color = "Monthly Mean"), size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(x = Month, y = Median, color = "Monthly Median"), size = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = Month, y = Mean, color = "Monthly Mean"), size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = Month, y = Median, color = "Monthly Median"), size = 2) +
    ggplot2::scale_color_manual(values = c("Monthly Mean" = "skyblue2", "Monthly Median" = "dodgerblue4",
                                           "Long-term Mean" = "forestgreen", "Long-term Median" = "darkorchid4")) +
    ggplot2::scale_fill_manual(values = c("25-75 Percentiles" = "lightblue4", "5-95 Percentiles" = "lightblue3",
                                          "Max-Min Range" = "lightblue2")) +
    {if(log_discharge) ggplot2::scale_y_log10(expand = c(0,0))} +
    {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0))} +
    {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(0.07, "cm"),
                                                     mid = ggplot2::unit(0.15, "cm"), long = ggplot2::unit(0.2, "cm"))} +
    ggplot2::scale_x_discrete(expand = c(0.01,0.01)) +
    ggplot2::ylab("Discharge (cms)") +
    ggplot2::xlab(NULL) +
    ggplot2::theme(legend.position = "right",
                   legend.title = ggplot2::element_blank(),
                   legend.spacing = ggplot2::unit(0, "cm"),
                   legend.justification = "top",
                   panel.border = ggplot2::element_rect(colour = "grey80", fill = NA, size = 0.1),
                   plot.title = ggplot2::element_text(size = 12, colour = "grey25", face ="italic"),
                   panel.grid = ggplot2::element_line(size = 0.2),
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linetype = c(2,2,1,1), shape = c(NA,NA,16,16))))
 
  
  
  longterm_plot
  
  
  } 
