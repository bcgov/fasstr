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

#' @title Plot cumulative monthly flow statistics
#'
#' @description Plot the monthly cumulative mean, median, maximum, minimum, and 5, 25, 75, 95th percentiles for each month of the year 
#'    from a streamflow dataset. Plots the statistics from all monthly cumulative values from all years, unless specified. 
#'    Data calculated using calc_monthly_cumulative_stats() function. Can plot individual years for comparision using the 
#'    include_year argument. Defaults to volumetric cumulative flows, can use \code{use_yield} and \code{basin_area} to convert to 
#'    runoff yield.
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
#' @param groups Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and groups column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param use_yield Logical value indicating whether to use yield runoff, in mm, instead of volumetric. Default \code{FALSE}.
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each grouping factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
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
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default \code{FALSE}.
#' @param include_year Integer of a year to include cumulative data from. Leave blank for no years.
#'    
#' @return A ggplot2 object of monthly cumulative flows with the following plots
#'   \item{Mean}{monthly cumulative mean}
#'   \item{Median}{monthly cumulative median}
#'   \item{Min-5 Percentile Range}{a ribbon showing the range of data between the monthly cumulative minimum and 5th percentile}
#'   \item{5-25 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 5th and 25th percentiles}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 25th and 75th percentiles}
#'   \item{75-95 Percentiles Range}{a ribbon showing the range of data between the monthly cumulative 75th and 95th percentiles}
#'   \item{95 Percentile-Max Range}{a ribbon showing the range of data between the monthly cumulative 95th percentile and the maximum}
#'   \item{'Year' Flows}{(optional) the monthly cumulative flows for the designated year}
#'   
#' @examples
#' \dontrun{
#' 
#'plot_monthly_cumulative_stats(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



plot_monthly_cumulative_stats <- function(data = NULL,
                                          dates = Date,
                                          values = Value,
                                          groups = STATION_NUMBER,
                                          use_yield = FALSE, 
                                          basin_area = NA,
                                          water_year = FALSE,
                                          water_year_start = 10,
                                          start_year = 0,
                                          end_year = 9999,
                                          exclude_years = NULL, 
                                          complete_years = FALSE,
                                          ignore_missing = TRUE,
                                          log_discharge=FALSE,
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
    data <- data[,c(as.character(substitute(groups)),
                    as.character(substitute(dates)),
                    as.character(substitute(values)))]
    colnames(data) <- c("STATION_NUMBER", "Date", "Value")
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
  
  monthly_stats <- calc_monthly_cumulative_stats(data = data,
                                                 percentiles = c(5,25,75,95),
                                                 use_yield = use_yield, 
                                                 basin_area = basin_area,
                                                 water_year = water_year,
                                                 water_year_start = water_year_start,
                                                 start_year = start_year,
                                                 end_year = end_year,
                                                 exclude_years = exclude_years, 
                                                 complete_years = complete_years,
                                                 ignore_missing = ignore_missing)
  
  
  ## PLOT STATS
  ## ----------

  monthly_stats$Month <- match(monthly_stats$Month, month.abb)
  
  # Create the daily stats plots
  monthly_stats_plot <- ggplot2::ggplot(monthly_stats, ggplot2::aes(x = Month)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = P5, fill = "Min-5th Percentile")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P5, ymax = P25, fill = "5th-25th Percentile")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P25, ymax = P75, fill = "25th-75th Percentile")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P75, ymax = P95, fill = "75th-95th Percentile")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = P95, ymax = Maximum, fill = "95th Percentile-Max")) +
    ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = 1) +
    ggplot2::scale_fill_manual(values = c("Min-5th Percentile" = "orange" , "5th-25th Percentile" = "yellow",
                                          "25th-75th Percentile" = "skyblue1", "75th-95th Percentile" = "dodgerblue2",
                                          "95th Percentile-Max" = "royalblue4")) +
    ggplot2::scale_color_manual(values = c("Median" = "purple3", "Mean" = "springgreen4")) +
    {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0))} +
    {if(log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))} +
    {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                     short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                     long = ggplot2::unit(.2, "cm"))} +
    ggplot2::xlab("Month")+
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb[1:12], expand = c(0,0)) +
    {if(!use_yield) ggplot2::ylab("Cumulative Discharge (cubic metres)")} +
    {if(use_yield) ggplot2::ylab("Cumulative Runoff Yield (mm)")} +
    ggplot2::theme_bw() +
    ggplot2::labs(color = 'Monthly Statistics', fill = "Monthly Ranges") +
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

    # Add cumulative flows
    if (use_yield){
      flow_data <- add_cumulative_yield(data = flow_data, water_year = water_year, water_year_start = water_year_start, basin_area = basin_area)
      flow_data$Cumul_Flow <- flow_data$Cumul_Yield_mm
    } else {
      flow_data <- add_cumulative_volume(data = flow_data, water_year = water_year, water_year_start = water_year_start)
      flow_data$Cumul_Flow <- flow_data$Cumul_Volume_m3
    }

    if (water_year) {
      flow_data$AnalysisYear <- flow_data$WaterYear
    }  else {
      flow_data$AnalysisYear <- flow_data$Year
    }
    
    flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
    flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))

    if(!include_year %in% flow_data$AnalysisYear) stop(paste0("Year in include_year does not exist. Please choose a year between ",
                                                              min(flow_data$AnalysisYear), " and ", max(flow_data$AnalysisYear), "."))
    flow_data <- dplyr::filter(flow_data, AnalysisYear == include_year)
    
    monthly_data <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, Month),
                                     Monthly_Total = max(Cumul_Flow, na.rm = FALSE))

    # Plot the year
    suppressMessages(
      monthly_stats_plot <- monthly_stats_plot +
        ggplot2::geom_line(data = monthly_data, ggplot2::aes(x = Month, y = Monthly_Total, colour = "yr.colour"), size = 1) +
        ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                    labels = c("Mean", "Median", paste0(include_year, " Flows")))
    )
  }


  monthly_stats_plot
  
}

