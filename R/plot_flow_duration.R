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



#' @title Plot flow duration curves
#'
#' @description Plots flow duration curves, percent time a flow value is equalled or exceeded, for a streamflow dataset. Plots 
#'    statistics from all daily discharge values from all years, unless specified. Data calculated using calc_longterm_stats() 
#'    function then converted for plotting.
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
#' @param custom_months Numeric vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    If wanting months that overlap calendar years (ex. Oct-Mar), choose water_year and a water_year_month that begins before the first 
#'    month listed. Leave blank for no custom month summary.
#' @param custom_months_label Character string to use as a label of custom months. For example, if choosing months 7:9  you may choose 
#'    "Summer" or "Jul-Sep". Default \code{"Custom-Months"}.
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#' @param incl_months Numeric vector of month curvesto plot. NA if no months required. Default \code{1:12}.
#' @param incl_longterm Logical value indicating whether to include longterm curve of all data. Default \code{TRUE}.
#'
#' @return A ggplot2 object with plots for each month, long-term, and custom months showing the percentage of time that 
#'    flows are likely equal or exceeded for each time period.
#'   
#' @examples
#' \dontrun{
#' 
#'plot_flow_duration(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



plot_flow_duration <- function(data = NULL,
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
                               custom_months = NULL,
                               custom_months_label = "Custom-Months",
                               ignore_missing = TRUE,
                               incl_months = 1:12,
                               incl_longterm = TRUE,
                               log_discharge = TRUE){
  
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
  
  if(!is.logical(log_discharge))  stop("log_discharge argument must be logical (TRUE/FALSE).")
  
  
  ## CALC STATS
  ## ----------
  
  percentiles_data <- calc_longterm_stats(data = data,
                                          percentiles = c(.01,.1,.2:9.8,10:90,90.2:99.8,99.9,99.99),
                                          rolling_days = rolling_days,
                                          rolling_align = rolling_align,
                                          water_year = water_year,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years,
                                          complete_years = complete_years,
                                          custom_months = custom_months,
                                          ignore_missing = ignore_missing)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(percentiles_data)) {
    percentiles_data <- dplyr::ungroup(percentiles_data)
    percentiles_data <- dplyr::select(percentiles_data, -STATION_NUMBER)
  }
  
  # Setup and calculate the probabilites
  percentiles_data <- dplyr::select(percentiles_data, -Mean, -Median, -Maximum, -Minimum)
  percentiles_data <- tidyr::gather(percentiles_data, Percentile, Value, -1)
  percentiles_data <- dplyr::mutate(percentiles_data, Percentile = 100 - as.numeric(gsub("P", "", Percentile)))
  
  # Filter for months and longterm selected to plot
  include <- month.abb[incl_months]
  if (incl_longterm) { include <- c(include, "Long-term") }
  if (!is.null(custom_months)) { include <- c(include, "Custom-Months") }
  percentiles_data <- dplyr::filter(percentiles_data, Month %in% include)
  
  
  ## PLOT STATS
  ## ----------
  
  ggplot2::ggplot(percentiles_data, ggplot2::aes(x = Percentile, y = Value, colour = Month))+
    ggplot2::geom_line()+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0, 0))}+
    ggplot2::scale_x_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("% Time flow equalled or exceeded")+
    ggplot2::scale_color_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                           "Apr" = "forestgreen", "May" = "limegreen", "Jun" = "gold", "Jul" = "orange", 
                                           "Aug" = "red", "Sep" = "darkred", "Oct" = "orchid", "Nov" = "purple3",
                                           "Dec" = "midnightblue", "Long-term" = "black", "Custom-Months" = "grey60")) +
    ggplot2:: annotation_logticks(sides = "l", base = 10, colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"),
                                  mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))+
    ggplot2::labs(color = 'Time Periods') +  
    ggplot2::theme_bw()+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   legend.justification = "top",
                   axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                   axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                   legend.text = ggplot2::element_text(size = 9, colour = "grey25"))
  
  
  
}
