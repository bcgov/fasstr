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

#' @title Calculate annual summary and missing data statistics
#'
#' @description Calculates mean, median, maximum, minimum, standard deviation of annual flows and data availability and missing data
#'    statistics for each year and month of each year. Calculates the statistics from all daily discharge values from all years, 
#'    unless specified.
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
#' @param groups Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and groups column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param rolling_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param rolling_align Character string identifying the direction of the rolling mean from the specified date, either by the first ('left'), last
#'    ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param transpose Logical value indicating if the results rows and columns are to be switched. Default \code{FALSE}.
#'
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{n_days}{number of days per year}
#'   \item{n_Q}{number of days per year with flow data}
#'   \item{n_missing_Q}{number of days per year with no flow data}
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{Median}{annual median of all daily flows for a given year}
#'   \item{StandardDeviation}{annual 1 standard deviation of all daily flows for a given year}
#'   and the following monthly missing columns (order will depend on water_year_month):
#'   \item{Jan_missing_Q}{number of Jan days per year with no flow data}
#'   \item{Feb_missing_Q}{number of Feb days per year with no flow data}
#'   \item{Mar_missing_Q}{number of Mar days per year with no flow data}
#'   \item{Apr_missing_Q}{number of Apr days per year with no flow data}
#'   \item{May_missing_Q}{number of May days per year with no flow data}
#'   \item{Jun_missing_Q}{number of Jun days per year with no flow data}
#'   \item{Jul_missing_Q}{number of Jul days per year with no flow data}
#'   \item{Aug_missing_Q}{number of Aug days per year with no flow data}
#'   \item{Sep_missing_Q}{number of Sep days per year with no flow data}
#'   \item{Oct_missing_Q}{number of Oct days per year with no flow data}
#'   \item{Nov_missing_Q}{number of Nov days per year with no flow data}
#'   \item{Dec_missing_Q}{number of Dec days per year with no flow data}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'screen_flow_data(data = flow_data)
#' 
#'screen_flow_data(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



screen_flow_data <- function(data = NULL,
                             dates = Date,
                             values = Value,
                             groups = STATION_NUMBER,
                             rolling_days = 1,
                             rolling_align = "right",
                             water_year = FALSE,
                             water_year_start = 10,
                             start_year = 0,
                             end_year = 9999,
                             transpose = FALSE){             
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(is.vector(data)) {
    if(!all(data %in% dplyr::pull(tidyhydat::allstations[1]))) 
      stop("One or more stations numbers listed in data argument do not exist in HYDAT. Re-check numbers or provide a data frame of data.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
  } else {
    flow_data <- data
  }
  if(!is.data.frame(flow_data)) stop("Incorrect selection for data argument, must provide a data frame or HYDAT station number(s).")
  flow_data <- as.data.frame(flow_data) # Getting random 'Unknown or uninitialised column:' warnings if using tibble
  
  # Save the original columns (to check for groups column later) and ungroup
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # If no groups (default STATION_NUMBER) in data, make it so (required)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # Get the just groups (default STATION_NUMBER), Date, and Value columns
  # This method allows the user to select the Station, Date or Value columns if the column names are different
  if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
    stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
  
  # Gather required columns (will temporarily rename groups column as STATION_NUMBER if isn't already)
  flow_data <- flow_data[,c(as.character(substitute(groups)),
                            as.character(substitute(dates)),
                            as.character(substitute(values)))]
  colnames(flow_data) <- c("STATION_NUMBER","Date","Value")
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in provided data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in provided data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.numeric(rolling_days))                        stop("rolling_days argument must be numeric")
  if(!all(rolling_days %in% c(1:180)))                 stop("rolling_days argument must be integers > 0 and <= 180)")
  if(!rolling_align %in% c("right", "left", "center")) stop("rolling_align argument must be 'right', 'left', or 'center'")
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  if(length(start_year)>1)        stop("Only one start_year value can be listed")
  if(!start_year %in% c(0:9999))  stop("start_year must be an integer.")
  if(length(end_year)>1)          stop("Only one end_year value can be listed")
  if(!end_year %in% c(0:9999))    stop("end_year must be an integer.")
  if(start_year > end_year)       stop("start_year must be less than or equal to end_year.")
  
  if(!is.logical(transpose))       stop("transpose argument must be logical (TRUE/FALSE).")
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_rolling_means(data = flow_data, days = rolling_days, align = rolling_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)

  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate basic stats
  Q_summary <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                n_days = length(AnalysisYear),
                                n_Q = sum (!is.na(RollingValue)),
                                n_missing_Q = sum (is.na(RollingValue)),
                                Minimum = ifelse(n_Q == 0, NA, min(RollingValue, na.rm = TRUE)),
                                Maximum = ifelse(n_Q == 0, NA, max(RollingValue,na.rm = TRUE)),
                                Mean = ifelse(n_Q == 0, NA, mean(RollingValue,na.rm = TRUE)),
                                Median = stats::median(RollingValue, na.rm = TRUE),
                                StandardDeviation = stats::sd(RollingValue, na.rm = TRUE))
  
  # Calculate for each month for each year
  Q_summary_month <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear, MonthName),
                                        n_missing_Q = sum(is.na(RollingValue)))
  Q_summary_month <- dplyr::rename(Q_summary_month, Month = MonthName)
  Q_summary_month <- dplyr::mutate(Q_summary_month, Month = paste0(Month, "_missing_Q"))
  
  
  if (water_year) {
    if (water_year_start == 1) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q"))
    } else if (water_year_start == 2) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", 
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q"))
    } else if (water_year_start == 3) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q"))
    } else if (water_year_start == 4) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
    } else if (water_year_start == 5) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q", 
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
    } else if (water_year_start == 6) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q", 
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
    } else if (water_year_start == 7) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q"))
    } else if (water_year_start == 8) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q"))
    } else if (water_year_start == 9) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q"))
    } else if (water_year_start == 10) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", 
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q"))
    } else if (water_year_start == 11) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", 
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q"))
    } else if (water_year_start == 12) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q"))
    }
  } else {           
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", 
                                                                    "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                    "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                    "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q"))
  }
  
  
  Q_summary_month <- tidyr::spread(Q_summary_month, Month, n_missing_Q)
  
  Q_summary <- merge(Q_summary, Q_summary_month, by = c("STATION_NUMBER","AnalysisYear"), all = TRUE)
  Q_summary <- dplyr::rename(Q_summary, Year = AnalysisYear)
  row_order <- names(Q_summary[, -1])
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(Q_summary[-(1:2)])
    
    # Transpose the columns for rows
    Q_summary <- tidyr::gather(Q_summary, Statistic, Value, -STATION_NUMBER, -Year)
    Q_summary <- tidyr::spread(Q_summary, Year, Value)
    
    # Order the columns
    Q_summary$Statistic <- as.factor(Q_summary$Statistic)
    levels(Q_summary$Statistic) <- stat_levels
    Q_summary <- with(Q_summary, Q_summary[order(STATION_NUMBER, Statistic),])
  }
  # 
  # if(transpose){
  #   Q_summary_tpose <- tidyr::gather(Q_summary,Statistic,Value,-Year)
  #   Q_summary_tpose_temp <- dplyr::mutate(Q_summary_tpose,Value=round(Value,write_digits)) # for writing to csv
  #   Q_summary <- tidyr::spread(Q_summary_tpose,Year,Value)
  #   Q_summary <- Q_summary_[match(row_order, Q_summary_$Statistic),]
  # }
  # 
  
  
  
  dplyr::as_tibble(Q_summary)
  
}

