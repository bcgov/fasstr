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

#' @title Write a streamflow dataset as a .xlsx, .xls, or .csv file
#'
#' @description Write a streamflow dataset as a .xlsx .xls, or .csv file to a directory. Can add missing dates or filter data by
#'     years before writing using given arguments. Just list data frame or HYDAT station number to write its entirety. Writing as 
#'     .xlsx or .xls uses the 'writexl' package.
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
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param start_date Date (YYYY-MM-DD) of first date to consider for plotting. Leave blank if all years are required.
#' @param end_date  Date (YYYY-MM-DD) of last date to consider for plotting. Leave blank if all years are required.
#' @param fill_missing Logical value indicating whether to fill dates with missing flow data with NA. Default \code{FALSE}.
#' @param file Character string naming the output file. Default filetype is .xlsx. Change to .csv using filtype argument.
#' @param digits Integer indicating the number of decimal places or significant digits used to round flow values. Use follows 
#'    that of base::round() digits argument.
#'
#' @examples
#' \dontrun{
#' 
#'write_flow_data(data = "08NM116", 
#'                file = "Mission_Creek_daily_flows.xlsx",
#'                fill_missing = TRUE)
#' 
#' }
#' @export



write_flow_data <- function(data = NULL,
                            dates = Date,
                            values = Value,
                            groups = STATION_NUMBER,
                            water_year = FALSE,
                            water_year_start = 10,
                            start_year = 0,
                            end_year = 9999,
                            start_date = "0000-01-01",
                            end_date = "3000-12-31",
                            fill_missing = FALSE,
                            file = "",
                            digits = 10){  
  
  
  
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
  # Temporarily rename the Date and Value columns
  names(flow_data)[names(flow_data) == as.character(substitute(groups))] <- "STATION_NUMBER"
  names(flow_data)[names(flow_data) == as.character(substitute(dates))] <- "Date"
  names(flow_data)[names(flow_data) == as.character(substitute(values))] <- "Value"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in provided data frame does not contain dates.")
  if(!is.numeric(flow_data$Value))          stop("'Value' column in provided data frame does not contain numeric values.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  if(length(start_year)>1)            stop("Only one start_year value can be listed")
  if(!start_year %in% c(0:9999))      stop("start_year must be an integer.")
  if(length(end_year)>1)              stop("Only one end_year value can be listed")
  if(!end_year %in% c(0:9999))        stop("end_year must be an integer.")
  if(start_year > end_year)           stop("start_year must be less than or equal to end_year.")
  
  if(class(try(as.Date(start_date))) == "try-error") stop("start_date must be a date formatted YYYY-MM-DD")
  if(class(try(as.Date(end_date))) == "try-error")   stop("end_date must be a date formatted YYYY-MM-DD")
  if(start_date >= end_date)                         stop("start_date must be less than end_date")
  
  if(!is.logical(fill_missing))            stop("fill_missing argument must be logical (TRUE/FALSE).")
  
  if(file == "") stop("file name must be provided, ending with either .xlsx, .xls, or .csv.")
  
  filetype <- sub('.*\\.', '', file)
  if(!filetype %in% c("xlsx", "xls", "csv")) stop("file name must end with .xlsx, .xls, or .csv.")
  
  if(length(digits) != 1) stop("Only one number can be provided to digits.")
  if(!is.numeric(digits)) stop("digits must be a numeric value.")
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  if(fill_missing) {
    flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  }
  
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  
  # Filter for specific dates, if selected
  flow_data <- dplyr::filter(flow_data, Date >= start_date)
  flow_data <- dplyr::filter(flow_data, Date <= end_date)
  
  # Round the values
  flow_data$Value <- round(flow_data$Value, digits = digits)
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  flow_data <-  dplyr::as_tibble(flow_data[,c(orig_cols)])
  
  
  ## WRITE FLOW DATA
  ## ---------------
  
  if(filetype == "csv") {
    utils::write.csv(flow_data, file = file, row.names = FALSE, na = "")
  } else {
    writexl::write_xlsx(flow_data, path = file)
  }
 
}

