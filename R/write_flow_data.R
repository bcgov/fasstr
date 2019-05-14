# Copyright 2019 Province of British Columbia
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
#' @description Write a streamflow dataset to a directory. Can fill missing dates or filter data by years or dates before writing using 
#'    given arguments. Just list data frame or HYDAT station number to write its entirety. Can write as .xls, .xlsx, or .csv file types.
#'    Writing as Excel file type uses the 'writexl' package.
#'
#' @inheritParams calc_annual_stats
#' @param start_year Numeric value of the first year of data to write. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year of data to write. Leave blank to use the last year of the source data.
#' @param start_date Date (YYYY-MM-DD) of first date of data to write. Leave blank if all dates required.
#' @param end_date  Date (YYYY-MM-DD) of last date of data to write. Leave blank if all dates required.
#' @param file_name Character string naming the output file. If none provided, a default file name (with .xlsx) is provided (see 
#'    "Successfully created" message when using function for file name).
#' @param fill_missing Logical value indicating whether to fill dates with missing flow data with NA. Default \code{FALSE}.
#' @param digits Integer indicating the number of decimal places or significant digits used to round flow values. Use follows 
#'    that of base::round() digits argument.
#'
#' @examples
#' \dontrun{
#' 
#' # Write data from a data frame
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' write_flow_data(data = flow_data, 
#'                 file_name = "Mission_Creek_daily_flows.xlsx")
#' 
#' # Write data directly from HYDAT
#' write_flow_data(station_number = "08NM116", 
#'                 file_name = "Mission_Creek_daily_flows.xlsx")
#' 
#' # Write data directly from HYDAT and fill missing dates with NA
#' write_flow_data(station_number = "08NM116", 
#'                 file_name = "Mission_Creek_daily_flows.xlsx",
#'                 fill_missing = TRUE)
#' }
#' @export



write_flow_data <- function(data,
                            dates = Date,
                            values = Value,
                            groups = STATION_NUMBER,
                            station_number,
                            water_year_start = 1,
                            start_year,
                            end_year,
                            start_date,
                            end_date,
                            file_name,
                            fill_missing = FALSE,
                            digits){  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }
  if (missing(start_date)) {
    start_date = "0000-01-01"
  }
  if (missing(end_date)) {
    end_date = "3000-12-31"
  }
  if (missing(digits)) {
    digits = 10
  }
  if (missing(file_name)) {
    file_name = ""
  }

  
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years = NULL)
  
  if (class(try(as.Date(start_date))) == "try-error") stop("start_date must be a date formatted YYYY-MM-DD.", call. = FALSE)
  if (class(try(as.Date(end_date))) == "try-error")   stop("end_date must be a date formatted YYYY-MM-DD.", call. = FALSE)
  if (start_date >= end_date)                         stop("start_date must be less than end_date.", call. = FALSE)
  
  if (!is.logical(fill_missing))            stop("fill_missing argument must be logical (TRUE/FALSE).", call. = FALSE)
  
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = FALSE)
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  if (fill_missing) {
    flow_data <- fill_missing_dates(data = flow_data, water_year_start = water_year_start)
  }
  
  flow_data <- add_date_variables(data = flow_data, water_year_start = water_year_start)
  

  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  
  # Filter for specific dates, if selected
  flow_data <- dplyr::filter(flow_data, Date >= start_date)
  flow_data <- dplyr::filter(flow_data, Date <= end_date)
  
  # Round the values
  flow_data$Value <- round(flow_data$Value, digits = digits)
  
  # Sort by station and date
  flow_data <- dplyr::arrange(flow_data, STATION_NUMBER, Date)
  
  # Get list of stations for writing (if data argument used)
  stns <- unique(flow_data$STATION_NUMBER)
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  flow_data <-  dplyr::as_tibble(flow_data[,c(orig_cols)])
  
  
  ## WRITE FLOW DATA
  ## ---------------
  
  # If no file_name name provided
  if (file_name == "") {#stop("file_name name must be provided, ending with either .xlsx, .xls, or .csv.", call. = FALSE)
    
    # If station_number used
    if (!is.null(station_number)) {
      if (length(station_number) == 1) {
        file_name <- paste0(station_number, "_daily_data.xlsx")
      } else {
        file_name <- paste0("HYDAT_daily_data.xlsx")
      }
      
      # If data used
    } else {
      
      if (length(stns) == 1 & stns != "XXXXXXX") {
        file_name <- paste0(stns, "_daily_data.xlsx")
      } else {
        file_name <- paste0("fasstr_daily_data.xlsx")
      }
      
    }
    
  }    
  
  # Checks on file_name name and digits
  filetype <- sub('.*\\.', '', file_name)
  if (!filetype %in% c("xlsx", "xls", "csv")) stop("file_name name must end with .xlsx, .xls, or .csv.", call. = FALSE)
  
  if (length(digits) != 1) stop("Only one number can be provided to digits.", call. = FALSE)
  if (!is.numeric(digits)) stop("digits must be a numeric value.", call. = FALSE)
  
  message(paste0("* writing '", file_name, "'"))
  
  # Write the data
  if(filetype == "csv") {
    utils::write.csv(flow_data, file = file_name, row.names = FALSE, na = "")
    message(paste0("* DONE. For file go to: '", normalizePath(file_name), "'"))
  } else {
    invisible(openxlsx::write.xlsx(flow_data, file = file_name))
    message(paste0("* DONE. For file go to: '", normalizePath(file_name), "'"))
  }
  
}

