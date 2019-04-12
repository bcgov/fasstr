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

#' @title Write a data frame as a .xlsx, .xls, or .csv file
#'
#' @description Write a data frame to a directory with all numbers rounded to specified digits. Can write as .xls, .xlsx, or .csv 
#'    file types. Writing as .xlsx or .xls uses the 'writexl' package.
#'
#' @param data Data frame to be written to a directory.
#' @param file_name Character string naming the output file. Required.
#' @param digits Integer indicating the number of decimal places or significant digits used to round flow values. Use follows 
#'    that of base::round() digits argument.
#'
#' @examples
#' \dontrun{
#' 
#' # Example data to write
#' data_results <- calc_longterm_stats(station_number = c("08HA002", "08HA011"),
#'                                     start_year = 1971, end_year = 2000)
#' 
#' # Write the data and round numbers to 1 decimal place
#' write_results(data = data_results, 
#'               file_name = "Cowichan River Long-term Flows (1971-2000).xlsx", 
#'               digits = 1)
#' }
#' @export



write_results <- function(data,
                          file_name,
                          digits){  
  
  
  
  ## CHECKS ON DATA
  ## --------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(file_name)) {
    file_name = ""
  }
  if (missing(digits)) {
    digits = 10
  }
  
  
  if(is.null(data))         stop("data must be provided.", call. = FALSE)
  if(!is.data.frame(data))  stop("data must be a data frame.", call. = FALSE)
  
  if(file_name == "") stop("file_name name must be provided, ending with either .xlsx, .xls, or .csv.", call. = FALSE)
  
  filetype <- sub('.*\\.', '', file_name)
  if(!filetype %in% c("xlsx", "xls", "csv")) stop("file_name name must end with .xlsx, .xls, or .csv.", call. = FALSE)
  
  if(length(digits) != 1) stop("Only one number can be provided to digits.", call. = FALSE)
  if(!is.numeric(digits)) stop("digits must be a numeric value.", call. = FALSE)  
  
  
  # Round any numeric column to the specified digits
  numeric_cols <- sapply(data, is.numeric) 
  data[numeric_cols] <- lapply(data[numeric_cols], round, digits = digits) 
  
  
  ## WRITE FLOW DATA
  ## ---------------
  
  message(paste0("* writing '", file_name, "'"))
  
  if(filetype == "csv") {
    utils::write.csv(data, file = file_name, row.names = FALSE, na = "")
    message(paste0("* DONE. For file go to: '", normalizePath(file_name), "'"))
  } else {
    invisible(openxlsx::write.xlsx(data, file = file_name))
    message(paste0("* DONE. For file go to: '", normalizePath(file_name), "'"))
  }
  
}

