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

#' @title Write a data frame as a .xlsx, .xls, or .csv file
#'
#' @description Write a data frame to a directory with all numbers rounded to specified digits. Can write as .xls, .xlsx, or .csv 
#'    file types. Writing as .xlsx or .xls uses the 'writexl' package.
#'
#' @param data Data frame to be written to a directory.
#' @param file Character string naming the output file. Required.
#' @param digits Integer indicating the number of decimal places or significant digits used to round flow values. Use follows 
#'    that of base::round() digits argument.
#'
#' @examples
#' \dontrun{
#' 
#' write_results(data = calc_longterm_stats(data = c("08HA002", "08HA011"),
#'                                          start_year = 1971, end_year = 2000), 
#'               file = "Cowichan River Long-term Flows (1971-2000).xlsx", 
#'               digits = 1)
#' 
#' }
#' @export



write_results <- function(data = NULL,
                          file = "",
                          digits = 10){  
  
  
  
  ## CHECKS ON DATA
  ## --------------
  
  if(is.null(data))         stop("data must be provided.", call. = FALSE)
  if(!is.data.frame(data))  stop("data must be a data frame.", call. = FALSE)
  
  if(file == "") stop("file name must be provided, ending with either .xlsx, .xls, or .csv.", call. = FALSE)
  
  filetype <- sub('.*\\.', '', file)
  if(!filetype %in% c("xlsx", "xls", "csv")) stop("file name must end with .xlsx, .xls, or .csv.", call. = FALSE)
  
  if(length(digits) != 1) stop("Only one number can be provided to digits.", call. = FALSE)
  if(!is.numeric(digits)) stop("digits must be a numeric value.", call. = FALSE)  
  
  
  # Round any numeric column to the specified digits
  numeric_cols <- sapply(data, is.numeric) 
  data[numeric_cols] <- lapply(data[numeric_cols], round, digits = digits) 
  
  
  ## WRITE FLOW DATA
  ## ---------------
  
  if(filetype == "csv") {
    utils::write.csv(data, file = file, row.names = FALSE, na = "")
    message(paste0("Successfully created ", file, "."))
  } else {
    invisible(writexl::write_xlsx(data, path = file))
    message(paste0("Successfully created ", file, "."))
    
  }

  
}

