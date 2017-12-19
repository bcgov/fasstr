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

#' @title Write a streamflow dataset as a .csv
#'
#' @description Write a streamflow dataset as a .csv file to a directory. Can add missing dates or filter data by years before writing
#'    using given arguments. Just list flowdata data frame or HYDAT station number to write its entirety.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na Character. String to use for missing values in the data. Default \code{""} (blank).
#' 
#' @return A .csv file of streamflow data in a selected directory.
#'
#' @examples
#' \dontrun{
#' 
#'write_results(flowdata = flowdata, station_name = "MissionCreek", na="")
#' 
#'write_results(HYDAT = "08NM116")
#' 
#' }
#' @export

#--------------------------------------------------------------

write_results <- function(data=NULL,
                          station_name="fasstr",
                          write_dir=".",
                          write_digits=4,
                          na=""){  
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
  if(is.null(data))         stop("one of flowdata or HYDAT arguments must be set")
  if(!is.data.frame(data))  stop("flowdata arguments is not a data frame")
  
  if(!is.na(station_name) & !is.character(station_name))  stop("station_name argument must be a character string.")
  

  
  #Write the file
  write.csv(flowdata,file = paste0(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-daily-flows.csv"),
            row.names = F)
}

