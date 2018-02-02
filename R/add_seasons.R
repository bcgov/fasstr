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

#' @title Add two and four seasons identifiers
#' 
#' @description Add columns of Seasons2 (two 6-month seasons) and Seasons4 (four 3-month seasons) to a data frame with a column of dates 
#'    called 'Date'. The start of the two and four seasons coincides with the start month of each year ('Jan-Jun' and 'Jan-Mar' for 
#'    the first two and four calendar year seasons, respectively, or 'Oct-Mar' and 'Apr-Sep' for the first two and four water year
#'    seasons starting in Oct, respectively). Each season is designated by the calendar or water year which it occurs.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates.
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' 
#' @return A data frame of the source data with additional columns:
#'   \item{Seasons2}{two seasons identifiers}
#'   \item{Seasons4}{four seasons identifiers}
#'
#' @examples
#' \dontrun{
#' 
#'add_seasons(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_seasons <- function(data = NULL,
                        dates = Date,
                        water_year = FALSE,
                        water_year_start = 10){
  
  
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
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Get groups of flow_data to return after
  flow_data_groups <- dplyr::group_vars(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Get the Date column set up
  # This method allows the user to select the Date column if the column names are different
  if(!as.character(substitute(dates)) %in% names(flow_data))  
    stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
  # Temporarily rename the Date column
  names(flow_data)[names(flow_data) == as.character(substitute(dates))] <- "Date"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  
  
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  
  
  
  # Add 2 and 4 seasons to each year, with the start of the first season on the first month of the year
  
  # Calendar Year seasons
  if (!water_year) {
    
    # Create seasons variables
    flow_data <- dplyr::mutate(flow_data, Seasons4 = ifelse(Month <= 3, "Jan-Mar",
                                                            ifelse(Month %in% 4:6, "Apr-Jun",
                                                                   ifelse(Month %in% 7:9, "Jul-Sep", "Oct-Dec"))))
    flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month <= 6, "Jan-Jun", "Jul-Dec"))
    
    
    # Apply levels to seasons
    flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Jan-Jun", "Jul-Dec"))
    flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
  }
  
  
  # Water Year seasons
  if (water_year) {
    
    # Create seasons variables
    if (water_year_start %in% c(1, 4, 7, 10)) { 
      flow_data <- dplyr::mutate(flow_data, Seasons4 = ifelse(Month <= 3, "Jan-Mar",
                                                              ifelse(Month %in% 4:6, "Apr-Jun",
                                                                     ifelse(Month %in% 7:9, "Jul-Sep", "Oct-Dec"))))
      if (water_year_start %in% c(1,7)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month <= 6, "Jan-Jun", "Jul-Dec"))
      } else if (water_year_start %in% c(4,10)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month %in% 4:9, "Apr-Sep", "Oct-Mar"))
      }
    } else if (water_year_start %in% c(2, 5, 8, 11)) { 
      flow_data <- dplyr::mutate(flow_data, Seasons4 = ifelse(Month %in% c(1, 11:12), "Nov-Jan",
                                                              ifelse(Month %in% 2:4, "Feb-Apr",
                                                                     ifelse(Month %in% 5:7, "May-Jul", "Aug-Oct"))))
      if (water_year_start %in% c(2,8)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month %in% 2:7, "Feb-Jul", "Aug-Jan"))
      } else if (water_year_start %in% c(5,11)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month %in% c(5:10), "May-Oct", "Nov-Apr"))
      }
    } else if (water_year_start %in% c(3, 6, 9, 12)) { 
      flow_data <- dplyr::mutate(flow_data, Seasons4 = ifelse(Month %in% c(12, 1:2), "Dec-Feb",
                                                              ifelse(Month %in% 3:5, "Mar-May",
                                                                     ifelse(Month %in% 6:8, "Jun-Aug", "Sep-Nov"))))
      if (water_year_start %in% c(3,9)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month %in% 3:8, "Mar-Aug", "Sep-Apr"))
      } else if (water_year_start %in% c(6,12)) {
        flow_data <- dplyr::mutate(flow_data, Seasons2 = ifelse(Month %in% c(6:11), "Jun-Nov", "Dec-May"))
      }
      
    }
    
    # Apply levels to seasons
    if (water_year_start==1) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Jan-Jun", "Jul-Dec"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
    } else if (water_year_start==2) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Feb-Jul", "Aug-Jan"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Feb-Apr", "May-Jul", "Aug-Oct", "Nov-Jan"))
    } else if (water_year_start==3) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Mar-Aug", "Sep-Apr"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Mar-May", "Jun-Aug", "Sep-Nov", "Dec-Feb"))
    } else if (water_year_start==4) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Apr-Sep", "Oct-Mar"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Apr-Jun", "Jul-Sep", "Oct-Dec", "Jan-Mar"))
    } else if (water_year_start==5) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("May-Oct", "Nov-Apr"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("May-Jul", "Aug-Oct", "Nov-Jan", "Feb-Apr"))
    } else if (water_year_start==6) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Jun-Nov", "Dec-May"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Jun-Aug", "Sep-Nov", "Dec-Feb", "Mar-May"))
    } else if (water_year_start==7) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Jul-Dec", "Jan-Jun"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Jul-Sep", "Oct-Dec", "Jan-Mar", "Apr-Jun"))
    } else if (water_year_start==8) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Aug-Jan", "Feb-Jul"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Aug-Oct", "Nov-Jan", "Feb-Apr", "May-Jul"))
    } else if (water_year_start==9) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Sep-Apr", "Mar-Aug"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Sep-Nov", "Dec-Feb", "Mar-May", "Jun-Aug"))
    } else if (water_year_start==10) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Oct-Mar", "Apr-Sep"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Oct-Dec", "Jan-Mar","Apr-Jun", "Jul-Sep"))
    } else if (water_year_start==11) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Nov-Apr", "May-Oct"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Nov-Jan", "Feb-Apr", "May-Jul", "Aug-Oct"))
    } else if (water_year_start==12) {
      flow_data$Seasons2 <- factor(flow_data$Seasons2, levels = c("Dec-May", "Jun-Nov"))
      flow_data$Seasons4 <- factor(flow_data$Seasons4, levels = c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov"))
    }
  }
  
  # Return the original names of the Date column
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  
  
  # Return columns to original order plus new column
  if(all(c("Seasons2", "Seasons4") %in% orig_cols)){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, "Seasons2", "Seasons4")]
  }
  
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(flow_data_groups))
  
  
  dplyr::as_tibble(flow_data)

  
}
