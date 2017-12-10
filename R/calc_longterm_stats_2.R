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

#' @title Calculate the long-term and long-term monthly summary statistics
#'
#' @description Calculates the long-term and long-term monthly mean, median, maximum, minimum, and percentiles of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @param flow_data Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flow_data} argument is used.
#' @param percentiles Numeric. Vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param custom_months Integer. Vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    Leave blank for no custom month summary.
#' @param custom_months_label Character. Label of custom months. For example, if choosing months 7:9  you may choose "Summer" or "Jul-Sep".
#'    Default \code{"Custom-Months"}.
#' @param transpose Logical. Switch the rows and columns of the results table. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flow_data} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @return A data frame with the following columns:
#'   \item{Month}{month of the year, included Long-term for all months, and Custom-Months if selected}
#'   \item{Mean}{mean of all daily flows for a given month and longterm over all years}
#'   \item{Median}{median of all daily flows for a given month and longterm over all years}
#'   \item{Maximum}{maximum of all daily flows for a given month and longterm over all years}
#'   \item{Minimum}{minimum of all daily flows for a given month and longterm over all years}
#'   \item{P'n'}{each  n-th percentile selected for a given month and longterm over all years}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile selected for a given month and longterm over all years}
#'   \item{P90}{annual 90th percentile selected for a given month and longterm over all years}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_longterm_stats(flow_data = flow_data, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_longterm_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_longterm_stats(HYDAT = "08NM116", custom_months = c(5:9))
#'
#' }
#' @export

#--------------------------------------------------------------


calc_longterm_stats_2 <- function(flow_data=NULL,
                                  flow_dates=Date,
                                  flow_values=Value,
                                  HYDAT=NULL,
                                  percentiles=c(10,90),
                                  water_year=FALSE,
                                  water_year_start=10,
                                  start_year=0,
                                  end_year=9999,
                                  exclude_years=NULL,
                                  custom_months=NULL,
                                  custom_months_label="Custom-Months",
                                  transpose=FALSE,
                                  station_name=NA,
                                  write_table=FALSE,
                                  write_digits=3,
                                  write_dir=".",
                                  na.rm=list(na.rm.global=FALSE)){
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(flow_data) & is.null(HYDAT))   stop("No flow data provided, must use flow_data or HYDAT arguments.")
  if(!is.null(flow_data) & !is.null(HYDAT)) stop("Only one of flow_data or HYDAT arguments can be used.")
  
  # Get HYDAT data if selected and stations exist
  if(!is.null(HYDAT)) {
    if(!all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1]))) stop("One or more stations listed in 'HYDAT' do not exist.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
    if(is.na(station_name & length(HYDAT)==1)) {station_name <- HYDAT}
    if(is.na(station_name & length(HYDAT)>=1)) {station_name <- "fasstr"}
  }
  
  # If no STATION_NUMBER in flow_data, make it so (required for grouping)
  if(!"STATION_NUMBER" %in% colnames(flow_data)) {
    flow_data$STATION_NUMBER <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or name the column using 'flow_dates' argument.")
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or name the column using 'flow_values' argument.")
  
  # Gather required columns
  flow_data <- flow_data[,c("STATION_NUMBER",
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
  
  if(!is.null(custom_months) & !is.numeric(custom_months))             
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(custom_months %in% c(1:12)))                                 
    stop("custom_months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!is.na(custom_months_label) & !is.character(custom_months_label)) 
    stop("custom_months_label argument must be a character string.")
  
  if(!is.numeric(percentiles))               stop("percentiles argument must be numeric.")
  if(!all(percentiles>0 & percentiles<100))  stop("percentiles must be > 0 and < 100.")
  
  if(!is.na(station_name) & !is.character(station_name)) stop("station_name argument must be a character string.")
  
  if(!is.logical(transpose))     stop("transpose argument must be logical (TRUE/FALSE).")
  if(!is.logical(write_table))   stop("write_table argument must be logical (TRUE/FALSE).")
  if(!is.numeric(write_digits))  stop("write_digits argument must be be numeric.")
  write_digits <- round(write_digits[1])
  
  if(!dir.exists(as.character(write_dir))) {
    message("Directory for saved files does not exist, a new directory will be created.")
    if(write_table & write_dir!=".") {dir.create(write_dir)}
  }
  
  #### SORT ME OUUUUUUTTTTTT
  if( !is.list(na.rm))                        {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm)))             {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill in the missing dates and the add the date variables again
  flow_data <- fasstr::fill_missing_dates(flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- fasstr::add_date_variables(flow_data, water_year = water_year, water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flow_data$AnalysisYear <- flow_data$WaterYear
  }  else {
    flow_data$AnalysisYear <- flow_data$Year
  }
  
  # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate the monthly and longterm stats
  Q_months <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                               Mean = mean(Value, na.rm = TRUE),
                               Median = median(Value, na.rm = TRUE),
                               Maximum = max(Value, na.rm = TRUE),
                               Minimum = min(Value, na.rm = TRUE))
  Q_months <- dplyr::ungroup(Q_months)
  Q_longterm   <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER),
                                   Mean = mean(Value, na.rm = TRUE),
                                   Median = median(Value, na.rm = TRUE),
                                   Maximum = max(Value, na.rm = TRUE),
                                   Minimum = min(Value, na.rm = TRUE))
  Q_longterm <- dplyr::ungroup(Q_longterm)
  Q_longterm <- dplyr::mutate(Q_longterm, MonthName = as.factor("Long-term"))
  Q_longterm <- rbind(Q_months, Q_longterm)  
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      
      Q_months_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                                         Percentile = quantile(Value, ptile / 100, na.rm = TRUE))
      Q_months_ptile <- dplyr::ungroup(Q_months_ptile)
      Q_longterm_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                           Percentile = quantile(Value, ptile / 100, na.rm = TRUE))
      Q_longterm_ptile <- dplyr::ungroup(Q_longterm_ptile)
      Q_longterm_ptile <- dplyr::mutate(Q_longterm_ptile, MonthName = "Long-term")
      colnames(Q_months_ptile)[3] <- paste0("P", ptile)
      colnames(Q_longterm_ptile)[2] <- paste0("P", ptile)
      
      Q_longterm_ptile <- rbind(Q_months_ptile, Q_longterm_ptile)
      
      # Merge with Q_longterm
      Q_longterm <- merge(Q_longterm,Q_longterm_ptile,by=c("STATION_NUMBER", "MonthName"))
    }
  }
  
  # If custom_months are selected, append a row on the bottom
  if(is.numeric(custom_months) & all(custom_months %in% c(1:12))) {
    flow_data_temp <- dplyr::filter(flow_data, Month %in% custom_months)
    Q_months_custom <-   dplyr::summarize(dplyr::group_by(flow_data_temp,STATION_NUMBER),
                                          Mean = mean(Value, na.rm = TRUE),
                                          Median = median(Value, na.rm = TRUE),
                                          Maximum = max(Value,na.rm = TRUE),
                                          Minimum = min(Value,na.rm = TRUE))
    Q_months_custom <- dplyr::ungroup(Q_months_custom)
    Q_months_custom <- dplyr::mutate(Q_months_custom, MonthName = paste0(custom_months_label))
    
    if (!all(is.na(percentiles))){
      for (ptile in percentiles) {
        Q_ptile_custom <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                           Percentile = quantile(Value, ptile / 100, na.rm = TRUE))
        Q_ptile_custom <- dplyr::ungroup(Q_ptile_custom)
        Q_ptile_custom <- dplyr::mutate(Q_ptile_custom, MonthName = paste0(custom_months_label))
        colnames(Q_ptile_custom)[2] <- paste0("P",ptile)
        Q_months_custom <- merge(Q_months_custom, Q_ptile_custom, by = c("STATION_NUMBER", "MonthName"))
      }
    }
    Q_longterm <- rbind(Q_longterm, Q_months_custom)
  }
  
  # Rename Month column and reorder to proper levels (set in add_date_vars)
  Q_longterm <- dplyr::rename(Q_longterm, Month = MonthName)
  Q_longterm <- with(Q_longterm, Q_longterm[order(STATION_NUMBER, Month),])
  row.names(Q_longterm) <- c(1:nrow(Q_longterm))
  
  
  ## OTHER STUFF
  ## --------------------
  
  # Switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(Q_longterm[-(1:2)])
    
    # Transpose the columns for rows
    Q_longterm <- tidyr::gather(Q_longterm, Statistic, Value, -STATION_NUMBER, -Month)
    Q_longterm <- tidyr::spread(Q_longterm, Month, Value)
    
    # Order the columns
    Q_longterm$Statistic <- as.factor(Q_longterm$Statistic)
    levels(Q_longterm$Statistic) <- stat_levels
    Q_longterm <- with(Q_longterm, Q_longterm[order(STATION_NUMBER, Statistic),])
  }
  
  # Remove the STATION_NUMBER columns if one wasn't in flowdata originally
  if(all(flow_data$STATION_NUMBER=="XXXXXXX")) {
    Q_longterm <- dplyr::select(Q_longterm, -STATION_NUMBER)
  }
  
  #  Write out summary tables for calendar years
  if (write_table) {
    file_stat_csv <-file.path(write_dir, paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr")),
                                                "-longterm-summary-stat.csv"))
    temp <- Q_longterm
    if("STATION_NUMBER" %in% names(temp)) {
      temp[,3:ncol(temp)] <- round(temp[,3:ncol(temp)], write_digits)
    } else {
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], write_digits)
    }
    utils::write.csv(temp, file=file_stat_csv, row.names=FALSE)
  }
  
  
  Q_longterm
  
  
}
