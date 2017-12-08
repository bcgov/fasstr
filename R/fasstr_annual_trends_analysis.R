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



#' @title Calculate prewhitened nonlinear trends on streamflow data
#'
#' @description Calculates prewhitened nonlinear trends on annual streamflow data. Uses the
#'    \href{https://cran.r-project.org/web/packages/zyp/index.html}{'zyp'} package to trend. Review 'zyp' to understand its methology.
#'    Use \code{trendsdata} if providing your own data frame of statistics to trend. If using \code{HYDAT} or \code{flowdata}, then all
#'    annual statistics will be calculated using the fasstr_annual_all_stats() function which uses the following fasstr functions:
#' \itemize{
#'  \item{fasstr_annual_stats()}
#'  \item{fasstr_annual_lowflows()}
#'  \item{fasstr_annual_total_flows()}
#'  \item{fasstr_annual_flow_timing()}
#'  \item{fasstr_monthly_stats()}
#'  \item{fasstr_annual_days_outside_normal()}
#'  }
#' 
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param trendsdata Data frame. Annual data with column names of years and rows of annual statistics. Leave blank if using \code{HYDAT}
#' @param zyp_method Character. The prewhitened trend method to use from 'zyp', either "zhang' or "yuepilon". Required.
#' @param basin_area Numeric. Upstream drainage basin area of the hydrometric station, in sq. km. Leave blank if \code{HYDAT} is used or 
#'    a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the basin area will be extracted from HYDAT. 
#'    Setting the basin area will replace the HYDAT basin area. 
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.
#' @param annual_percentiles Numeric. Vector of percentiles to calculate annually. Set to NA if none required. Used for fasstr_annual_stats()
#'    function. Default \code{c(10,90)}.
#' @param monthly_percentiles Numeric. Vector of percentiles to calculate monthly for each year. Set to NA if none required. Used for 
#'    fasstr_monthly_stats() function. Default \code{c(10,90)}.
#' @param lowflow_days  Numeric. The number of days to apply a rolling mean. Used for fasstr_annual_lowflows() function. Default \code{1}.
#' @param lowflow_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Used for fasstr_annual_lowflows() function. Default \code{'right'}.
#' @param totalflow_seasons Logical. Include seasonal yields and total discharges.Used for fasstr_annual_total_flows() function. 
#'    Default \code{TRUE}.
#' @param timing_percent Numeric. Percents of annual total flows to determine dates. Used for fasstr_annual_flow_timing() function. 
#'    Default \code{c(25,33.3,50,75)}.
#' @param normal_percentiles Numeric. Lower and upper percentiles, respectively indicating the limits of the normal range. 
#'    Used for fasstr_annual_days_outside_normal() function. Default \code{c(25,75)}.
#' @param transpose Logical. Switch the rows and columns of the results table. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table_data Logical. Write the trending input data table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_table_results Logical. Write the trending results table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @return A data frame containing trends with the following outputs from the zyp package:
#'   \item{Statistic}{the annual statistic used for trending.}
#'   \item{lbound}{the lower bound of the trend's confidence interval}
#'   \item{trend}{the Sens' slope (trend) per unit time}
#'   \item{trendp}{the Sen's slope (trend) over the time period}
#'   \item{ubound}{the upper bound of the trend's confidence interval}
#'   \item{tau}{Kendall's tau statistic computed on the final detrended timeseries}
#'   \item{sig}{Kendall's P-value computed for the final detrended timeseries}
#'   \item{nruns}{the number of runs required to converge upon a trend}
#'   \item{autocor}{the autocorrelation of the final detrended timeseries}
#'   \item{valid_frac}{the fraction of the data which is valid (not NA) once autocorrelation is removed}
#'   \item{linear}{the least squares fit trend on the same dat}
#'   \item{intercept}{the lower bound of the trend's confidence interval}
#'   \item{lbound}{the intercept of the Sen's slope (trend)}
#'   and the following additional columns:
#'   \item{min_year}{the minimum year used in the trending}
#'   \item{max_year}{the maximum year used in the trending}
#'   \item{n_years}{the number of years with data for trending}
#'   \item{mean}{the mean of all values used for trending}
#'   \item{median}{the median of all values used for trending}
#'   \item{min}{the minimum of all values used for trending}
#'   \item{max}{the maximum of all values used for trending}
#'   Transposing data creates a column of "Trends_Statistics" and subsequent columns of each trends statistic for each annual statistic.
#'   
#' @examples
#' \dontrun{
#' 
#'fasstr_annual_trends_analysis(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_annual_trends_analysis(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------


fasstr_annual_trends_analysis <- function(flowdata=NULL,
                                          HYDAT=NULL,
                                          trendsdata=NULL,
                                          zyp_method=NA,
                                          basin_area=NA, 
                                          water_year=FALSE,
                                          water_year_start=10,
                                          start_year=NULL,
                                          end_year=NULL,
                                          exclude_years=NULL,
                                          annual_percentiles=c(10,90),
                                          monthly_percentiles=c(10,20),
                                          lowflow_days=c(1,3,7,30),
                                          lowflow_align="right",
                                          totalflow_seasons=TRUE,
                                          timing_percent=c(25,33,50,75),
                                          normal_percentiles=c(25,75),
                                          transpose=FALSE,
                                          station_name=NA,
                                          write_table_data=FALSE,      
                                          write_table_results=FALSE, 
                                          write_dir=".",
                                          na.rm=list(na.rm.global=FALSE)){       
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
  
  if( is.null(trendsdata) ){
    if( !is.null(HYDAT) & !is.null(flowdata))           {stop("must select either flowdata or HYDAT arguments, not both, if no trendsdata")}
    if( is.null(HYDAT)) {
      if( is.null(flowdata))                            {stop("one of flowdata or HYDAT arguments must be set, if no trendsdata")}
      if( !is.data.frame(flowdata))                     {stop("flowdata is not a data frame")}
      if( !all(c("Date","Value") %in% names(flowdata))) {stop("flowdata data frame doesn't contain columns 'Date' and 'Value'")}
      if( !inherits(flowdata$Date[1], "Date"))          {stop("'Date' column in flowdata data frame is not a date")}
      if( !is.numeric(flowdata$Value))                  {stop("'Value' column in flowdata data frame is not numeric")}
      if( any(flowdata$Value <0, na.rm=TRUE))           {warning('flowdata cannot have negative values - check your data')}
    }
  }
  
  if( !is.logical(water_year))         {stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) )  {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1)      {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( length(start_year)>1)   {stop("only one start_year value can be selected")}
  if( !is.null(start_year) )  {if( !start_year %in% c(0:5000) )  {stop("start_year must be an integer")}}
  if( length(end_year)>1)     {stop("only one end_year value can be selected")}
  if( !is.null(end_year) )    {if( !end_year %in% c(0:5000) )  {stop("end_year must be an integer")}}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("list of exclude_years must be numeric - ex. 1999 or c(1999,2000)")}
  
  if( !is.na(basin_area) & !is.numeric(basin_area)) {stop("basin_area argument must be numeric")}
  if( length(basin_area)>1)                         {stop("basin_area argument cannot have length > 1")}
  
  if( !is.numeric(lowflow_days))                       {stop("lowflow_days argument must be numeric")}
  if( !all(lowflow_days %in% c(1:180)) )               {stop("lowflow_days argument must be integers > 0 and <= 180)")}
  if( !lowflow_align %in% c("right","left","center"))  {stop("lowflow_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.logical(totalflow_seasons))  {stop("totalflow_seasons argument must be logical (TRUE/FALSE)")}
  
  if( !is.numeric(timing_percent) )                 {stop("timing_percent must be numeric")}
  if( !all(timing_percent>0 & timing_percent<100))  {stop("timing_percent must be >0 and <100)")}
  
  if( !all(is.na(annual_percentiles)) & !is.numeric(annual_percentiles) )                 {stop("annual_percentiles argument must be numeric")}
  if( !all(is.na(annual_percentiles)) & (!all(annual_percentiles>0 & annual_percentiles<100)) )  {stop("annual_percentiles must be >0 and <100)")}
  if( !all(is.na(monthly_percentiles)) & !is.numeric(monthly_percentiles) )                 {stop("monthly_percentiles argument must be numeric")}
  if( !all(is.na(monthly_percentiles)) & (!all(monthly_percentiles>0 & monthly_percentiles<100)) )  {stop("monthly_percentiles must be >0 and <100)")}
  
  if( !is.numeric(normal_percentiles) )                {stop("normal_percentiles must be numeric")}
  if( length(normal_percentiles)!=2 )                  {stop("normal_percentiles must be two percentile values (ex. c(25,75))")}
  if( normal_percentiles[1] >= normal_percentiles[2] ) {stop("normal_percentiles[1] must be < normal_percentiles[2]")}
  if( !all(is.na(normal_percentiles)) & (!all(normal_percentiles>0 & normal_percentiles<100)) )  {stop("normal_percentiles must be >0 and <100)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  if( length(station_name)>1 )        {stop("station_name parameter cannot have length > 1")}
  
  if( is.na(zyp_method) | !zyp_method %in% c("yuepilon","zhang") )   {stop('zyp_trending aregument must be either "yuepilon" or "zhang"')}
  
  if( !is.logical(write_table_data))  {stop("write_table_data parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table_results)){stop("write_table_results parameter must be logical (TRUE/FALSE)")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  
  if( (!is.null(HYDAT) | !is.null(flowdata)) & !is.null(trendsdata))  {stop("only trendsdata or either HYDAT or flowdata can be provided")}
  if( is.null(HYDAT) & is.null(flowdata) ) {
    if( is.null(trendsdata))                                          {stop("trendsdata or either HYDAT or flowdata must be provided")}
    if( !is.data.frame(trendsdata))                                   {stop("trendsdata arguments is not a data frame")}
  }
  
  
  trends_data <- trendsdata
  
  # If no trendsata is provided, use flowdata ot HYDAT
  if ( is.null(trendsdata) ) {
    trends_data <- fasstr::fasstr_annual_all_stats(flowdata=flowdata,
                                                   HYDAT = HYDAT,
                                                   station_name=station_name,
                                                   water_year=water_year,
                                                   water_year_start=water_year_start,
                                                   start_year=start_year,
                                                   end_year=end_year,
                                                   exclude_years=exclude_years,
                                                   basin_area=basin_area,
                                                   lowflow_days=lowflow_days,
                                                   totalflow_seasons=totalflow_seasons,
                                                   timing_percent = timing_percent,
                                                   transpose=TRUE,
                                                   na.rm=na.rm)
  }
  
  # Compute some summary stats on the input data
  colnames(trends_data)[1] <- "Statistic"
  trends_data_summary <- tidyr::gather(trends_data,Year,Value,2:ncol(trends_data))
  trends_data_summary <- dplyr::summarise(dplyr::group_by(trends_data_summary,Statistic),
                                          min_year=min(Year),
                                          max_year=max(Year),
                                          n_years= sum(!is.na(Value)),
                                          mean=mean(Value, na.rm = T),
                                          median=median(Value,na.rm = T),
                                          min=min(Value, na.rm = T),
                                          max=max(Value, na.rm = T))
  
  # Complete trends analysis
  trends_results <- zyp::zyp.trend.dataframe(indat = trends_data,
                                             metadata.cols = 1,
                                             method=zyp_method)
  
  # Merge the summary stats with the results
  trends_results <- merge(trends_results,trends_data_summary, by="Statistic",all=TRUE)
  # merge all inputdata at end for dataframe? (incl_data=TRUE)
  
  
  # Transpose data if selected
  if(transpose){
    col_names <- names(trends_results[-1])
    trends_results <- tidyr::gather(trends_results,Trends_Statistic,Value,-Statistic)
    trends_results <- tidyr::spread(trends_results,Statistic,Value)
    trends_results <- trends_results[match(col_names, trends_results$Trends_Statistic),]
    row.names(trends_results) <- c(1:nrow(trends_results))
  }
  
  # Write the data if selected
  if(write_table_data){
    file_trends_data <-file.path(write_dir,paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-data.csv",sep=""))
    temp <- trends_data
    utils::write.csv(temp, file=file_trends_data, row.names=FALSE)
  }
  
  if(write_table_results){
    file_trends_results <-file.path(write_dir,paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-results.csv",sep=""))
    utils::write.csv(trends_results, file=file_trends_results, row.names=FALSE)
  }
  
  
  
  
  
  
  return(trends_results)
} # end of function

