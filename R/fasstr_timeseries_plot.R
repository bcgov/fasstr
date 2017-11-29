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

#' @title Plots the time series record.
#'
#' @description Plots the time series record.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param station_name Character. Identifier name of the stream or station. Required when supplying data through \code{flowdata}.
#'    The station name will be used in plots and filenames of exported tables and plot. If using \code{HYDAT} to supply
#'    data and no \code{station_name} is provided, the HYDAT station number will be the identifier.
#' @param water_year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water_year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).   
#' @param percentiles Numeric. List of numbers to calculate percentiles (5 = 5th percentile). Default c(5,25,75,95).
#' @param rolling_days Numeric. Rolling days. Default 1.
#' @param rolling_align Character. Specifies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_plot Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param plot_type Character. pdf, png, bmp, jpeg, tiff. Default pdf.
#' @param plot_title Character. Text string of desired title for all plots. Default NA.
#' @param plot_by_year Logical. Plot each year of data individually. Default FALSE.
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default FALSE (linear).
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#'
#'
#' @examples
#' \dontrun{
#' 
#' coming soon :)
#' }
#' @export

#'


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

fasstr_timeseries_plot <- function(flowdata=NULL,
                                     HYDAT=NULL,
                                     station_name="fasstr",
                                     water_year=FALSE, #create another for own water year????
                                     water_year_start=10,
                                     start_year=NULL,
                                     end_year=NULL,
                                     exclude_years=NULL, # list of stations
                                     rolling_days=1,
                                     rolling_align="right",
                                     write_plot=FALSE,        # write out statistics on calendar year
                                     plot_type="pdf",        # write out statistics on calendar year
                                     plot_by_year=FALSE,
                                     log_discharge=FALSE,
                                     plot_title=NA,
                                     report_dir="."){
  
  
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {
    stop("station_name argument must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {
    stop("station_name argument cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
    stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  
  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("rolling_align argument must be 'right', 'left', or 'center'.")}
  
  if( !is.logical(water_year))  {
    stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {
    stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  
  if( !is.logical(log_discharge))  {
    stop("log_discharge argument must be logical (TRUE/FALSE)")}
  
  if( !is.logical(write_plot))  {
    stop("write_plot argument must be logical (TRUE/FALSE)")}
  if( length(plot_type)>1)        {
    stop("plot_type argument cannot have length > 1")}
  if( !is.na(plot_type) & !plot_type %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("plot_type argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  if( !is.logical(plot_by_year))  {
    stop("plot_by_year argument must be logical (TRUE/FALSE)")}
  
  
  if( !dir.exists(as.character(report_dir)))      {
    stop("directory for saved files does not exist")}

  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' argument does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata,water_year_start=water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start=water_year_start)
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
  }
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))

  
  timeseries_plot <- ggplot2::ggplot(data=flowdata, ggplot2::aes(x=Date, y=RollingValue))+
    #ggtitle("Mean Daily Discharge")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::geom_line(colour="dodgerblue4")+
    ggplot2::ylab("Discharge (cms)")+
    {if (plot_by_year) ggplot2::facet_wrap(~AnalysisYear, scales="free_x")} +
    {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8),expand = c(0, 0))}+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (plot_by_year) ggplot2::scale_x_date(date_labels = "%b")} +
    {if (!plot_by_year) ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 12))} +
    ggplot2::theme( panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.5),
           panel.grid.minor.y = ggplot2::element_blank())
  
  
  if (write_plot) {
    file_timeseries_plot <- paste(report_dir,"/",station_name,
                                  ifelse(plot_by_year,paste0("-annual-daily-timeseries."),paste0("-longterm-daily-timeseries.")),
                                  plot_type,sep = "")
    ggplot2::ggsave(filename = file_timeseries_plot,
                    timeseries_plot,
                    height=ifelse(plot_by_year,11,6.35),
                    width = 18)
  }
  

  
  
  
  
  return(timeseries_plot)
  
  
} # end of function
