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


#' @title Plot the annual low flow minimums and the day they occur.
#'
#' @description Plot the annual low flow minimums and the day they occur.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#' No transpose due to dates.
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
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param rolling_days Numeric. The rolling n-day mean. Default c(1,3,7,30).
#' @param rolling_align Character. Specifies whether the date of the means should be left- (first day) or right (last day)-aligned 
#'    or centered (middle day). Default right.
#' @param write_plot Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param plot_type Character. pdf, png, bmp, jpeg, tiff. Default pdf.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' Coming soon :)
#' 
#' 
#' }
#' @export

#'
#--------------------------------------------------------------

fasstr_annual_lowflows_plots <- function(flowdata=NULL,
                                         HYDAT=NULL,
                                         station_name="fasstr",
                                         water_year=FALSE,
                                         water_year_start=10,
                                         start_year=NULL,
                                         end_year=NULL,
                                         exclude_years=NULL,
                                         rolling_days=c(1,3,7,30),
                                         rolling_align="right",
                                         write_plot=FALSE,
                                         plot_type="pdf",      
                                         report_dir=".",
                                         na.rm=list(na.rm.global=FALSE)){
  
  #############################################################
  
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("align rolling_align must be 'right', 'left', or 'center'.")}
  
  if( !is.logical(write_plot))  {stop("write_plot parameter must be logical (TRUE/FALSE)")}
  if( length(plot_type)>1)        {
    stop("plot_type argument cannot have length > 1")}
  if( !is.na(plot_type) & !plot_type %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("plot_type argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exist")}
  
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
  }
  
  
  # COmpute the low flows data
  lowflows_data <- fasstr::fasstr_annual_lowflows(flowdata=flowdata,
                                                  HYDAT=HYDAT,
                                                  station_name=station_name,
                                                  water_year=water_year,
                                                  water_year_start=water_year_start,
                                                  start_year=start_year,
                                                  end_year=end_year,
                                                  exclude_years=exclude_years,
                                                  rolling_days=rolling_days,
                                                  rolling_align=rolling_align,
                                                  transpose=FALSE,
                                                  write_table=FALSE,
                                                  report_dir=".",
                                                  na.rm=list(na.rm.global=FALSE))
  
  
  # Gather data and plot the minimums day
  lowflows_DoY_data <- dplyr::select(lowflows_data,Year,dplyr::contains("DoY"))
  lowflows_DoY_data <- tidyr::gather(lowflows_DoY_data,Statistic,Value,-1)
  
  doy_plot <- ggplot2::ggplot(data=lowflows_DoY_data, ggplot2::aes(x=Year, y=Value))+
    ggplot2::geom_line(ggplot2::aes(colour=Statistic))+
    ggplot2::geom_point(ggplot2::aes(colour=Statistic))+
    ggplot2::facet_wrap(~Statistic, scales="free_x",ncol = 1, strip.position="right")+
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::ylab("Day of Year")+
    ggplot2::xlab("Year")+
    ggplot2::guides(colour=FALSE)+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
                   panel.grid = ggplot2::element_line(size=.2))
  
  
  # Gather data and plot the minimums values
  lowflows_value_data <- dplyr::select(lowflows_data,Year,dplyr::contains("Day"),
                                       -dplyr::contains("DoY"),
                                       -dplyr::contains("Date"))
  lowflows_value_data <- tidyr::gather(lowflows_value_data,Statistic,Value,-1)
  
  min_plot <- ggplot2::ggplot(data=lowflows_value_data, ggplot2::aes(x=Year, y=Value))+
    ggplot2::geom_line(ggplot2::aes(colour=Statistic))+
    ggplot2::geom_point(ggplot2::aes(colour=Statistic))+
    ggplot2::facet_wrap(~Statistic, scales="free_x",ncol = 1, strip.position="right")+
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("Year")+
    ggplot2::guides(colour=FALSE)+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
                   panel.grid = ggplot2::element_line(size=.2))
  
  
    # Save the plots if selected.
  if (write_plot) {
    file_doy_plot <- paste(report_dir,"/",station_name,"_annual_lowflow_minimums.",plot_type,sep = "")
    ggplot2::ggsave(filename =file_doy_plot,doy_plot,width=8.5,height=5)
    
    file_min_plot <- paste(report_dir,"/",station_name,"_annual_lowflow_minimum_days.",plot_type,sep = "")
    ggplot2::ggsave(filename =file_min_plot,min_plot,width=8.5,height=5)
  }
  
  
  return(list("Annual_Minimums"=min_plot,
              "Annual_Minimum_Days"=doy_plot))
}

