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

#' @title Flow duration plots
#'
#' @description Compute a long-term percentiles table.
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
#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
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

fasstr_flow_duration_curves <- function(flowdata=NULL,
                                       HYDAT=NULL,
                                       station_name="fasstr",
                                       water_year=FALSE, #create another for own water year????
                                       water_year_start=10,
                                       start_year=NULL,
                                       end_year=NULL,
                                       exclude_years=NULL, # list of stations
                                       #months="all",
                                       percentiles=c(1:99),
                                       log_discharge=TRUE,
                                       write_plot=FALSE,        # write out statistics on calendar year
                                       plot_type="pdf",        # write out statistics on calendar year
                                       report_dir=".",
                                       na.rm=list(na.rm.global=FALSE)){
  
  
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {
    stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {
    stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
    stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {
    stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {
    stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.logical(log_discharge))  {
    stop("log_discharge argument must be logical (TRUE/FALSE)")}
  
  if( !is.logical(write_plot))  {
    stop("write_plot parameter must be logical (TRUE/FALSE)")}
  if( length(plot_type)>1)        {
    stop("plot_type argument cannot have length > 1")}
  if( !is.na(plot_type) & !plot_type %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("plot_type argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  
  if( !dir.exists(as.character(report_dir)))      {
    stop("directory for saved files does not exist")}
  
  if( !is.list(na.rm))              {
    stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   
    stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){
    stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  
  percentiles_data <- fasstr_longterm_percentiles(flowdata=flowdata,
                                             HYDAT=HYDAT,
                                             station_name=station_name,
                                             water_year=water_year, #create another for own water year????
                                             water_year_start=water_year_start,
                                             start_year=start_year,
                                             end_year=end_year,
                                             exclude_years=exclude_years, # list of stations
                                             percentiles=c(1:99),
                                             transpose=FALSE,
                                             write_table=FALSE,        # write out statistics on calendar year
                                             report_dir=report_dir,
                                             na.rm=list(na.rm.global=FALSE),
                                             table_nddigits=3)
  percentiles_data <- tidyr::gather(percentiles_data,Percentile,Value,-1)
  percentiles_data <- dplyr::mutate(percentiles_data,Percentile=100-as.numeric(gsub("P", "", Percentile)))
  
  flow_duration_plot <- ggplot2::ggplot(percentiles_data,ggplot2::aes(x=Percentile,y=Value,colour=Month))+
    ggplot2::geom_line()+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0, 0))}+
    ggplot2::scale_x_continuous(expand =c(0,0),breaks = scales::pretty_breaks(n = 10))+
    #ggtitle(paste0("Flow Duration Curves - ",Stream_Name," (",yeartype.years.label,")"))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("% Time flow equalled or exceeded")+
    ggplot2::scale_color_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise","Apr" = "forestgreen", "May" = "limegreen","Jun" = "gold","Jul" = "orange", "Aug" = "red","Sep" = "darkred", "Oct" = "orchid", "Nov" = "purple3","Dec" = "midnightblue","Long-term" = "black"))+
    ggplot2:: annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))+
    ggplot2::theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
      panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
      panel.grid = ggplot2::element_line(size=.2),
      legend.title = ggplot2::element_blank(),
      legend.justification = "top")
  
  
  
  #  Write out summary tables for calendar years
  if (write_plot) {
    file_flowduration_plot <- paste(report_dir,"/",station_name,"-flow_duration_curves.",plot_type,sep = "")
    ggplot2::ggsave(filename = file_flowduration_plot,
                    flow_duration_plot,
                    height= 6,
                    width = 11)
  }
  
  
  return(flow_duration_plot)
  
  
} # end of function
