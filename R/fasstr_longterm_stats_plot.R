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

#' @title Compute long-term statistics.
#'
#' @description Computes long-term statistics of streamflow data.
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
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param write_dir Character. Folder location of where to write tables and plots. Default is the working directory.
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

fasstr_longterm_stats_plot <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  station_name=NA,
                                  water_year=FALSE,
                                  water_year_start=10,
                                  start_year=NULL,
                                  end_year=NULL,
                                  exclude_years=NULL,
                                  write_plot=FALSE,
                                  log_discharge=FALSE,
                                  write_imgtype="pdf",
                                  write_dir=".",
                                  na.rm=list(na.rm.global=FALSE)){
  
  
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
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
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(write_plot))  {
    stop("write_plot argument must be logical (TRUE/FALSE)")}
  if( length(write_imgtype)>1)        {
    stop("write_imgtype argument cannot have length > 1")}
  if( !is.na(write_imgtype) & !write_imgtype %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("write_imgtype argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  
  if( !dir.exists(as.character(write_dir)))      {
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
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  
  longterm_stats <- fasstr::fasstr_longterm_stats(flowdata=flowdata,
                                                  HYDAT=HYDAT,
                                                  station_name=station_name,
                                                  water_year=water_year, #create another for own water year????
                                                  water_year_start=water_year_start,
                                                  start_year=start_year,
                                                  end_year=end_year,
                                                  exclude_years=exclude_years, # list of stations
                                                  percentiles=c(5,25,75,95),
                                                  transpose=FALSE,
                                                  write_table=FALSE,        # write out statistics on calendar year
                                                  write_dir=".",
                                                  na.rm=list(na.rm.global=FALSE),
                                                  write_digits=3)
  
  
  longterm_stats_months <- dplyr::filter(longterm_stats,Month!="Long-term")
  longterm_stats_longterm <- dplyr::filter(longterm_stats,Month=="Long-term")
  
  longterm_plot <- ggplot2::ggplot(longterm_stats_months,ggplot2::aes(group = 1))+
    ggplot2::geom_ribbon(ggplot2::aes(x=Month,ymin=Minimum,ymax=Maximum,fill="Max-Min Range"))+
    ggplot2::geom_ribbon(ggplot2::aes(x=Month,ymin=P5,ymax=P95,fill="5-95 Percentiles"))+
    ggplot2::geom_ribbon(ggplot2::aes(x=Month,ymin=P25,ymax=P75,fill="25-75 Percentiles"))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=longterm_stats_longterm$Mean, colour="Long-term Mean"),size=.6, linetype=2)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=longterm_stats_longterm$Median, colour="Long-term Median"),size=.6, linetype=2)+
    ggplot2::geom_line(ggplot2::aes(x=Month,y=Mean,color="Monthly Mean"), size=.6)+
    ggplot2::geom_line(ggplot2::aes(x=Month,y=Median,color="Monthly Median"), size=.6)+
    ggplot2::geom_point(ggplot2::aes(x=Month,y=Mean,color="Monthly Mean"), size=2)+
    ggplot2::geom_point(ggplot2::aes(x=Month,y=Median,color="Monthly Median"), size=2)+
    ggplot2::scale_color_manual(values = c("Monthly Mean"="skyblue2","Monthly Median"="dodgerblue4","Long-term Mean"="forestgreen","Long-term Median"="darkorchid4"))+
    ggplot2::scale_fill_manual(values = c("25-75 Percentiles"="lightblue4","5-95 Percentiles"="lightblue3","Max-Min Range"="lightblue2"))+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0))}+
    {if (log_discharge) ggplot2::annotation_logticks(base= 10,"l",colour = "grey25",size=0.3,short =  ggplot2::unit(.07, "cm"), mid =  ggplot2::unit(.15, "cm"), long =  ggplot2::unit(.2, "cm"))}+
    ggplot2::scale_x_discrete(expand = c(.01, .01))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab(NULL)+
    ggplot2::theme(legend.position = "right", 
                   legend.title =  ggplot2::element_blank(),
                   legend.spacing = ggplot2::unit(0, "cm"),
                   legend.justification = "top",
                   panel.border =  ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
                   plot.title =  ggplot2::element_text(size=12, colour = "grey25",face="italic"),
                   panel.grid =  ggplot2::element_line(size=.2),
                   panel.grid.major.x  =  ggplot2::element_blank())+
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes = list(linetype=c(2,2,1,1), shape=c(NA,NA,16,16))))

  
  
  #  Write out summary tables for calendar years
  if (write_plot) {
    file_longterm_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-longterm-statistics.",write_imgtype,sep = "")
    ggplot2::ggsave(filename = file_longterm_plot,
                    plot=longterm_plot,
                    height= 5,
                    width = 11)
  }
  
  
  return(longterm_plot)
  
  
} # end of function
