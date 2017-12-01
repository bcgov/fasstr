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


#' @title Compute basic monthly summary statistics.
#'
#' @description Compute basic monthly summary statistics.
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
#' @param percentiles Numeric. List of monthly percentiles to calculate. Default c(10,20).
#' @param write_plot Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(write_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_imgtype Character. pdf, png, bmp, jpeg, tiff. Default pdf.
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default FALSE (linear).
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' 
#' Coming soon :)
#' 
#' 
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_monthly_stats_plots <- function(flowdata=NULL,
                                HYDAT=NULL,
                                station_name=NA,
                                water_year=FALSE,
                                water_year_start=10,
                                start_year=NULL,
                                end_year=NULL,
                                exclude_years=NULL, 
                                percentiles=c(10,20),
                                write_plots=FALSE,
                                write_imgtype="pdf",
                                log_discharge=FALSE,
                                write_dir=".",
                                na.rm=list(na.rm.global=FALSE)){
  
  #############################################################
  
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
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
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(percentiles))   {
    stop("percentiles must be numeric")}
  if( !all(percentiles>0 & percentiles<=100))  {
    stop("percentiles must be >0 and <=100)")}
  
  if( !is.logical(write_plots))  {stop("write_plots parameter must be logical (TRUE/FALSE)")}
  if( length(write_imgtype)>1)        {
    stop("write_imgtype argument cannot have length > 1")}
  if( !is.na(write_imgtype) & !write_imgtype %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("write_imgtype argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  
  if( !is.logical(log_discharge))  {stop("log_discharge must be logical (TRUE/FALSE)")}
  
  
  
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
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  
  monthly_data <- fasstr_monthly_stats(flowdata=flowdata,
                                   HYDAT=HYDAT,
                                   station_name=station_name,
                                   water_year=water_year,
                                   water_year_start=water_year_start,
                                   start_year=start_year,
                                   end_year=end_year,
                                   exclude_years=exclude_years, 
                                   percentiles=percentiles,
                                   spread=FALSE,
                                   transpose=FALSE,
                                   write_table=FALSE,
                                   write_dir=".",
                                   na.rm=list(na.rm.global=FALSE),
                                   write_digits=3)
  monthly_data <- tidyr::gather(monthly_data,Statistic,Value,-(1:2))
  
  if (write_plots) {
    file_stat_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-monthly-summary-statistics",sep = "")
    dir.create(file_stat_plot)
  }
  
  # Create the list to place all plots
  monthly_stats_plots <- list()
  
  for (stat in unique(monthly_data$Statistic)) {
    monthly_data_plot <- dplyr::filter(monthly_data,Statistic==stat)
    monthly_plot <- ggplot2::ggplot(data=monthly_data_plot, ggplot2::aes(x=Year, y=Value))+
      ggplot2::geom_line(ggplot2::aes(colour=Month), alpha=0.5)+
      ggplot2::geom_point(ggplot2::aes(colour=Month))+
      ggplot2::facet_wrap(~Month, scales="free_x")+
      ggplot2::ggtitle(paste0("Monthly ",stat," Flows"))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     panel.grid = ggplot2::element_line(size=.2),
                     panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1))+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
      {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))}+
      {if (log_discharge) ggplot2::scale_y_log10()}+
      {if (log_discharge) ggplot2::annotation_logticks(base= 10,"left",colour = "grey25",size=0.3,
                                                       short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
      ggplot2::ylab("Discharge (cms)")+
      ggplot2::guides(colour=FALSE)+
      ggplot2::scale_colour_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                              "Apr" = "forestgreen", "May" = "limegreen","Jun" = "gold",
                                              "Jul" = "orange", "Aug" = "red","Sep" = "darkred",
                                              "Oct" = "orchid", "Nov" = "purple3","Dec" = "midnightblue"))
    
    monthly_stats_plots[[paste0("Monthly_",stat)]] <- monthly_plot
    
    if (write_plots) {
      file_plot <- paste(file_stat_plot,"/","Monthly_",stat,".",write_imgtype,sep = "")
      ggplot2::ggsave(filename =file_plot,monthly_plot,width=8.5,height=5.5)
    }
  }
    
  
  return(monthly_stats_plots)
}

