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


#' @title Compute annual trends.
#'
#' @description Computes annual trends.
#'
#' @param trendsdata Dataframe. Annual data with column names of years and rows of annual statistics.
#'@param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
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
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded.years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(write_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_transposed_table Logical. Should a file be created with the transposed of the annual statistics
#'    (both calendar and water year)?
#'    The file name will be  \code{file.path(write_dir,paste(station_name,'-annual-summary-stat-trans.csv'))}.
#' @param write_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' Coming
#' 
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the trends analysis

fasstr_annual_trends_plots <- function(flowdata=NULL,
                                 HYDAT=NULL,
                                 trendsdata=NULL,
                                 zyp_method=NA,
                                 zyp_alpha=0.05,
                                 station_name=NA,
                                 water_year=FALSE,
                                 water_year_start=10,
                                 start_year=NULL,
                                 end_year=NULL,
                                 exclude_years=NULL, 
                                 basin_area=NA,
                                 lowflow_days=c(1,3,7,30),
                                 totalflow_seasons=TRUE,
                                 timing_percent=c(25,33,50,75),
                                 write_plots=FALSE,      
                                 write_imgtype="pdf",  
                                 write_dir=".",
                                 na.rm=list(na.rm.global=FALSE)){             
  
  
  
  
  #  Some basic error checking on the input parameters

  # if trendsdata is provided
  if( !is.null(trendsdata) ) {
    if( !is.data.frame(trendsdata))         {
      stop("trendsdata parameter is not a dataframe.")}

  # if no trendsdata is provided, but flowdata is
  } else {
    if( is.null(flowdata) & is.null(HYDAT)) {
      stop("If no trendsdata provided, one of flowdata or HYDAT parameters must be set.")}
    if( !is.null(HYDAT) & !is.null(flowdata))  {
      stop("If no trendsdata provided, one of flowdata or HYDAT parameters must be set, not both.")}
    if( !is.null(HYDAT) & length(HYDAT)>1 )        {
      stop("Only one HYDAT station can be selected.")}
    if( is.null(HYDAT) & !is.data.frame(flowdata))         {
      stop("flowdata parameter is not a dataframe.")}
    if( is.null(HYDAT) & !("Date" %in% names(flowdata)) ){
      stop("flowdata dataframe doesn't contain 'Date' column.")}
    if( is.null(HYDAT) & !("Value" %in% names(flowdata)) ){
      stop("flowdata dataframe doesn't contain a flow column labeled 'Value'.")}
    if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
      stop("Date column in flowdata dataframe is not a date.")}
    if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
      stop("Flow data ('Value') column in flowdata dataframe is not numeric.")}
    if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {warning('flowdata cannot have negative values - check your data')}
    
    
    if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
    
    if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
    if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
    if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
    
    if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
    
    if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
    
    if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
    if( length(basin_area)>1 )        {stop("basin_area parameter cannot have length > 1")}
  }
  
  if( is.na(zyp_method) | !zyp_method %in% c("yuepilon","zhang") )   {
    stop('zyp_trending parameter must have either "yuepilon" or "zhang" listed')}
  
    if( length(station_name)>1 )        {stop("station_name parameter cannot have length > 1")}
  
  if( !is.logical(write_plots))  {
    stop("write_plots argument must be logical (TRUE/FALSE)")}
  if( length(write_imgtype)>1)        {
    stop("write_imgtype argument cannot have length > 1")}
  if( !is.na(write_imgtype) & !write_imgtype %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("write_imgtype argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}

  if( !dir.exists(as.character(write_dir)))      {stop("directory for saved files does not exist")}

  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  if (!is.null(HYDAT)) {
    if( is.na(station_name) ) {station_name <- HYDAT}
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
                                                timing_percent=timing_percent,
                                                transpose=TRUE,
                                                na.rm=na.rm)
  }
  
  
  
  trends_results <- fasstr::fasstr_annual_trends_analysis(trendsdata=trends_data,
                                                          flowdata=NULL,
                                                          HYDAT=NULL,
                                                          zyp_method=zyp_method#,
                                                          #station_name=station_name,
                                                          #water_year=water_year,
                                                          #water_year_start=water_year_start,
                                                          #start_year=start_year,
                                                          #end_year=end_year,
                                                          #exclude_years=exclude_years, 
                                                          #basin_area=basin_area,
                                                          #write_trends_data=FALSE,      
                                                          #write_trends_results=FALSE,
                                                          #write_dir=".",
                                                          #na.rm=list(na.rm.global=FALSE),
                                                          #table_nddigits=3
  )
  
  
  # Create the list to place all plots
  trends_plots <- list()
  
  if (write_plots) {
    if (write_imgtype=="pdf"){
      file_trends_plot <-file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-plots.pdf", sep=""))
      pdf(file = file_trends_plot,8.5,4)
    }
    if (write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      file_trends_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-plots",sep = "")
      dir.create(file_trends_plot)
    }
  }
  
  # Add each annaul data to the plot
  trends_data <- tidyr::gather(trends_data,Year,Value,-1)
  trends_data <- dplyr::mutate(trends_data,Year=as.numeric(Year))
  if ( is.null(trendsdata) ) {
    trends_data <- dplyr::mutate(trends_data,
                                 Units="Discharge (cms)",
                                 Units=replace(Units, grepl("Yield_mm",Statistic), "Runoff Yield (mm)"),
                                 Units=replace(Units, grepl("TotalQ_m3",Statistic), "Total Discharge (cubic metres)"),
                                 Units=replace(Units, grepl("DoY",Statistic), "Day of Year"),
                                 Units=replace(Units, grepl("Days",Statistic), "Number of Days"))
  }
  
  for (stat in unique(trends_results$Statistic)){
    # Filter for metric
    trends_data_stat <- dplyr::filter(trends_data,Statistic==stat)
    trends_results_stat <- dplyr::filter(trends_results,Statistic==stat)
    int <- trends_results_stat$intercept - trends_results_stat$trend * (min(trends_data_stat$Year))
    # Plot each metric
    trends_plot <- ggplot2::ggplot(trends_data_stat,ggplot2::aes(x=Year,y=Value))+
      ggplot2::geom_point()+  
      ggplot2::geom_line(alpha = 0.3) +
      ggplot2::ggtitle(paste0(stat,"   (Sig. = ",round(trends_results_stat$sig,3),")"))+
      ggplot2::xlab("Year")+
      {if (is.null(trendsdata)) ggplot2::ylab(trends_data_stat$Units)}+
      {if (!is.null(trendsdata)) ggplot2::ylab("Units")}+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                     panel.grid = ggplot2::element_line(size=.2))
    # If sig. trend, plot trend
    if (trends_results_stat$sig < zyp_alpha & !is.na(trends_results_stat$sig)) {
      trends_plot <- trends_plot+
        ggplot2::geom_abline(slope = trends_results_stat$trend, intercept = int, colour="red")
    }
    
    trends_plots[[paste0(stat,"-trends")]] <- trends_plot
    
    
    if (write_plots & write_imgtype=="pdf") {
      plot(trends_plot)
    }
    
    # Save the plots if the png,jpeg,tiff,or bmp images are selected
    if (write_plots & write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      file_trendstat_plot <- paste(file_trends_plot,"/",stat,"-annual-trends.",write_imgtype,sep = "")
      ggplot2::ggsave(filename =file_trendstat_plot,trends_plot,width=8.5,height=4)
    }
    
    
  }
  
  # End the PDF device if selected
  if (write_plots & write_imgtype=="pdf") {
    dev.off()
  }
  
  
  return(trends_plots)
} # end of function

