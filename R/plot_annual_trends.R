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


#' @title Plot prewhitened nonlinear trends on streamflow data
#'
#' @description Plots prewhitened nonlinear trends on annual streamflow data. Uses the
#'    \href{https://cran.r-project.org/web/packages/zyp/index.html}{'zyp'} package to trend. Review 'zyp' to understand its methology.
#'    Use \code{trendsdata} if providing your own data frame of statistics to trend. If using \code{HYDAT} or \code{flowdata}, then all
#'    annual statistics will be calculated using the calc_all_annual_stats() function which uses the following fasstr functions:
#' \itemize{
#'  \item{calc_annual_stats()}
#'  \item{calc_annual_lowflows()}
#'  \item{calc_annual_total_flows()}
#'  \item{calc_annual_flow_timing()}
#'  \item{calc_monthly_stats()}
#'  \item{calc_annual_outside_normal()}
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
#' @param zyp_alpha Numeric. Significance level of when to plot a trend line. Set to NA if no line required. Default \code{0.05}.
#' @param basin_area Numeric. Upstream drainage basin area of the hydrometric station, in sq. km. Leave blank if \code{HYDAT} is used or 
#'    a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the basin area will be extracted from HYDAT. 
#'    Setting the basin area will replace the HYDAT basin area. 
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.
#' @param annual_percentiles Numeric. Vector of percentiles to calculate annually. Set to NA if none required. Used for calc_annual_stats()
#'    function. Default \code{c(10,90)}.
#' @param monthly_percentiles Numeric. Vector of percentiles to calculate monthly for each year. Set to NA if none required. Used for 
#'    calc_monthly_stats() function. Default \code{c(10,90)}.
#' @param lowflow_days  Numeric. The number of days to apply a rolling mean. Used for calc_annual_lowflows() function. Default \code{1}.
#' @param lowflow_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Used for calc_annual_lowflows() function. Default \code{'right'}.
#' @param totalflow_seasons Logical. Include seasonal yields and total discharges.Used for calc_annual_total_flows() function. 
#'    Default \code{TRUE}.
#' @param timing_percent Numeric. Percents of annual total flows to determine dates. Used for calc_annual_flow_timing() function. 
#'    Default \code{c(25,33.3,50,75)}.
#' @param normal_percentiles Numeric. Lower and upper percentiles, respectively indicating the limits of the normal range. 
#'    Used for calc_annual_outside_normal() function. Default \code{c(25,75)}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(5,8.5)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @details If using \code{HYDAT} or \code{flowdata} then proper units will be displayed on each plot. If using \code{trendsdata} then the 
#'   units will be displayed as "Units". To label units on your plots when using \code{trendsdata}, create the plot and use 
#'   \code{+ggplot2::ylab()} to customize.
#' 
#' @return A list of ggplot2 objects with plots of trended annual statistics, as the name of each object, with a trend line
#'    plotted if less than the zyp_alpha provided.
#'
#' @examples
#' \dontrun{
#' 
#'plot_annual_trends(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'plot_annual_trends(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------


plot_annual_trends <- function(flowdata=NULL,
                               HYDAT=NULL,
                               trendsdata=NULL,
                               zyp_method=NA,
                               zyp_alpha=0.05,
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
                               station_name=NA,
                               write_plot=FALSE,
                               write_imgtype="pdf",
                               write_imgsize=c(5,8.5),
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
  
  if( is.na(zyp_method) & !zyp_method %in% c("yuepilon","zhang") )   {stop('zyp_trending aregument must be either "yuepilon" or "zhang"')}
  if( !is.na(zyp_alpha) & !is.numeric(zyp_alpha) )                 {stop("zyp_alpha must be numeric")}
  if( !is.na(zyp_alpha) & !all(zyp_alpha>0 & zyp_alpha<1))  {stop("timing_percent must be >0 and <1)")}
  
  if( !is.logical(write_plot))      {stop("write_plot argument must be logical (TRUE/FALSE)")}
  if( length(write_imgtype)>1)      {stop("write_imgtype argument cannot have length > 1")} 
  if( !is.na(write_imgtype) & !write_imgtype %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("write_imgtype argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  if( !is.numeric(write_imgsize) )   {stop("write_imgsize must be two numbers for height and width, respectively")}
  if( length(write_imgsize)!=2 )   {stop("write_imgsize must be two numbers for height and width, respectively")}
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  
  # If HYDAT station is listed, check if it exists
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  
  #--------------------------------------------------------------
  # Set up trendsdata
  
  trends_data <- trendsdata
  
  # If no trendsdata is provided, use flowdata ot HYDAT
  if ( is.null(trendsdata) ) {
    trends_data <- fasstr::calc_all_annual_stats(flowdata=flowdata,
                                                 HYDAT=HYDAT,
                                                 basin_area=basin_area, 
                                                 water_year=water_year,
                                                 water_year_start=water_year_start,
                                                 start_year=start_year,
                                                 end_year=end_year,
                                                 exclude_years=exclude_years,
                                                 annual_percentiles=annual_percentiles,
                                                 monthly_percentiles=monthly_percentiles,
                                                 lowflow_days=lowflow_days,
                                                 lowflow_align=lowflow_align,
                                                 totalflow_seasons=totalflow_seasons,
                                                 timing_percent=timing_percent,
                                                 normal_percentiles=normal_percentiles,
                                                 transpose=TRUE,
                                                 na.rm=na.rm)
  }
  
  
  #--------------------------------------------------------------
  # Complete trending
  
  # Trend the trends_data
  trends_results <- fasstr::compute_annual_trends(flowdata=NULL,
                                                  HYDAT=NULL,
                                                  trendsdata=trends_data,
                                                  zyp_method=zyp_method,
                                                  basin_area=NA)
  
  #--------------------------------------------------------------
  # Complete plotting
  
  # Create the list to place all plots
  trends_plots <- list()
  
  if (write_plot) {
    if (write_imgtype=="pdf"){
      file_trends_plot <-file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-plots.pdf", sep=""))
      pdf(file = file_trends_plot,8.5,4)
    }
    if (write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      file_trends_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-trends-plots",sep = "")
      dir.create(file_trends_plot)
    }
  }
  
  # Set data for plotting
  trends_data <- tidyr::gather(trends_data,Year,Value,-1)
  trends_data <- dplyr::mutate(trends_data,Year=as.numeric(Year))
  #Give some units
  if ( is.null(trendsdata) ) {
    trends_data <- dplyr::mutate(trends_data,
                                 Units="Discharge (cms)",
                                 Units=replace(Units, grepl("Yield_mm",Statistic), "Runoff Yield (mm)"),
                                 Units=replace(Units, grepl("TotalQ_m3",Statistic), "Total Discharge (cubic metres)"),
                                 Units=replace(Units, grepl("DoY",Statistic), "Day of Year"),
                                 Units=replace(Units, grepl("Days",Statistic), "Number of Days"))
  }
  
  # Loop through each statistic and plot the annual data, add trendline if < zyp_alpha
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
    if ( !is.na(zyp_alpha) & trends_results_stat$sig < zyp_alpha & !is.na(trends_results_stat$sig) ) {
      trends_plot <- trends_plot+
        ggplot2::geom_abline(slope = trends_results_stat$trend, intercept = int, colour="red")
    }
    
    trends_plots[[paste0(stat,"-trends")]] <- trends_plot
    
    # Save the plots if pdf
    if (write_plot & write_imgtype=="pdf") {
      plot(trends_plot)
    }
    
    # Save the plots if the png,jpeg,tiff,or bmp images are selected
    if (write_plot & write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      file_trendstat_plot <- paste(file_trends_plot,"/",stat,"-annual-trends.",write_imgtype,sep = "")
      ggplot2::ggsave(filename =file_trendstat_plot,trends_plot,width=write_imgsize[2],height=write_imgsize[1])
    }
    
    
  }
  
  # End the PDF device if selected
  if (write_plot & write_imgtype=="pdf") {
    dev.off()
  }
  
  
  return(trends_plots)
  
  
}

