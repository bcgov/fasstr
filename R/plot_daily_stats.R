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

#' @title Plot the daily summary statistics
#'
#' @description Plot the daily mean, median, maximum, minimum, and percentiles for each day of the year of daily flow values 
#'    from a streamflow dataset. Plots the statistics from all daily discharge values from all years, unless specified. Can determine
#'    statistics of rolling mean days (e.g. 7-day flows) using the rolling_days argument. Data calculated using calc_daily_stats()
#'    function.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param rolling_days  Numeric. The number of days to apply a rolling mean. Default \code{1}.
#' @param rolling_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.  
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default \code{TRUE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(5,11)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#'
#' @return A list of ggplot2 objects, the first the daily statistics plot containing the listed plots below, and the sebsequent plots for each
#'    year of data provided containing the first plot plus the daily flow data for each year.
#'   \item{Mean}{daily mean}
#'   \item{Median}{daily median}
#'   \item{25-75 Percentiles Range}{a ribbon showing the range of data between the daily 25th and 75th percentiles}
#'   \item{5-95 Percentiles Range}{a ribbon showing the range of data between the daily 5th and 95th percentiles}
#'   \item{Max-Min Range}{a ribbon showing the range of data between the daily minimum and maximums}
#'   \item{'Year' Flows}{(on annual plots) the daily flows for the designated year}
#'    
#' @examples
#' \dontrun{
#' 
#'plot_daily_stats(flowdata = flowdata, station_name = "MissionCreek", write_plot = TRUE)
#' 
#'plot_daily_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


#--------------------------------------------------------------

plot_daily_stats <- function(flowdata=NULL,
                             HYDAT=NULL,
                             rolling_days=1,
                             rolling_align="right",
                             water_year=FALSE,
                             water_year_start=10,
                             start_year=NULL,
                             end_year=NULL,
                             exclude_years=NULL,
                             log_discharge=TRUE,
                             station_name=NA,
                             write_plot=FALSE,      
                             write_imgtype="pdf",       
                             write_imgsize=c(4,8.5),
                             write_dir=".",
                             na.rm=list(na.rm.global=FALSE)){
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
  
  if( !is.null(HYDAT) & !is.null(flowdata))           {stop("must select either flowdata or HYDAT arguments, not both")}
  if( is.null(HYDAT)) {
    if( is.null(flowdata))                            {stop("one of flowdata or HYDAT arguments must be set")}
    if( !is.data.frame(flowdata))                     {stop("flowdata arguments is not a data frame")}
    if( !all(c("Date","Value") %in% names(flowdata))) {stop("flowdata data frame doesn't contain the variables 'Date' and 'Value'")}
    if( !inherits(flowdata$Date[1], "Date"))          {stop("'Date' column in flowdata data frame is not a date")}
    if( !is.numeric(flowdata$Value))                  {stop("'Value' column in flowdata data frame is not numeric")}
    if( any(flowdata$Value <0, na.rm=TRUE))           {warning('flowdata cannot have negative values - check your data')}
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
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(log_discharge))         {stop("log_discharge argument must be logical (TRUE/FALSE)")}
  
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
  
  if( !is.list(na.rm))                        {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm)))             {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis (required for plotting daily values)
  
  # Select just Date and Value for analysis
  flowdata <- dplyr::select(flowdata,Date,Value)
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  flowdata <- fasstr::add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  # Set selected year-type and day of year, and date columns for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
    
    # Create origin date to apply to flowdata and Q_daily later on
    if (water_year_start==1)         {origin_date <- as.Date("1899-12-31")
    } else if (water_year_start==2)  {origin_date <- as.Date("1899-01-31")
    } else if (water_year_start==3)  {origin_date <- as.Date("1899-02-28")
    } else if (water_year_start==4)  {origin_date <- as.Date("1899-03-31")
    } else if (water_year_start==5)  {origin_date <- as.Date("1899-04-30")
    } else if (water_year_start==6)  {origin_date <- as.Date("1899-05-31")
    } else if (water_year_start==7)  {origin_date <- as.Date("1899-06-30")
    } else if (water_year_start==8)  {origin_date <- as.Date("1899-07-31")
    } else if (water_year_start==9)  {origin_date <- as.Date("1899-08-31")
    } else if (water_year_start==10) {origin_date <- as.Date("1899-09-30")
    } else if (water_year_start==11) {origin_date <- as.Date("1899-10-31")
    } else if (water_year_start==12) {origin_date <- as.Date("1899-11-30")}
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
    
    # Create origin date to apply to flowdata and Q_daily later on
    origin_date <- as.Date("1899-12-31")
  }
  flowdata <- dplyr::mutate(flowdata,AnalysisDate=as.Date(AnalysisDoY, origin = origin_date))
  
  # Filter for the selected and excluded years and leap year values (last day)
  flowdata <- dplyr::filter(flowdata, AnalysisDoY <366)
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  
  #--------------------------------------------------------------
  # Complete analysis
  
  Q_daily<- fasstr::calc_daily_stats(flowdata=flowdata,
                                     HYDAT=NULL,
                                     station_name=station_name,
                                     water_year=water_year,
                                     water_year_start=water_year_start,
                                     start_year=start_year,
                                     end_year=end_year,
                                     exclude_years=exclude_years,
                                     percentiles=c(5,25,75,95),
                                     rolling_days=rolling_days,
                                     rolling_align=rolling_align)
  
  Q_daily <- dplyr::mutate(Q_daily,Date=as.Date(DayofYear, origin = origin_date))
  
  #--------------------------------------------------------------
  # Complete plotting
  
  # Create the list to place all plots
  daily_stats_plots <- list()
  
  if (write_plot) {
    if (write_imgtype=="pdf"){
      file_stat_plot <-file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                                  "-daily-summary-statistics.pdf", sep=""))
      pdf(file = file_stat_plot,width=write_imgsize[2],height=write_imgsize[1])
    }
    if (write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      file_stat_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                              "-daily-summary-statistics",sep = "")
      dir.create(file_stat_plot)
    }
  }
  
  # Create the daily stats plots
  daily_stats_plot <- ggplot2::ggplot(Q_daily,ggplot2::aes(x=Date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=Minimum,ymax=Maximum,fill = "Max-Min Range"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P5,ymax=P95,fill = "5-95 Percentiles"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P25,ymax=P75,fill = "25-75 Percentiles"))+
    ggplot2::geom_line(ggplot2::aes(y=Median, colour="Median"), size=.5)+
    ggplot2::geom_line(ggplot2::aes(y=Mean, colour="Mean"), size=.5) +
    ggplot2::scale_fill_manual(values = c("Max-Min Range" = "lightblue2" ,"5-95 Percentiles" = "lightblue3", 
                                          "25-75 Percentiles" = "lightblue4")) +
    ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4"),labels = c("Mean", "Median")) +
    {if (!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0))}+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))} +
    {if (log_discharge) ggplot2::annotation_logticks(base= 10,"left",colour = "grey25",size=0.3,
                                                     short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                     long = ggplot2::unit(.2, "cm"))} +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month", 
                          limits = as.Date(c(NA,as.character(max(Q_daily$Date)))),expand=c(0,0)) +
    ggplot2::xlab(NULL)+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::theme(axis.text=ggplot2::element_text(size=6, colour = "grey25"),
                   axis.title=ggplot2::element_text(size=8, colour = "grey25"),
                   axis.ticks = ggplot2::element_line(size=.1, colour = "grey25"),
                   axis.ticks.length=ggplot2::unit(0.05,"cm"),
                   axis.title.y=ggplot2::element_text(margin=ggplot2::margin(0,0,0,0)),
                   panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(size=.1),
                   panel.background = ggplot2::element_rect(fill = "grey94"),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=7, colour="grey25"),
                   legend.box = "vertical",legend.justification = "top",
                   legend.key.size = ggplot2::unit(0.4,"cm"),
                   legend.spacing=ggplot2::unit(0, "cm")) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2))
  # Add the plot to the list
  daily_stats_plots[["daily_statisitics"]] <- daily_stats_plot
  
  # Plot the plot to the PDF device if selected
  if (write_plot & write_imgtype=="pdf") {
    plot(daily_stats_plot)
  }
  
  # Save the plots if the png,jpeg,tiff,or bmp images are selected
  if (write_plot & write_imgtype %in% c("png","jpeg","tiff","bmp")) {
    summary_plot <- paste(file_stat_plot,"/","daily-summary-stat.",write_imgtype,sep = "")
    ggplot2::ggsave(filename =summary_plot,daily_stats_plot,width=write_imgsize[2],height=write_imgsize[1])
  }
  
  # Add each annaul data to the plot
  for (yr in unique(flowdata$AnalysisYear)){
    flowdata_plot <- dplyr::filter(flowdata,AnalysisYear==yr)
    suppressMessages(daily_stats_year <- daily_stats_plot +
                       ggplot2::geom_line(data = flowdata_plot, ggplot2::aes(x=AnalysisDate, y=RollingValue, colour= "yr.colour"), size=0.5) +
                       ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                                   labels = c("Mean", "Median",paste0(yr," Flows"))))
    daily_stats_plots[[paste0("daily_", yr)]] <- daily_stats_year
    
    # Plot the plots to the PDF device if selected
    if (write_plot & write_imgtype=="pdf") {
      plot(daily_stats_year)
    }
    
    # Save the plots if the png,jpeg,tiff,or bmp images are selected
    if (write_plot & write_imgtype %in% c("png","jpeg","tiff","bmp")) {
      annual_plot <- paste(file_stat_plot,"/","daily-summary-",yr,".",write_imgtype,sep = "")
      ggplot2::ggsave(filename =annual_plot,daily_stats_year,width=write_imgsize[2],height=write_imgsize[1])
    }
  }
  
  # End the PDF device if selected
  if (write_plot & write_imgtype=="pdf") {
    dev.off()
  }
  
  
  return(daily_stats_plots)
  
  
}
