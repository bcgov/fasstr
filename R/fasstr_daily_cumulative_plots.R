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

#' @title Plot streamflow statistics for each day of the year.
#'
#' @description Compute streamflow statistics for each day of the year.
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
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm)
#' @param use_yield Logical. Use runoff yield for total flows instead of total volume. Basin Area required. Default FALSE
#' @param percentiles Numeric. List of numbers to calculate percentiles (5 = 5th percentile). Default c(5,25,75,95).
#' @param rolling_days Numeric. Rolling days. Default 1.
#' @param rolling_align Character. Specifies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations#' 
#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_plot Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param plot_type Character. pdf, png, bmp, jpeg, tiff. Default pdf.
#' @param plot_title Character. Text string of desired title for all plots. Default NA.
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default FALSE (linear).
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
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

fasstr_daily_cumulative_plots <- function(flowdata=NULL,
                                          HYDAT=NULL,
                                          station_name="fasstr",
                                          water_year=FALSE,
                                          water_year_start=10,
                                          start_year=NULL,
                                          end_year=NULL,
                                          exclude_years=NULL, 
                                          use_yield=FALSE,
                                          basin_area=NA,
                                          write_plot=FALSE,      
                                          plot_type="pdf",    
                                          #plot_all_years=TRUE,
                                          log_discharge=FALSE,
                                          plot_title=NA,
                                          report_dir=".",
                                          na.rm=list(na.rm.global=FALSE)){
  
  
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  
  # check if datasets have colums of columns created in fucntions (Vtotal in flowdata before daily_cumu function created error)
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
  
  if( !is.logical(water_year))  {
    stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {
    stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.logical(use_yield))  {
    stop("use_yield parameter must be logical (TRUE/FALSE)")}
  if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
  if( length(basin_area)>1)        {stop("basin_area parameter cannot have length > 1")}
  
  
  if( !is.logical(write_plot))  {
    stop("write_plot argument must be logical (TRUE/FALSE)")}
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
  
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' argument does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  # Looks for STATION_NUMBER column to search for basin_area
  if ( is.na(basin_area) & "STATION_NUMBER" %in% names(flowdata)){
    basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = flowdata$STATION_NUMBER[1])$DRAINAGE_AREA_GROSS)
  }
  
  # Check if no basin_area if use-yield is TRUE
  if( use_yield & is.na(basin_area) )  {
    stop("no basin_area provided with use_yield")}
  
  
  flowdata <- fasstr::fasstr_add_date_vars(flowdata = flowdata,water_year = T,water_year_start=water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  flowdata <- dplyr::filter(flowdata, AnalysisDoY <366)
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  
  if (water_year) {
    if (water_year_start==1) {
      origin_date <- as.Date("1899-12-31")
    } else if (water_year_start==2) {
      origin_date <- as.Date("1899-01-31")
    } else if (water_year_start==3) {
      origin_date <- as.Date("1899-02-28")
    } else if (water_year_start==4) {
      origin_date <- as.Date("1899-03-31")
    } else if (water_year_start==5) {
      origin_date <- as.Date("1899-04-30")
    } else if (water_year_start==6) {
      origin_date <- as.Date("1899-05-31")
    } else if (water_year_start==7) {
      origin_date <- as.Date("1899-06-30")
    } else if (water_year_start==8) {
      origin_date <- as.Date("1899-07-31")
    } else if (water_year_start==9) {
      origin_date <- as.Date("1899-08-31")
    } else if (water_year_start==10) {
      origin_date <- as.Date("1899-09-30")
    } else if (water_year_start==11) {
      origin_date <- as.Date("1899-10-31")
    } else if (water_year_start==12) {
      origin_date <- as.Date("1899-11-30")
    }
  } else if (!water_year) {
    origin_date <- as.Date("1899-12-31")
  }
  
  flowdata <- dplyr::mutate(flowdata,AnalysisDate=as.Date(AnalysisDoY, origin = origin_date))
  
  
  
  Q_daily_total <- fasstr::fasstr_daily_cumulative_stats(flowdata=flowdata,
                                                         HYDAT=NULL,
                                                         station_name=station_name,
                                                         water_year=water_year, #create another for own water year????
                                                         water_year_start=water_year_start,
                                                         start_year=start_year,
                                                         end_year=end_year,
                                                         exclude_years=exclude_years, # list of stations
                                                         use_yield=use_yield,
                                                         basin_area=basin_area,
                                                         percentiles=c(5,25,75,95),
                                                         transpose=FALSE,
                                                         write_table=FALSE,        # write out statistics on calendar year
                                                         report_dir=".",
                                                         na.rm=list(na.rm.global=FALSE),
                                                         table_nddigits=3)
  
  # Add cumulative flows to original flow data
  if (use_yield){
    flowdata <- fasstr::fasstr_add_cumulative_yield(flowdata,water_year = water_year, water_year_start = water_year_start, basin_area = basin_area)
    flowdata$Cumul_Flow <- flowdata$Cumul_Yield
  } else {
    flowdata <- fasstr::fasstr_add_cumulative_volume(flowdata,water_year = water_year, water_year_start = water_year_start)
    flowdata$Cumul_Flow <- flowdata$Cumul_Volume
  }
  
  
  Q_daily_total <- dplyr::mutate(Q_daily_total,Date=as.Date(DayofYear, origin = origin_date))
  
  
  # Create the list to place all plots
  daily_stats_plots <- list()
  
  if (write_plot) {
    if (plot_type=="pdf"){
      file_stat_plot <-file.path(report_dir, paste(station_name,"-daily-cumulative",ifelse(use_yield,paste0("-yield"),paste0("-volume")),"-stats.pdf", sep=""))
      pdf(file = file_stat_plot,8.5,4)
    }
    if (plot_type %in% c("png","jpeg","tiff","bmp")) {
      file_stat_plot <- paste(report_dir,"/",station_name,"-daily-cumulative",ifelse(use_yield,paste0("-yield"),paste0("-volume")),"-stats",sep = "")
      dir.create(file_stat_plot)
    }
  }
  
  # Create the daily stats plots
  daily_stats_plot <- ggplot2::ggplot(Q_daily_total,ggplot2::aes(x=Date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=Minimum,ymax=P5,fill = "Min-5th Percentile"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P5,ymax=P25,fill = "5th-25th Percentile"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P25,ymax=P75,fill = "25th-75th Percentile"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P75,ymax=P95,fill = "75th-95th Percentile"))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=P95,ymax=Maximum,fill = "95th Percentile-Max"))+
    ggplot2::geom_line(ggplot2::aes(y=Median, colour="Median"), size=.5) +
    ggplot2::geom_line(ggplot2::aes(y=Mean, colour="Mean"), size=.5) +
    ggplot2::scale_fill_manual(values = c("Min-5th Percentile" = "orange" ,"5th-25th Percentile" = "yellow", "25th-75th Percentile" = "skyblue1","75th-95th Percentile" = "dodgerblue2","95th Percentile-Max" = "royalblue4")) +
    ggplot2::scale_color_manual(values = c("Median"="purple3","Mean"="springgreen4")) +
    {if (!log_discharge) ggplot2::scale_y_continuous(expand = c(0, 0))}+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))} +
    {if (log_discharge) ggplot2::annotation_logticks(base= 10,"left",colour = "grey25",size=0.3,
                                                     short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c(NA,as.character(max(Q_daily_total$Date)))),expand=c(0,0)) +
    ggplot2::xlab(NULL)+
    {if (!use_yield) ggplot2::ylab("Cumulative Discharge (cubic metres)")}+
    {if (use_yield) ggplot2::ylab("Cumulative Runoff Yield (mm)")}+
    {if (!is.na(plot_title)) ggplot2::ggtitle(paste0(plot_title))}+
    ggplot2::theme(axis.text=ggplot2::element_text(size=6, colour = "grey25"),
                   axis.title=ggplot2::element_text(size=8, colour = "grey25"),
                   axis.title.y=ggplot2::element_text(margin=ggplot2::margin(0,0,0,0)),
                   axis.ticks = ggplot2::element_line(size=.1, colour = "grey25"),
                   axis.ticks.length=ggplot2::unit(0.05,"cm"),
                   #plot.title = element_text(size=12, colour = "grey25",hjust = 0.5,face="italic"),
                   panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(size=.1),
                   panel.background = ggplot2::element_rect(fill = "grey94"),
                   legend.title = ggplot2::element_blank(),        #legend.position = "top",
                   legend.text = ggplot2::element_text(size=7, colour="grey25"),
                   legend.box = "vertical",
                   legend.justification = "top",
                   legend.key.size = ggplot2::unit(0.4,"cm"),
                   legend.spacing =ggplot2::unit(0, "cm")
    ) +
    {if (!is.na(plot_title)) ggplot2::theme(plot.title = ggplot2::element_text(size=12, colour = "grey25",hjust = 0,face="italic"))}+
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2))
  # Add the plot to the list
  daily_stats_plots[["cumulative_statistics"]] <- daily_stats_plot
  
  # Plot the plot to the PDF device if selected
  if (write_plot & plot_type=="pdf") {
    plot(daily_stats_plot)
  }
  
  # Save the plots if the png,jpeg,tiff,or bmp images are selected
  if (write_plot & plot_type %in% c("png","jpeg","tiff","bmp")) {
    summary_plot <- paste(file_stat_plot,"/","daily-cumulative-stat.",plot_type,sep = "")
    ggplot2::ggsave(filename =summary_plot,daily_stats_plot,width=8.5,height=4)
  }
  
  # Add each annaul data to the plot
  for (yr in unique(flowdata$AnalysisYear)){
    flowdata_plot <- dplyr::filter(flowdata,AnalysisYear==yr)
    suppressMessages(daily_stats_year <- daily_stats_plot +
                       ggplot2::geom_line(data = flowdata_plot, ggplot2::aes(x=AnalysisDate, y=Cumul_Flow, colour= "yr.colour"), size=0.5) +
                       ggplot2::scale_color_manual(values = c("Mean" = "paleturquoise", "Median" = "dodgerblue4", "yr.colour" = "red"),
                                                   labels = c("Mean", "Median",paste0(yr," Flows"))))
    daily_stats_plots[[paste0("cumulative_", yr)]] <- daily_stats_year
    
    # Plot the plots to the PDF device if selected
    if (write_plot & plot_type=="pdf") {
      plot(daily_stats_year)
    }
    
    # Save the plots if the png,jpeg,tiff,or bmp images are selected
    if (write_plot & plot_type %in% c("png","jpeg","tiff","bmp")) {
      annual_plot <- paste(file_stat_plot,"/","daily-cumulative-",yr,".",plot_type,sep = "")
      ggplot2::ggsave(filename =annual_plot,daily_stats_year,width=8.5,height=4)
    }
  }
  
  # End the PDF device if selected
  if (write_plot & plot_type=="pdf") {
    dev.off()
  }
  
  
  return(daily_stats_plots)
  
  
} # end of function
