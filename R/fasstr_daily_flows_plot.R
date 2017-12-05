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

#' @title Plot daily mean streamflow
#'
#' @description Plot the daily mean flow values from a streamflow dataset. Plots the statistics from all daily discharge values from all 
#'    years, unless specified. Can choose specific dates to start and end plotting. Can choose to plot out each year separately.
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
#' @param start_year Integer. First year to consider for plotting. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for plotting. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from plotting. Leave blank if all years are required.  
#' @param start_date Date. First date to consider for plotting. Leave blank if all years are required.
#' @param end_date  Date. Last date to consider for plotting. Leave blank if all years are required.
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default \code{FALSE} (linear).
#' @param plot_by_year Logical. Plot each year of data individually. Default \code{FALSE}.
#' @param log_discharge Logical. Place the discharge axis (Y) on log scale. Default \code{FALSE} (linear).
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(5,11)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory. 
#' 
#' @return A plot of a daily mean streamflow dataset
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_daily_flows_plot(flowdata = flowdata, station_name = "MissionCreek", write_plot = TRUE)
#' 
#'fasstr_daily_flows_plot(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------

fasstr_daily_flows_plot <- function(flowdata=NULL,
                                    HYDAT=NULL,
                                    rolling_days=1,
                                    rolling_align="right",
                                    water_year=FALSE,
                                    water_year_start=10,
                                    start_year=NULL,
                                    end_year=NULL,
                                    exclude_years=NULL,
                                    start_date=NULL,
                                    end_date=NULL,
                                    log_discharge=FALSE,
                                    plot_by_year=FALSE,
                                    station_name=NA,
                                    write_plot=FALSE,      
                                    write_imgtype="pdf",       
                                    write_imgsize=c(ifelse(plot_by_year,11,6.35),18),
                                    write_dir="."){
  
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
  
  if( !is.null(start_date)) {if(class(try(as.Date(start_date)))=="try-error" ) {stop("start_date must be a date formatted YYYY-MM-DD")}}
  if( !is.null(end_date))   {if(class(try(as.Date(end_date)))=="try-error" )   {stop("end_date must be a date formatted YYYY-MM-DD")}}
  if( !is.null(end_date) & !is.null(start_date) ) {if( start_date>=end_date )  {stop("start_date must be less than end_date")}}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for plotting
  
  flowdata <- dplyr::select(flowdata,Date,Value)
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata,water_year_start=water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start=water_year_start)
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  # determine the min/max cal/water years
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  # Set selected year-type for plotting
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
  }
  
  # Filter for specific years, if selected
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  # Filter for specific dates, if selected
  if( !is.null(start_date)) { flowdata <- dplyr::filter(flowdata, Date >= start_date) }
  if( !is.null(end_date))   { flowdata <- dplyr::filter(flowdata, Date <= end_date) }
  
  
  #--------------------------------------------------------------
  # Plot the data
  
  timeseries_plot <- ggplot2::ggplot(data=flowdata, ggplot2::aes(x=Date, y=RollingValue))+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::geom_line(colour="dodgerblue4")+
    ggplot2::ylab("Discharge (cms)")+
    {if (plot_by_year) ggplot2::facet_wrap(~AnalysisYear, scales="free_x")} +
    {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8),expand = c(0, 0))}+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (plot_by_year) ggplot2::scale_x_date(date_labels = "%b")} +
    {if (!plot_by_year) ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 12))} +
    {if (!log_discharge) ggplot2::expand_limits(y = c(0,max(flowdata$RollingValue)*1.05))}+
    {if (log_discharge) ggplot2::expand_limits(y = c(min(flowdata$RollingValue)*.95,max(flowdata$RollingValue)*1.05))}+
    ggplot2::theme( panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.5),
                    panel.grid.minor.y = ggplot2::element_blank())
  
  
  if (write_plot) {
    file_timeseries_plot <- paste(write_dir,"/",
                                  paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                  ifelse(plot_by_year,paste0("-annual-daily-flows."),paste0("-daily-flows.")),
                                  write_imgtype,sep = "")
    ggplot2::ggsave(filename = file_timeseries_plot,
                    timeseries_plot,
                    height = write_imgsize[1],
                    width = write_imgsize[2])
  }
  
  
  
  
  
  
  return(timeseries_plot)
  
  
} # end of function
