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


#' @title Plot annual lowflows
#'
#' @description Plot annual n-day minimum values, and the day of year of occurrence of daily flow values from a 
#'    streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. Data
#'    calculated from calc_annual_lowflows() function.
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
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param months Integer. Vector of months to consider for analysis (ex. \code{6:8} for Jun-Aug). Leave blank if all months
#'    are required. Default \code{1:12}.
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
#' @return A list of the following ggplot2 objects:
#'   \item{Annual_Minimums}{ggplot2 object of annual minimums of selected n-day rolling means}
#'   \item{Annual_Minimums_Days}{ggplot2 object of the day of years of annual minimums of selected n-day rolling means}
#'   
#' @examples
#' \dontrun{
#' 
#'plot_annual_lowflows(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'plot_annual_lowflows(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, rolling_days = c(3,7))
#'
#' }
#' @export

#--------------------------------------------------------------

plot_annual_lowflows <- function(flowdata=NULL,
                                 HYDAT=NULL,
                                 rolling_days=c(1,3,7,30),
                                 rolling_align="right",
                                 water_year=FALSE,
                                 water_year_start=10,
                                 start_year=NULL,
                                 end_year=NULL,
                                 exclude_years=NULL, 
                                 months=1:12,
                                 station_name=NA,
                                 write_plot=FALSE,
                                 write_imgtype="pdf",
                                 write_imgsize=c(5,8.5),
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
  
  if( !is.numeric(months) )        {stop("months argument must be integers")}
  if( !all(months %in% c(1:12)) )  {stop("months argument must be integers between 1 and 12 (Jan-Dec)")}
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
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
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  #--------------------------------------------------------------
  # Complete analysis
  
  # COmpute the low flows data
  lowflows_data <- fasstr::calc_annual_lowflows(flowdata=flowdata,
                                                  HYDAT=HYDAT,
                                                  rolling_days=rolling_days,
                                                  rolling_align=rolling_align,
                                                  water_year=water_year,
                                                  water_year_start=water_year_start,
                                                  start_year=start_year,
                                                  end_year=end_year,
                                                  exclude_years=exclude_years, 
                                                  months=months)
  
  # Gather data and plot the minimums day
  lowflows_DoY_data <- dplyr::select(lowflows_data,Year,dplyr::contains("DoY"))
  lowflows_DoY_data <- tidyr::gather(lowflows_DoY_data,Statistic,Value,-1)
  
  # Gather data and plot the minimums values
  lowflows_value_data <- dplyr::select(lowflows_data,Year,dplyr::contains("Day"),
                                       -dplyr::contains("DoY"),
                                       -dplyr::contains("Date"))
  lowflows_value_data <- tidyr::gather(lowflows_value_data,Statistic,Value,-1)
  
  #--------------------------------------------------------------
  # Complete plotting
  
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
  
  
  # Write the plots if selected.
  if (write_plot) {
    file_doy_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                           "_annual_lowflow_minimums.",write_imgtype,sep = "")
    ggplot2::ggsave(filename =file_doy_plot,doy_plot,width=write_imgsize[2],height=write_imgsize[1])
    
    file_min_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                           "_annual_lowflow_minimum_days.",write_imgtype,sep = "")
    ggplot2::ggsave(filename =file_min_plot,min_plot,width=write_imgsize[2],height=write_imgsize[1])
  }
  
  low_flow_plots <- list()
  low_flow_plots[["Annual_Minimums"]] <- min_plot
  low_flow_plots[["Annual_Minimum_Days"]] <- doy_plot
  
  
  
  return(low_flow_plots)
  
}

