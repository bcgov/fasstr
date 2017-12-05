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


#' @title Plot annual and monthly missing dates
#'
#' @description Plots the number of missing data for each month of each year. Calculates the statistics from all daily discharge 
#'    values from all years, unless specified. Data calculated using fasstr_data_screening() function.
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
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(6,8.5)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#'
#' @return A plot of annual data statistics for screening
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_annual_missing_plots(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_annual_missing_plots(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


#--------------------------------------------------------------

fasstr_annual_missing_plots <- function(flowdata=NULL,
                                        HYDAT=NULL,
                                        rolling_days=1,
                                        rolling_align="right",
                                        water_year=FALSE, 
                                        water_year_start=10,
                                        start_year=NULL,
                                        end_year=NULL,
                                        station_name=NA,
                                        write_plot=FALSE,
                                        write_imgtype="pdf",
                                        write_imgsize=c(6,8.5),
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
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
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
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  #--------------------------------------------------------------
  # Complete analysis
  
  flow_summary <- fasstr::fasstr_data_screening(flowdata=flowdata,
                                                HYDAT=HYDAT,
                                                rolling_days=rolling_days,
                                                rolling_align=rolling_align,
                                                water_year=water_year,
                                                water_year_start=water_year_start,
                                                start_year=start_year,
                                                end_year=end_year)
  
  missing_plotdata <- flow_summary[,c(1,10:21)]
  missing_plotdata <- tidyr::gather(missing_plotdata,Month,Value,2:13)

  # Set the levels for plot ordering
  if (water_year) {
    if (water_year_start==1) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                        "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                        "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
    } else if (water_year_start==2) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                        "Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                        "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q"))
    } else if (water_year_start==3) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                        "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q",
                                                                        "Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q"))
    } else if (water_year_start==4) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q",
                                                                        "Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q",
                                                                        "Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
    } else if (water_year_start==5) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                        "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q",
                                                                        "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
    } else if (water_year_start==6) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                        "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q",
                                                                        "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
    } else if (water_year_start==7) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q",
                                                                        "Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q",
                                                                        "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q"))
    } else if (water_year_start==8) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q",
                                                                        "Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                        "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q"))
    } else if (water_year_start==9) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q",
                                                                        "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                        "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q"))
    } else if (water_year_start==10) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", 
                                                                        "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                        "Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q"))
    } else if (water_year_start==11) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", 
                                                                        "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                        "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q"))
    } else if (water_year_start==12) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                        "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q",
                                                                        "Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q"))
    }
  } else {
    missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                      "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
  }
  
  #--------------------------------------------------------------
  # Complete plotting 
  
  missing_plot <- ggplot2::ggplot(data=missing_plotdata, ggplot2::aes(x=Year, y=Value))+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::geom_line(colour="dodgerblue4")+
    ggplot2::geom_point(colour="firebrick3")+
    ggplot2::facet_wrap(~Month, ncol=3, scales="free_y")+
    ggplot2::ylab("Missing Days")+
    ggplot2::xlab("Year")
  
  if (write_plot) {
    file_missing_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                               "-missing-dates.",write_imgtype,sep = "")
    ggplot2::ggsave(filename = file_missing_plot,
                    missing_plot,
                    height= write_imgsize[1],
                    width = write_imgsize[2])
  }
  
  return(missing_plot)
  
}
