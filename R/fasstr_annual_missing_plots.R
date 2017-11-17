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


#' @title Plots missing data
#'
#' @description  Plots missing data
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
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param rolling_days Numeric. Rolling days. Default 1.
#' @param rolling_align Character. Specifies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations#'
#' @param write_plot Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param plot_type Character. pdf, png, bmp, jpeg, tiff. Default pdf.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#'
#'
#' @examples
#' \dontrun{
#' 
#' Coming soon :)
#' 
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_annual_missing_plots <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  station_name="fasstr",
                                  water_year=FALSE, #create another for own water year????
                                  start_year=NULL,
                                  end_year=NULL,
                                  water_year_start=10,
                                  rolling_days=1,
                                  rolling_align="right",
                                  write_plot=FALSE,        # write out statistics on calendar year
                                  plot_type="pdf",
                                  report_dir="."){              # decimal digits for csv files for statistics

  #############################################################
  
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}

  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( length(rolling_days)>1 ) {
    stop("rolling_days must be one number")  }
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("rolling_align parameter must be 'right', 'left', or 'center'.")}
  
  if( !is.logical(write_plot))  {stop("write_plot parameter must be logical (TRUE/FALSE)")}
  if( length(plot_type)>1)        {
    stop("plot_type argument cannot have length > 1")}
  if( !is.na(plot_type) & !plot_type %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("plot_type argument must be one of 'pdf','png','jpeg','tiff', or 'bmp'")}
  
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exist")}

  flow_summary <- fasstr::fasstr_annual_summary(flowdata=flowdata,
                                                HYDAT=HYDAT,
                                                station_name=station_name,
                                                water_year=water_year, 
                                                start_year=start_year,
                                                end_year=end_year,
                                                water_year_start=water_year_start,
                                                rolling_days=rolling_days,
                                                rolling_align=rolling_align,
                                                transpose=FALSE,
                                                write_table=FALSE,
                                                report_dir=report_dir,
                                                table_nddigits=3)
  
  missing_plotdata <- flow_summary[,c(1,10:21)]
  missing_plotdata <- tidyr::gather(missing_plotdata,Month,Value,2:13)

  if (water_year) {
    if (water_year_start==1) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
    } else if (water_year_start==2) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q"))
    } else if (water_year_start==3) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q"))
    } else if (water_year_start==4) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
    } else if (water_year_start==5) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
    } else if (water_year_start==6) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
    } else if (water_year_start==7) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q"))
    } else if (water_year_start==8) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q"))
    } else if (water_year_start==9) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q"))
    } else if (water_year_start==10) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q"))
    } else if (water_year_start==11) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q"))
    } else if (water_year_start==12) {
      missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q"))
    }
  } else {           
    missing_plotdata$Month <- factor(missing_plotdata$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
  }
  
  
  missing_plot <- ggplot2::ggplot(data=missing_plotdata, ggplot2::aes(x=Year, y=Value))+
    #ggtitle("Annual Summary Statistics")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
    ggplot2::geom_line(colour="dodgerblue4")+
    ggplot2::geom_point(colour="firebrick3")+
    ggplot2::facet_wrap(~Month, ncol=3, scales="free_y")+
    ggplot2::ylab("Missing Days")+
    ggplot2::xlab("Year")
  
  if (write_plot) {
    file_missing_plot <- paste(report_dir,"/",station_name,"-annual-missing-dates.",plot_type,sep = "")
    ggplot2::ggsave(filename = file_missing_plot,
                    missing_plot,
                    height= 5.5,
                    width = 8.5)
  }
  
  return(missing_plot)
} # end of function

