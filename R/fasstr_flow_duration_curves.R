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



#' @title Plot flow duration curves
#'
#' @description Plots flow duration curves, percent time a flow value is equalled or exceeded, for a streamflow dataset. Plots 
#'    statistics from all daily discharge values from all years, unless specified. Data calculated using fasstr_longterm_stats() 
#'    function then converted for plotting.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.  
#' @param incl_months Numeric. Months of data to plot. NA if no months required. Default \code{1:12}.
#' @param incl_longterm Logical. Include longterm curve of all data. Default \code{TRUE}.
#' @param custom_months Integer. Vector of months to combine to plot (ex. \code{6:8} for Jun-Aug). Leave blank for no custom months to plot.
#' @param log_discharge Logical. Plot the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(6,11)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#'
#' @return A plot of flow duration curves for specified long-term months and long-term flow data.
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_flow_duration_curves(flowdata = flowdata, station_name = "MissionCreek", write_plot = TRUE)
#' 
#'fasstr_flow_duration_curves(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


fasstr_flow_duration_curves <- function(flowdata=NULL,
                                        HYDAT=NULL,
                                        water_year=FALSE,
                                        water_year_start=10,
                                        start_year=NULL,
                                        end_year=NULL,
                                        exclude_years=NULL,
                                        incl_months=1:12,
                                        incl_longterm=TRUE,
                                        custom_months=NULL,
                                        log_discharge=TRUE,
                                        station_name=NA,
                                        write_plot=FALSE,     
                                        write_imgtype="pdf",    
                                        write_imgsize=c(6,11),
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
    if( any(flowdata$Value <0, na.rm=TRUE))           {stop('flowdata cannot have negative values - check your data')}
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
  
  if( !is.logical(incl_longterm))                       {stop("incl_longterm argument must be logical (TRUE/FALSE)")}
  if( !all(is.na(incl_months)) & !is.numeric(incl_months) )  {stop("incl_months argument must be integers")}
  if( !all(is.na(incl_months)) & !all(incl_months %in% c(1:12)) )                  {stop("incl_months argument must be integers between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(custom_months) & !is.numeric(custom_months) )  {stop("custom_months argument must be integers")}
  if( !all(custom_months %in% c(1:12)) )                      {stop("custom_months argument must be integers between 1 and 12 (Jan-Dec)")}
  
  if( !is.logical(log_discharge))         {stop("log_discharge argument must be logical (TRUE/FALSE)")}
  
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
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
  }
  
  #--------------------------------------------------------------
  #  Complete analysis and setup for plotting
  
  percentiles_data <- fasstr::fasstr_longterm_stats(flowdata=flowdata,
                                                    HYDAT=HYDAT,
                                                    station_name=station_name,
                                                    water_year=water_year,
                                                    water_year_start=water_year_start,
                                                    start_year=start_year,
                                                    end_year=end_year,
                                                    exclude_years=exclude_years,
                                                    custom_months = custom_months,
                                                    percentiles=c(.01,.1,.2,.5,1:99,99.5,99.8,99.9,99.99))
  percentiles_data <- dplyr::select(percentiles_data,-Mean,-Median,-Maximum,-Minimum)
  percentiles_data <- tidyr::gather(percentiles_data,Percentile,Value,-1)
  percentiles_data <- dplyr::mutate(percentiles_data,Percentile=100-as.numeric(gsub("P", "", Percentile)))
  
  # Filter for months and longterm selected to plot
  include <- month.abb[incl_months]
  if (incl_longterm) { include <- c(include,"Long-term") }
  if (!is.null(custom_months)) { include <- c(include,"Custom-Months") }
  percentiles_data <- dplyr::filter(percentiles_data,Month %in% include)
  
  
  #--------------------------------------------------------------
  #  Complete plotting
  
  flow_duration_plot <- ggplot2::ggplot(percentiles_data,ggplot2::aes(x=Percentile,y=Value,colour=Month))+
    ggplot2::geom_line()+
    {if (log_discharge) ggplot2::scale_y_log10(expand = c(0, 0))}+
    {if (!log_discharge) ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10),expand = c(0, 0))}+
    ggplot2::scale_x_continuous(expand =c(0,0),breaks = scales::pretty_breaks(n = 10))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("% Time flow equalled or exceeded")+
    ggplot2::scale_color_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise",
                                           "Apr" = "forestgreen", "May" = "limegreen","Jun" = "gold","Jul" = "orange", 
                                           "Aug" = "red","Sep" = "darkred", "Oct" = "orchid", "Nov" = "purple3",
                                           "Dec" = "midnightblue","Long-term" = "black","Custom-Months" = "grey60"))+
    ggplot2:: annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = ggplot2::unit(.07, "cm"),
                                  mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
                   panel.grid = ggplot2::element_line(size=.2),
                   legend.title = ggplot2::element_blank(),
                   legend.justification = "top")
  
  if (write_plot) {
    file_flowduration_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                    "-flow_duration_curves.",write_imgtype,sep = "")
    ggplot2::ggsave(filename = file_flowduration_plot,
                    flow_duration_plot,
                    height= write_imgsize[1],
                    width = write_imgsize[2])
  }
  
  
  return(flow_duration_plot)
  
  
}
