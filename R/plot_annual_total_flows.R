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

#' @title Plot annual and seasonal total flows
#' 
#' @description Plots annual and seasonal total flows, volumetric and runoff yield flows, from a streamflow dataset. Calculates 
#'    the statistics from all daily discharge values from all years, unless specified. Data calculated from
#'    calc_annual_total_flows() function. For water year and seasonal data, the designated
#'    year is the year in which the year or season ends. For example, if using water years with a start month of 11, the OND season is
#'    designated by the water year which starts in November (designated by the calendar year in which it ends).
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm)
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required. 
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_plot Logical. Write the plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(5,11)}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#'    
#' @return A list of the following ggplot2 objects (yield plots unavailable if no basin_area):
#'   \item{TotalQ_Annual}{ggplot2 object of annual total volumetric discharge, in cubic metres}
#'   \item{TotalQ_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep total volumetric discharges, in cubic metres}
#'   \item{TotalQ_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec total volumetric discharges, in cubic metres}
#'   \item{Yield_Annual}{ggplot2 object of annual runoff yield, in millimetres}
#'   \item{Yield_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep runoff yields, in millimetres}
#'   \item{Yield_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec runoff yields, in millimetres}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#'plot_annual_total_flows(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'plot_annual_total_flows(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------
plot_annual_total_flows <- function(flowdata=NULL,
                                            HYDAT=NULL,
                                            basin_area=NA,
                                            water_year=FALSE,
                                            water_year_start=10,
                                            start_year=NULL,
                                            end_year=NULL,
                                            exclude_years=NULL,
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
  
  if( !is.na(basin_area) & !is.numeric(basin_area)) {stop("basin_area argument must be numeric")}
  if( length(basin_area)>1)                         {stop("basin_area argument cannot have length > 1")}

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
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
  }
  
  # Looks for STATION_NUMBER column to search for basin_area
  if ( is.na(basin_area) & "STATION_NUMBER" %in% names(flowdata)){
    basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = flowdata$STATION_NUMBER[1])$DRAINAGE_AREA_GROSS)
  }
  
  #--------------------------------------------------------------
  # Complete analysis
  
  totalflows_data <- fasstr::calc_annual_total_flows(flowdata=flowdata,
                                                       HYDAT=HYDAT,
                                                       station_name=station_name,
                                                       water_year=water_year,
                                                       water_year_start=water_year_start,
                                                       start_year=start_year,
                                                       end_year=end_year,
                                                       exclude_years=exclude_years,
                                                       basin_area=basin_area,
                                                       incl_seasons =TRUE)
  
  
  
  #--------------------------------------------------------------
  # Complete plotting
  
  options(scipen = 999)
  
  if (is.na(basin_area)) {
    plot_datatype <- list("TotalQ")
    message("no basin_area provided or available, no yield plots created")
  } else {
    plot_datatype <- list("TotalQ","Yield")
  }

  # Create an empty list to place the plots
  plots_list <- list()
  
  # Loop through TotalQ and Yield plots
  for (type in plot_datatype) {
    annual_data <- dplyr::select(totalflows_data,Year,dplyr::contains(paste0("Annual_",type)))
    annual_data <- tidyr::gather(annual_data,Statistic,Value,-1)
    seasons2_data <- dplyr::select(totalflows_data,Year,
                                   dplyr::contains(paste0("ONDJFM_",type)),
                                   dplyr::contains(paste0("AMJJAS_",type)))
    seasons2_data <- tidyr::gather(seasons2_data,Statistic,Value,-1)
    seasons4_data <- dplyr::select(totalflows_data,Year,
                                   dplyr::contains(paste0("OND_",type)),
                                   dplyr::contains(paste0("JFM_",type)),
                                   dplyr::contains(paste0("AMJ_",type)),
                                   dplyr::contains(paste0("JAS_",type)),
                                   -dplyr::contains(paste0("AMJJAS_",type)),
                                   -dplyr::contains(paste0("ONDJFM_",type)))
    seasons4_data <- tidyr::gather(seasons4_data,Statistic,Value,-1)
    
    # Create list of the dataframes to loop through
    data_list <- list(Annual=annual_data,"Two_Seasons"=seasons2_data,"Four_Seasons"=seasons4_data)
  
    title_num <- 1 #used to extract the dataframe name
    for (x in data_list) {
      title <- names(data_list[title_num])
      
      plot <- ggplot2::ggplot(data=x, ggplot2::aes(x=Year, y=Value))+
        ggplot2::geom_line(ggplot2::aes(colour=Statistic))+
        ggplot2::geom_point(ggplot2::aes(colour=Statistic))+
        ggplot2::facet_wrap(~Statistic, scales="free_x",ncol = 1, strip.position="right")+
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
        {if (type=="TotalQ") ggplot2::ylab("Total Discharge (cubic metres)")}+
        {if (type=="Yield") ggplot2::ylab("Runoff Yield (mm)")}+
        ggplot2::xlab("Year")+
        ggplot2::guides(colour=FALSE)+
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey80", fill=NA, size=.1),
                       panel.grid = ggplot2::element_line(size=.2))
      plots_list[[paste0(type,"_",title)]] <- plot
      
      # Write the plots if selected
      if (write_plot) {
        file_plot <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                           "_",type,"_",title,".",write_imgtype,sep = "")
        ggplot2::ggsave(filename =file_plot,plot,width=write_imgsize[2],height=write_imgsize[1])
      }
      
      title_num <- title_num+1
    }
  }
  
  
  return(plots_list)
  
}
