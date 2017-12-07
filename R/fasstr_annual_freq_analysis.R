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


#' @title Perform a volume frequency analysis on annual statistics
#'
#' @description Performs a volume frequency analysis on annual statistics from a streamflow dataset. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Analysis methodology replicates that from 
#'    \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param HYDAT_peaks Character. "MAX" or "MIN" instantaneous peaks pulled from HYDAT for analysis. Leave blank if not required.
#' @param rolling_days  Numeric. The number of days to apply a rolling mean. Default \code{1}.
#' @param rolling_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' @param use_max  Logical. Analyze the annual maximums rather than the minimums. Default \code{FALSE}.
#' @param use_log  Logical. Transform flow data to log-scale before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character. Plotting positions used in the frequency plots, one of "weibull","median", or "hazen". 
#'    Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the sample size and \code{a} and 
#'    \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median plotting postions; and (a=.5; b=.5) 
#'    for Hazen plotting positions. Default \code{"weibull"}.
#' @param prob_scale_points  Numeric. Vector of points to be plotted along the X axis in the frequency plot. Inverse of return period. 
#'    Default \code{c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)}.
#' @param fit_distr Character. Distribution to fit annual data, one of "PIII" (Pearson Log III distribution) or "weibull" 
#'    (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character. Method used to fit the distribution, one of  "MOM" (method of moments) or "MLE" (maximum
#'     likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if \code{fit_distr}=="weibull".
#' @param fit_quantiles Numeric. Vector of quantiles to be estimated from the fitted distribution. 
#'    Default \code{c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)}.
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
#' @param write_table_overview Logical. Write the overview table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_table_stats Logical. Write the annual statistics (used for analysis) table as a .csv file to specified directory. 
#'    Default \code{FALSE}.
#' @param write_table_plotdata Logical. Write the plotting data (used for analysis) table as a .csv file to specified directory. 
#'    Default \code{FALSE}.
#' @param write_table_quantiles Logical. Write the results table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_plot_frequency Logical. Write the frequency plot to specified directory. Default \code{FALSE}.
#' @param write_imgtype Character. One of "pdf","png","jpeg","tiff", or "bmp" image types to write the plot as. Default \code{"pdf"}.
#' @param write_imgsize Numeric. Height and width, respectively, of saved plot. Default \code{c(5,11)}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @return A list with the following elements:
#'   \item{Q_stat}{Data frame with Computed annual summary statistics used in analysis}
#'   \item{plotdata}{Data frame with Co-ordinates used in frequency plot.}
#'   \item{freqplot}{ggplot2 object with frequency plot}
#'   \item{fit}{List of fitted objects from fitdistrplus.}
#'   \item{fitted_quantiles}{Data frame with fitted quantiles.}
#'   \item{overview}{Data frame overview of parameters used in analysis.}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#' fasstr_annual_freq_analysis(HYDAT="08NM116",
#'                             start_year = 1980,
#'                             end_year = 2010)
#'                             
#' }
#' @export


fasstr_annual_freq_analysis <- function(flowdata=NULL,
                                        HYDAT=NULL,
                                        HYDAT_peaks=NULL,
                                        rolling_days=c(1,3,7,30),
                                        rolling_align="right",
                                        use_max=FALSE,
                                        use_log=FALSE,
                                        prob_plot_position=c("weibull","median","hazen"),
                                        prob_scale_points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                        fit_distr=c("PIII","weibull"),
                                        fit_distr_method=ifelse(fit_distr=="PIII","MOM","MLE"),
                                        fit_quantiles=c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                        water_year=FALSE,
                                        water_year_start=10,
                                        start_year=NULL,
                                        end_year=NULL,
                                        exclude_years=NULL,
                                        months=1:12,
                                        station_name=NA,
                                        write_table_overview=FALSE,
                                        write_table_stats=FALSE,
                                        write_table_plotdata=FALSE,  # write out the plotting data
                                        write_table_quantiles=FALSE, # write out the fitted quantiles
                                        write_plot_frequency=FALSE,  # write out the frequency plot
                                        write_imgtype=c("pdf"),
                                        write_imgsize=c(4,6),
                                        write_digits=3,
                                        write_dir='.',
                                        na.rm=list(na.rm.global=TRUE)){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
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
  
  if( is.null(HYDAT_peaks) ) {
    if(is.na(rolling_days) )     {stop("rolling_days argument must be provided")}
    if( !is.numeric(rolling_days) )                      {stop("rolling_days argument must be numeric")}
    if( !all(rolling_days %in% c(1:180)) )                    {stop("rolling_days argument must be integers > 0 and <= 180)")}
  }
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.null(HYDAT_peaks)) {if(!HYDAT_peaks %in% c("MAX","MIN") ) {stop("HYDAT_peaks argument must be 'MAX', 'MIN', or leave blank")}}
  if( is.null(HYDAT) & !is.null(HYDAT_peaks) )        {stop('station in HYDAT argument must be selected with HYDAT_peaks')}
  if( !is.null(HYDAT_peaks) ) {
    if(water_year)           warning("water_year argument was ignored - HYDAT_peaks is based on calendar years")
  }
  
  if( !is.list(na.rm))                {stop("na.rm is not a list") }
  if( !is.logical(unlist(na.rm))){    {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
    if( !is.logical(use_log))         {stop("use_log must be logical (TRUE/FALSE)")}
    if( !is.logical(use_max))         {stop("use_max must be logical (TRUE/FALSE)")}
    if( !all(prob_plot_position %in% c("weibull","median","hazen")))  stop("prob_plot_position must be one of weibull, 
                                                                           median, or hazen")}
  if( !is.numeric(prob_scale_points)) {stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(prob_scale_points>0 & prob_scale_points<1)){stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(fit_distr %in% c("weibull","PIII")))       {stop("fit_distr must be one of weibull or PIII")}
  if( !is.numeric(fit_quantiles))                     {stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(fit_quantiles >0 & fit_quantiles < 1))     {stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive)")}
  if( fit_distr[1]=='weibull' & use_log)              {stop("Cannot fit Weibull distribution on log-scale")}
  if( is.null(HYDAT) & fit_distr[1]=='weibull' & any(flowdata$Value<0, na.rm=TRUE)){stop("cannot fit weibull distribution with 
                                                                                         negative flow values")}
  if( fit_distr[1]!="PIII" & fit_distr_method[1]=="MOM"){stop('MOM only can be used with PIII distribution')}
  
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(write_table_overview))     {stop("write_table_overview must be logical (TRUE/FALSE")}
  if( !is.logical(write_table_stats))        {stop("write_table_stats must be logical (TRUE/FALSE")}
  if( !is.logical(write_table_plotdata))     {stop("write_table_plotdata must be logical (TRUE/FALSE")}
  if( !is.logical(write_table_quantiles))    {stop("write_table_quantiles must be logical (TRUE/FALSE")}
  if( !is.logical(write_plot_frequency))     {stop("write_plot_frequency must be logical (TRUE/FALSE)")}
  if( !write_imgtype[1] %in% c("pdf","png")) {stop("write_imgtype must be pdf or png")}
  if( !is.numeric(write_imgsize) )           {stop("write_imgsize must be two numbers for height and width, respectively")}
  if( length(write_imgsize)!=2 )             {stop("write_imgsize must be two numbers for height and width, respectively")}
  if( !is.numeric(write_digits))             {stop("write_digits must be numeric")}
  write_digits = round(write_digits)[1]
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
  
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Define functions for analysis
  
  # Define the log=Pearson III function needed for fitting at the GLOBAL environment level
  dPIII <<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)
  pPIII <<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  qPIII <<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  
  mPIII <<-function(order, shape, location, scale){
    # compute the empirical first 3 moments of the PIII distribution
    if(order==1) return( location + shape*scale)
    if(order==2) return(scale*scale*shape)
    if(order==3) return(2/sqrt(shape)*sign(scale))
  }
  
  
  #--------------------------------------------------------------
  # Gather annual data for analysis
  
  # If HYDAT_peaks is FALSE, then calculate the annual values to plot
  if ( is.null(HYDAT_peaks) ) {
    
    #--------------------------------------------------------------
    # Set the flowdata for analysis
    
    # Select just Date and Value for analysis
    flowdata <- dplyr::select(flowdata,Date,Value)
    
    # add date variables to determine the min/max cal/water years
    flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
    if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
    if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
    if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
    
    # Set selected year-type column for analysis
    if (water_year) {
      flowdata$AnalysisYear <- flowdata$WaterYear
    }  else {
      flowdata$AnalysisYear <- flowdata$Year
    }
    
    # Loop through each rolling_days and add customized names of rolling means to flowdata
    for (day in rolling_days) {
      flowdata_temp <- dplyr::select(flowdata,Date,Value)
      flowdata_temp <- fasstr::fasstr_add_rolling_means(flowdata_temp,days = day,align = rolling_align)
      names(flowdata_temp)[names(flowdata_temp) == paste0("Q",day,"Day")] <- paste("Q", formatC(day, width=3, format="d", flag="0"),"-day-Avg",sep="")
      flowdata_temp <- dplyr::select(flowdata_temp,-Value)
      flowdata <- merge(flowdata,flowdata_temp,by="Date",all = T)
    }
    
    # Truncate the data between the start and end year, excluded years, and months selected
    flowdata <- flowdata[ flowdata$AnalysisYear >=start_year & flowdata$AnalysisYear <= end_year,]
    flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
    flowdata <- dplyr::filter(flowdata, Month %in% months)
    
    # Calculate the min or max of the rolling means for each year
    flowdata <- flowdata[,-(1:8)]
    flowdata <- tidyr::gather(flowdata,Measure,value,-1)
    Q_stat <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisYear,Measure),
                               value=ifelse(use_max, max(value,na.rm=na.rm$na.rm.global), min(value,na.rm=na.rm$na.rm.global)))
    Q_stat <- dplyr::rename(Q_stat,Year=AnalysisYear)
    
  }
  
  # If HYDAT_peaks is TRUE, then grab the data from HYDAT
  if ( !is.null(HYDAT_peaks)) {
    inst_peaks <- suppressMessages(tidyhydat::hy_annual_instant_peaks(HYDAT))
    inst_peaks <- dplyr::filter(inst_peaks,Parameter=="Flow")
    inst_peaks <- dplyr::filter(inst_peaks,PEAK_CODE==HYDAT_peaks)
    inst_peaks <- dplyr::select(inst_peaks,Year=YEAR,Measure=PEAK_CODE,value=Value)
    inst_peaks <- dplyr::mutate(inst_peaks,Measure=paste0("INST_",Measure))
    
    if (is.null(start_year)) {start_year <- min(inst_peaks$Year)}
    if (is.null(end_year)) {end_year <- max(inst_peaks$Year)}
    if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
    
    inst_peaks <- inst_peaks[ inst_peaks$Year >=start_year & inst_peaks$Year <= end_year,]
    inst_peaks <- dplyr::filter(inst_peaks,!(Year %in% exclude_years))
    Q_stat <- inst_peaks
  }
  
  
  #--------------------------------------------------------------
  # Plot the data on the distrubtion
  
  # Compute the summary table for output
  Q_stat_output <- tidyr::spread(Q_stat,Measure,value)
  
  # See if a (natural) log-transform is to be used in the frequency analysis?
  # This flag also controls how the data is shown in the frequency plot
  if(use_log)Q_stat$value <- log(Q_stat$value)
  
  # make the plot. Remove any missing or infinite or NaN values
  Q_stat <- Q_stat[ is.finite(Q_stat$value),]  # remove missing/ Inf/ NaN values
  
  # get the plotting positions
  # From the HEC-SSP package, the  plotting positions are (m-a)/(n+1-a-b)
  a <- 0; b <- 0
  if(prob_plot_position[1]=='weibull'){a <- 0; b <- 0}
  if(prob_plot_position[1]=='median' ){a <-.3; b <-.3}
  if(prob_plot_position[1]=='hazen'  ){a <-.5; b <-.5}
  plotdata <- plyr::ddply(Q_stat, "Measure", function(x,a,b,use_max){
    # sort the data
    x <- x[ order(x$value),]
    x$prob <- ((1:length(x$value))-a)/((length(x$value)+1-a-b))
    if(use_max)x$prob <- 1- x$prob   # they like to use p(exceedance) if using a minimum
    x$dist.prob <- stats::qnorm(1-x$prob)
    x
  }, a=a, b=b, use_max=use_max)
  
  # change the measure labels in the plot
  plotdata2<- plotdata
  if (is.null(HYDAT_peaks)) {plotdata2$Measure <- paste(formatC(as.numeric(substr(plotdata2$Measure,2,4)),width=3),"-day Avg",sep="")}
  freqplot <- ggplot2::ggplot(data=plotdata2, ggplot2::aes(x=prob, y=value, group=Measure, color=Measure),environment=environment())+
    #ggplot2::ggtitle(paste(station_name, " Volume Frequency Analysis"))+
    ggplot2::geom_point()+
    ggplot2::xlab("Probability")+
    ggplot2::scale_x_continuous(trans=scales::probability_trans("norm", lower.tail=FALSE),
                                breaks=prob_scale_points,
                                sec.axis=ggplot2::sec_axis(trans=~1/.,
                                                           name='Return Period',
                                                           breaks=c(1.01,1.1,2,5,10,20,100,1000),
                                                           labels=function(x){ifelse(x<2,x,round(x,0))}))+
    ggplot2::theme(axis.title.x.top = ggplot2::element_text(size=10),
                   legend.title=ggplot2::element_blank(), legend.key.size=ggplot2::unit(.1,"in"))
  
  if(!use_max){ freqplot <- freqplot+ggplot2::theme(legend.justification=c(1,1), legend.position=c(1,1))}
  if( use_max){ freqplot <- freqplot+ggplot2::theme(legend.justification=c(1,0), legend.position=c(1,0))}
  if(!use_log){ freqplot <- freqplot + ggplot2::scale_y_log10(breaks=scales::pretty_breaks(n=10))}
  if( use_log){ freqplot <- freqplot + ggplot2::scale_y_continuous(breaks=scales::pretty_breaks(n=10))}
  if( use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("ln(Annual Maximum Flow (cms))")}  # adjust the Y axis label
  if( use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("ln(Annual Minimum Flow (cms))")}
  if(!use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("Annual Maximum Flow (cms)")}
  if(!use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("Annual Minimum Flow (cms)")}
  
  #--------------------------------------------------------------
  # fit the distribution to each measure
  
  # log-Pearson III implies that the log(x) has a 3-parameter gamma distribution
  ePIII <- function(x,order){
    # compute (centered) empirical centered moments of the data
    if(order==1) return(mean(x))
    if(order==2) return(stats::var(x))
    if(order==3) return(e1071::skewness(x, type=2))
  }
  
  fit <- plyr::dlply(Q_stat, "Measure", function(x, distr, fit_method){
    start=NULL
    # PIII is fit to log-of values unless use_log has been set, in which case data has previous been logged
    if(distr=='PIII' & !use_log){x$value <- log10(x$value)}
    # get starting values
    if(distr=='PIII'){
      # Note that the above forgot to mulitply the scale by the sign of skewness .
      # Refer to Page 24 of the Bulletin 17c
      m <- mean(x$value)
      v <- stats::var (x$value)
      s <- stats::sd  (x$value)
      g <- e1071::skewness(x$value, type=2)
      
      # This can be corrected, but HEC Bulletin 17b does not do these corrections
      # Correct the sample skew for bias using the recommendation of
      # Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited."
      # Water Resources Reseach 13(2): 427-443, as used by Kite
      #n <- length(x$value)
      #g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
      # We will use method of moment estimates as starting values for the MLE search
      
      my_shape <- (2/g)^2
      my_scale <- sqrt(v)/sqrt(my_shape)*sign(g)
      my_location <- m-my_scale*my_shape
      
      start=list(shape=my_shape, location=my_location, scale=my_scale)
    }
    
    if(fit_method=="MLE") {fit <- fitdistrplus::fitdist(x$value, distr, start=start, control=list(maxit=1000)) }# , trace=1, REPORT=1))
    if(fit_method=="MOM") {fit <- fitdistrplus::fitdist(x$value, distr, start=start,
                                                        method="mme", order=1:3, memp=ePIII, control=list(maxit=1000))
    } # fixed at MOM estimates
    fit
  }, distr=fit_distr[1], fit_method=fit_distr_method[1])
  
  
  #--------------------------------------------------------------
  # extracted the fitted quantiles from the fitted distribution
  
  fitted_quantiles <- plyr::ldply(names(fit), function (measure, prob,fit, use_max, use_log){
    # get the quantiles for each model
    x <- fit[[measure]]
    # if fitting minimums then you want EXCEEDANCE probabilities
    if( use_max) prob <- 1-prob
    quant <- stats::quantile(x, prob=prob)
    quant <- unlist(quant$quantiles)
    if(x$distname=='PIII' & !use_log)quant <- 10^quant # PIII was fit to the log-values
    if(  use_max) prob <- 1-prob  # reset for adding to data frame
    if(  use_log) quant <- exp(quant) # transforma back to original scale
    res <- data.frame(Measure=measure, distr=x$distname, prob=prob, quantile=quant ,stringsAsFactors=FALSE)
    rownames(res) <- NULL
    res
  }, prob=fit_quantiles, fit=fit, use_max=use_max, use_log=use_log)
  
  # get the transposed version
  fitted_quantiles$Return <- 1/fitted_quantiles$prob
  fitted_quantiles_output <- tidyr::spread(fitted_quantiles,Measure,quantile)
  fitted_quantiles_output <- dplyr::rename(fitted_quantiles_output,
                                           Distribution=distr,
                                           Probability=prob,
                                           "Return Period"=Return)
  
  
  if (write_table_overview | write_table_stats | write_table_plotdata | write_table_quantiles | write_plot_frequency) {
    folder_stat <- paste(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-frequency-analysis",sep = "")
    dir.create(folder_stat)
  }
  if(write_table_stats){
    # Write out the summary table for comparison to HEC spreadsheet
    file_stat_csv <- file.path(folder_stat,paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-vfa-annual-statistics.csv", sep=""))
    temp <- Q_stat_output
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], write_digits)  # round the output
    utils::write.csv(temp,file=file_stat_csv, row.names=FALSE)
  }
  
  if(write_table_plotdata){
    # Write out the plotdata for comparison with HEC output
    file_plotdata_csv <- file.path(folder_stat, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-vfa-plotdata.csv", sep=""))
    utils::write.csv(plotdata,file=file_plotdata_csv, row.names=FALSE)
  }
  
  if(write_table_quantiles){
    # Write out the summary table for comparison to HEC spreadsheet
    file_quantile_csv<- file.path(folder_stat, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-vfa-quantiles.csv", sep=""))
    temp <- fitted_quantiles_output
    temp[,3:ncol(temp)] <- round(temp[,3:ncol(temp)], write_digits)  # round the output
    utils::write.csv(temp,file=file_quantile_csv, row.names=FALSE)
  }
  
  if(write_plot_frequency){
    file_frequency_plot <- file.path(folder_stat, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-vfa-frequency-plot.",write_imgtype[1],sep=""))
    ggplot2::ggsave(plot=freqplot, file=file_frequency_plot, h=write_imgsize[1], w=write_imgsize[2], units="in", dpi=300)
  }
  
  analysis.options <- list("station name"= station_name,
                           "year type"=ifelse(!water_year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
                           "year range"=paste0(start_year," - ",end_year),
                           "excluded years"=paste(exclude_years,collapse = ', '),
                           "min or max"=ifelse(use_max,"maximums","minimums"),
                           rolling_days=paste(rolling_days,collapse = ', '),
                           fit_distr=fit_distr[1],  # distributions fit to the data
                           fit_distr_method=fit_distr_method[1],
                           prob_plot_position=prob_plot_position[1]
  )
  analysis.options.df <- data.frame("Option"=names(analysis.options),
                                    "Selection"=unlist(analysis.options,use.names = F))
  
  # Write out the analysis options
  if (write_table_overview){
    file_options_csv<- file.path(folder_stat, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-vfa-overview.csv", sep=""))
    utils::write.csv(analysis.options.df,file=file_options_csv, row.names=FALSE)
  }
  
  
  list(Q_stat=Q_stat_output,
       plotdata=plotdata,  # has the plotting positions for each point in frequency analysis
       freqplot = freqplot,
       fit = fit,               # list of fits of freq.distr to each measure
       fitted_quantiles=fitted_quantiles_output,             # fitted quantiles and their transposition
       overview = analysis.options.df
  )
}
