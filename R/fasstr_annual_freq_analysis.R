#' @title Perform a volume frequency analysis on annual statistics.
#'
#' @description Performs a volume frequency analysis on annual statistics similar to HEC-SSP.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics.
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param HYDAT_peaks Character. "MAX" or "MIN" instantaneous peaks pulled from HYDAT
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
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).
#' @param rolling_days Volume frequency analysis conducted on these rolling averages.
#' @param use_log  Transfrom to log-scale before analysis?
#' @param use_max  Analyze the maximums rather than the minimums.
#' @param prob_plot_position Which plotting positions should be used in the frequency plots. Points are plotted
#'       against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the sample size and
#'       \code{a} and \code{b} are defined as:
#'       (a=0, b=0) for Weibull plotting positions;
#'       (a=.2; b=.3) for Median plotting postions;
#'       (a=.5; b=.5) for Hazen plotting positions.
#' @param prob_scale_points  What points should be plotted along the $X$ axis in the frequency plot.
#' @param fit_distr Which distribution should be fit? PIII = Pearson Log III distribution; weibull=Weibull distribution.
#' @param fit_distr_method Which method used to fit the distribution. MOM=Method of moments; MLE=maximum likelihood estimation.
#' @param fit_quantiles Which quantiles should be estimated from the fitted distribution?
#' @param na.rm
#'
#' @param write_stat_table Should a file be created with the computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,"-annual-vfa-stat.csv", sep=""))}.
#' @param write_plotdata_table Should a file be created with the frequency plot data?
#'    The file name will be  \code{file.path(report_dir, paste(station_name,"-annual-vfa-plotdata.csv", sep=""))}.
#' @param write_quantiles_table Should a file be created with the fitted quantiles?.
#'    The file name will be  \code{file.path(report_dir, paste(station_name,"-annual-vfa-quantiles.csv", sep=""))}.
#' @param write_frequency_plot Should a file be created with the frequency plot..
#'    The file name will be \code{file.path(report_dir, paste(station_name,"-annual-vfa-frequency-plot.",write_frequency_plot_type[1],sep=""))}
#' @param write_frequency_plot_type Format of the frequency plot.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#'
#' @return A list with the following elements:
#'   \item{Q.flow.summary}{Data frame with flow summary.}
#'   \item{start_year}{Start year of the analysis}
#'   \item{end_year}{End year of the analysis}
#'   \item{water_year}{Were computations done on water year?}
#'   \item{use_max}{Were computations done on maximum values.}
#'   \item{rolling_days}{Rolling average days on which statistics computed.}
#'   \item{Q_stat}{Data frame with Computed annual summary statistics used in analysis}
#'   \item{Q_stat_trans}{Data frame with Computed annual summary statistics (transposed) used in analysis.}
#'   \item{plotdata}{Data frame with Co-ordinates used in frequency plot.}
#'   \item{prob_plot_position}{Which plotting position was used in frequency plot?}
#'   \item{freqplot}{ggplot2 object with frequency plot}
#'   \item{fit_distr}{Which distribution was fit to the data}
#'   \item{fit}{List of fitted objects from fitdistrplus.}
#'   \item{fitted_quantiles}{Data frame with fitted quantiles.}
#'   \item{fitted_quantiles_trans}{Data frame with fitted quantiles (transposed)}
#'   \item{file_stat_csv}{Object with file name of *.csv file with flow summary}
#'   \item{ file_stat_trans_csv}{Object with file name of *.csv file with flow summary (transposed)}
#'   \item{file_plotdata_csv}{Object with file name of *.csv file with plotting information for frequency plot.}
#'   \item{file_quantile_csv}{Object with file name of fitted quantiles.}
#'   \item{file_quantile_trans_csv}{Object with file name of fitted quantiles (transposed)}
#'   \item{file_frequency_plot}{Object with file name of *.pdf or *.png file with frequency plot}
#'   \item{Version}{Version of this function.}
#'   \item{Date}{Date function was run.}

#' @examples
#' \dontrun{
#' vfa.analysis <- compute.volume.frequency.analysis(
#'                      station_name ='XXX',
#'                      flowdata         =flow,
#'                      start_year   =1960,
#'                      end_year     =2014)
#' }
#' @export


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

# Compute the volume frequency analysis - similar to the HEC-SSP program
# The key difference lies in how the PIII distribuition is fit. I use MLE while
# HEC-SSP appears to use method of moments.

fasstr_annual_freq_analysis <- function(station_name="fasstr",
                                 flowdata=NULL,
                                 HYDAT=NULL,
                                 HYDAT_peaks=NA,
                                 water_year=FALSE,
                                 water_year_start=10,
                                 start_year=NULL,
                                 end_year=NULL,
                                 exclude_years=NULL,
                                 rolling_days=c(1,3,7,30),
                                 use_log=FALSE,
                                 use_max=FALSE,
                                 prob_plot_position=c("weibull","median","hazen"),
                                 prob_scale_points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                 fit_distr=c("PIII","weibull"),
                                 fit_distr_method=ifelse(fit_distr=="PIII","MOM","MLE"),
                                 fit_quantiles=c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                 na.rm=list(na.rm.global=TRUE),
                                 write_overview=FALSE,
                                 write_stat_table=FALSE,
                                 write_plotdata_table=FALSE,  # write out the plotting data
                                 write_quantiles_table=FALSE, # write out the fitted quantiles
                                 write_frequency_plot=FALSE,  # write out the frequency plot
                                 write_frequency_plot_type=c("pdf","png"),
                                 report_dir='.',
                                 table_nddigits=3)
# Input parameter - consult man-roxygen directory
# Output list - see above.

{
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
  # data checks
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station_name))  {stop("station_name required with flowdata parameter.")}
  if( is.null(HYDAT) & !is.character(station_name))  {stop("Station Code must be a character string.")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata data frame is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !HYDAT_peaks %in% c("MAX","MIN") & !is.na(HYDAT_peaks) ) {
    stop("HYDAT_peaks argument must be 'MAX', 'MIN', or NA.")}
if( is.null(HYDAT) & HYDAT_peaks %in% c("MAX","MIN"))   {
    stop('Station in HYDAT argument must be selected with HYDAT_peaks.')}
  if( !is.logical(water_year))  {stop("water_year must be logical (TRUE/FALSE")}
  if( water_year & HYDAT_peaks %in% c("MAX","MIN"))   {
    warning("water_year argument was ignored. HYDAT_peaks completed strictly using calendar years.")}
    
  
  if( !is.numeric(rolling_days))   {stop("rolling_days must be numeric")}
  if( !all(rolling_days>=0 & rolling_days<=180))
  {stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))
  {stop("rolling_days must be integers")}
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exits")}
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if( !is.logical(write_stat_table))  {stop("write_stat_table must be logical (TRUE/FALSE")}
  if( !is.logical(unlist(na.rm))){  {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
    if( !is.logical(use_log))         {stop("use_log must be logical (TRUE/FALSE)")}
    if( !is.logical(use_max))         {stop("use_max must be logical (TRUE/FALSE)")}
    if( !all(prob_plot_position %in% c("weibull","median","hazen"))){}
    stop("prob_plot_position must be one of weibull, median, or hazen")}
  if( !is.numeric(prob_scale_points)){stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(prob_scale_points>0 & prob_scale_points<1)){
    stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(fit_distr %in% c("weibull","PIII"))){
    stop("fit_distr must be one of weibull or PIII")}
  if( !is.numeric(fit_quantiles))  {stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive)")}
  if( !all(fit_quantiles >0 & fit_quantiles < 1)){
    stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive)")}
  if(  fit_distr[1]=='weibull' & use_log){stop("Cannot fit Weibull distribution on log-scale")}
  if(  is.null(HYDAT) & fit_distr[1]=='weibull' & any(flowdata$Value<0, na.rm=TRUE)){stop("cannot fit weibull distribution with negative flow values")}
  if(  fit_distr[1]!="PIII" & fit_distr_method[1]=="MOM"){stop('MOM only can be used with PIII distribution')}
  
  if( !is.logical(write_stat_table))      {stop("write_stat_table must be logical (TRUE/FALSE")}
  if( !is.logical(write_plotdata_table))  {stop("write_plotdata_table must be logical (TRUE/FALSE")}
  if( !is.logical(write_quantiles_table)) {stop("write_quantiles_table must be logical (TRUE/FALSE")}
  if( !is.logical(write_frequency_plot)) {stop("write_frequency_plot must be logical (TRUE/FALSE)")}
  if( !write_frequency_plot_type[1] %in% c("pdf","png")){stop("write_frequency_plot_type must be pdf or png")}
  
  if( !is.numeric(table_nddigits)){      stop("table_nddigits must be numeric")}
  table_nddigits = round(table_nddigits)[1]
  
  # merge the specified na.rm options with my options
  my.na.rm <- list(na.rm.global=TRUE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
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
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  
  # Compute the rolling averagw of interest
  flowdata <- flowdata[ order(flowdata$Date),]
  flowdata <- plyr::ldply(rolling_days, function (x, flowdata){
    # compute the rolling average of x days
    # create a variable to be attached to the statistic
    flowdata$Measure <- paste("Q", formatC(x, width=3, format="d", flag="0"),"-day Mean",sep="")
    flowdata$Q       <- zoo::rollapply( flowdata$Value,  x, mean, fill=NA, align="right")
    flowdata
  }, flowdata=flowdata)
  
  # Truncate the data between the start and end year
  flowdata <- flowdata[ flowdata$AnalysisYear >=start_year & flowdata$AnalysisYear <= end_year,]
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  # Compute the yearly min (unless max flag is set)
  Q_stat <- plyr::ddply(flowdata, c("AnalysisYear","Measure"), function(x,use_max=FALSE, na.rm){
    # compute min or max of Q for the year-measure combination
    value <- ifelse(use_max, max(x$Q,na.rm=na.rm$na.rm.global), min(x$Q,na.rm=na.rm$na.rm.global) )
    data.frame(value=value)
  },use_max=use_max, na.rm=na.rm)
  if(nrow(Q_stat)==0){stop("start_year and end_year eliminated ALL data values")}
  Q_stat <- dplyr::rename(Q_stat,Year=AnalysisYear)
  
  
  if ( !is.na(HYDAT_peaks)) {
  inst_peaks <- suppressMessages(tidyhydat::hy_annual_instant_peaks(HYDAT))
  inst_peaks <- dplyr::filter(inst_peaks,Parameter=="Flow")
  inst_peaks <- dplyr::filter(inst_peaks,PEAK_CODE==HYDAT_peaks)
  inst_peaks <- dplyr::select(inst_peaks,Year=YEAR,Measure=PEAK_CODE,value=Value)
  inst_peaks <- dplyr::mutate(inst_peaks,Measure=paste0("INST_",Measure))
  inst_peaks <- inst_peaks[ inst_peaks$Year >=start_year & inst_peaks$Year <= end_year,]
  inst_peaks <- dplyr::filter(inst_peaks,!(Year %in% exclude_years))
  Q_stat <- inst_peaks
  }
  
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
  if (is.na(HYDAT_peaks)) {plotdata2$Measure <- paste(formatC(as.numeric(substr(plotdata2$Measure,2,4)),width=3),"-day Avg",sep="")}
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
  if(!use_log){ freqplot <- freqplot + ggplot2::scale_y_log10(breaks=scales::pretty_breaks(n=20))}
  if( use_log){ freqplot <- freqplot + ggplot2::scale_y_continuous(breaks=scales::pretty_breaks(n=20))}
  if( use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("ln(Annual Max. Flow (cms))")}  # adjust the Y axis label
  if( use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("ln(Annual Min. Flow (cms))")}
  if(!use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("Annual Max. Flow (cms)")}
  if(!use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("Annual Min. Flow (cms)")}
  
  
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
  
  file_stat_csv <- NA
  if (write_overview | write_stat_table | write_plotdata_table | write_quantiles_table | write_frequency_plot) {
  folder_stat <- paste(report_dir,"/",station_name,"-annual-frequency-analysis",sep = "")
  dir.create(folder_stat)
  }
  if(write_stat_table){
    # Write out the summary table for comparison to HEC spreadsheet
    file_stat_csv <- file.path(folder_stat,paste(station_name,"-annual-vfa-annual-statistics.csv", sep=""))
    temp <- Q_stat_output
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], table_nddigits)  # round the output
    utils::write.csv(temp,file=file_stat_csv, row.names=FALSE)
  }
  
  file_plotdata_csv <- NA
  if(write_plotdata_table){
    # Write out the plotdata for comparison with HEC output
    file_plotdata_csv <- file.path(folder_stat, paste(station_name,"-annual-vfa-plotdata.csv", sep=""))
    utils::write.csv(plotdata,file=file_plotdata_csv, row.names=FALSE)
  }
  
  file_quantile_csv <- NA
  if(write_quantiles_table){
    # Write out the summary table for comparison to HEC spreadsheet
    file_quantile_csv<- file.path(folder_stat, paste(station_name,"-annual-vfa-quantiles.csv", sep=""))
    temp <- fitted_quantiles_output
    temp[,3:ncol(temp)] <- round(temp[,3:ncol(temp)], table_nddigits)  # round the output
    utils::write.csv(temp,file=file_quantile_csv, row.names=FALSE)
  }
  
  file_frequency_plot <- NA
  if(write_frequency_plot){
    file_frequency_plot <- file.path(folder_stat, paste(station_name,"-annual-vfa-frequency-plot.",write_frequency_plot_type[1],sep=""))
    ggplot2::ggsave(plot=freqplot, file=file_frequency_plot, h=4, w=6, units="in", dpi=300)
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
  if (write_overview){
  file_options_csv<- file.path(folder_stat, paste(station_name,"-annual-vfa-overview.csv", sep=""))
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
