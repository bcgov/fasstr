

library(dplyr)
library(tidyr)


calc_longterm_stats_test <- function(flowdata=NULL,
                                     Date=Date,
                                     Value=Value,
                                     HYDAT=NULL,
                                     start_year=NULL,
                                     end_year=NULL){

  # Check if data is provided
  if( is.null(flowdata) & is.null(HYDAT)  ) {stop("no flow data provided, must use flowdata or HYDAT arguments")}
  if( !is.null(flowdata) & !is.null(HYDAT)  ) {stop("only one of flowdata or HYDAT arguments can be used")}
  
  # Get HYDAT data if selected
  if( !is.null(HYDAT) ) {
    if( !all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1])) ) {stop("station in 'HYDAT' parameter does not exist")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # If no STATION_NUMBER, make it so (required for grouping)
  if( !"STATION_NUMBER" %in% colnames(flowdata) ) {
    flowdata$STATION_NUMBER <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if their names differ
  flowdata <- flowdata[,c("STATION_NUMBER",
                          as.character(substitute(Date)),
                          as.character(substitute(Value)))]
  colnames(flowdata) <- c("STATION_NUMBER","Date","Value")
  
  # Fill in missing dates and add dates
  flowdata <- fasstr::fill_missing_dates(flowdata)
  flowdata <- fasstr::add_date_variables(flowdata)
  
  
  
  # Calculate the monthly and longterm stats
  Q_month <-   dplyr::summarize(dplyr::group_by(flowdata,STATION_NUMBER,MonthName),
                                         Mean = mean(Value,na.rm=TRUE),
                                         Median = median(Value,na.rm=TRUE),
                                         Maximum = max(Value,na.rm=TRUE),
                                         Minimum = min(Value,na.rm=TRUE))
  Q_month <- dplyr::ungroup(Q_month)
  Q_all <-   dplyr::summarize(dplyr::group_by(flowdata,STATION_NUMBER),
                                       Mean = mean(Value,na.rm=TRUE),
                                       Median = median(Value,na.rm=TRUE),
                                       Maximum = max(Value,na.rm=TRUE),
                                       Minimum = min(Value,na.rm=TRUE))
  Q_all <- dplyr::mutate(Q_all,MonthName=as.factor("Long-term"))
  Q_longterm <- rbind(Q_month, Q_all)  
  Q_longterm <- dplyr::rename(Q_longterm,Month=MonthName)
  Q_longterm <- with(Q_longterm, Q_longterm[order(STATION_NUMBER,Month),])
  
  # Remove the STATION_NUMBER columns if one wasn't in flowdata originally
  if (unique(flowdata$STATION_NUMBER)=="XXXXXXX") {
    Q_longterm <- dplyr::select(Q_longterm,-STATION_NUMBER)
  }
  
  Q_longterm
  
}


test <- calc_longterm_stats_test(HYDAT = "08NM116")
test <- calc_longterm_stats_test(Value = flow)


flowdata <- ungroup(flowdata)





data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>%  select(-STATION_NUMBER,flow=Value)



  calc_longterm_stats_test()





