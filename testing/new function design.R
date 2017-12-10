

library(dplyr)
library(tidyr)


calc_longterm_stats_test <- function(flow_data=NULL,
                                     flow_dates=Date,
                                     flow_values=Value,
                                     HYDAT=NULL,
                                     start_year=0,
                                     end_year=9999#,
                                    # NARM*********
                                       ){
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if( is.null(flow_data) & is.null(HYDAT) )   stop("no flow data provided, must use flowdata or HYDAT arguments")
  if( !is.null(flow_data) & !is.null(HYDAT) ) stop("only one of flowdata or HYDAT arguments can be used")
  
  # Get HYDAT data if selected and exists
  if( !is.null(HYDAT) ) {
    if( !all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1])) ) stop("station in 'HYDAT' parameter does not exist")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # If no STATION_NUMBER, make it so (required for grouping)
  if( !"STATION_NUMBER" %in% colnames(flow_data) ) {
    flow_data$STATION_NUMBER <- "XXXXXXX"
  }
  
  # Get the just STATION_NUMBER, Date, and Value columns
  # This method allows the user to select the Date or Value columns if their names differ
  if( !as.character(substitute(flow_dates)) %in% names(flow_data) )  
    stop("Flow dates not found. Rename flow dates column to 'Date' or name the column using 'flow_dates' argument.")
  if( !as.character(substitute(flow_values)) %in% names(flow_data) ) 
    stop("Flow values not found. Rename flow values column to 'Value' or name the column using 'flow_values' argument.")
  
  flow_data <- flow_data[,c("STATION_NUMBER",
                          as.character(substitute(flow_dates)),
                          as.character(substitute(flow_values)))]
  colnames(flow_data) <- c("STATION_NUMBER","Date","Value")
  
  if( !inherits(flow_data$Date[1], "Date") )  stop("'Date' column in flowdata data frame is not a date")
  if( !is.numeric(flow_data$Value))           stop("'Value' column in flowdata data frame is not numeric")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  
  
  flowdata <- flow_data
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




write_calc <- function(flowdata,
                       HYDAT="08HB048",
                       fasstr_function=fasstr::calc_longterm_stats(HYDAT = HYDAT)){
  
  write.csv(x=fasstr_function,
            file = "testing.csv",
            row.names = FALSE)
}



test <- calc_longterm_stats_test(HYDAT = "08NM116")
test <- calc_longterm_stats_test(flow_data = data,
                                 flow_dates = Date)


flowdata <- ungroup(flowdata)

{if("STATION_NUMBER" %in% names(data)) STATION_NUMBER else NULL}
data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% select(-STATION_NUMBER) %>% 
  group_by(NULL) %>% 
  summarise(Mean=mean(Value))


data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>%  select(-STATION_NUMBER,flow=Value)





data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) 
data <- dplyr::group_by(data,STATION_NUMBER)
data <- dplyr::summarise(data,MEAN=mean(Value, na.rm = T))







calc_longterm_stats_test()




write_calc(HYDAT = "08HB048")
