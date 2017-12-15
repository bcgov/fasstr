

basin_area=NA
basin_area=10000

basin_area <- c("08HB048"=966,"08NM116"=NA)
basin_area <- c("08NM116"=750)
basin_area <- c("XXXX"=750)


flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116"))
flow_data <- dplyr::select(flow_data,Date,Value)
flow_data$STATION_NUMBER <- "XXXXXXX"


if(all(is.na(basin_area))){
  basin_stations <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER))
  basin_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = basin_stations$STATION_NUMBER))
  basin_HYDAT <- dplyr::select(basin_HYDAT, STATION_NUMBER, Basin_Area_sqkm = DRAINAGE_AREA_GROSS)
  basin_area_table <- merge(basin_HYDAT, basin_stations, by = "STATION_NUMBER", all.y = TRUE)
}
if(!all(is.na(basin_area))){
  if(!is.numeric(basin_area)) stop("basin_area arguments must be numeric.")
  if(is.null(names(basin_area)) & length(basin_area) == 1) {
    if(length(unique(flow_data$STATION_NUMBER)) > 1) warning("Just one basin_area area applied without a corresponding STATION_NUMBER, the basin_area will be applied to all stations.")
    basin_area_table <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), Basin_Area_sqkm = basin_area)
  } else {
    if(length(basin_area)!=length(unique(flow_data$STATION_NUMBER)) | !all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) 
      warning("The number/names of STATION_NUMBERS and basin_area values provided do not match the number/names of STATION_NUMBERS in the flow data. Only those that match will be applied.")
    #if(!all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) warning("All STATION_NUMBERS listed in basin_area do not match those in the flow data. Only those that match will be applied.")
    basin_area_table <- data.frame(STATION_NUMBER = names(basin_area), Basin_Area_sqkm = basin_area)
  }
}
if(all(is.na(basin_area_table$Basin_Area_sqkm))) warning("No basin_area values provided or extracted from HYDAT. All Yield_mm values will be NA.")




flow_data <- merge(flow_data, basin_area_table, by = "STATION_NUMBER", all.x = TRUE)







basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = c("08HB048","08NM116"))[,c(1,9)]) 


basin <- c("08HB048"=10,"08NM116"=750)


flow_data$basin <- basin[match(flow_data$STATION_NUMBER,names(basin))]


basin2 <- data.frame(STATION_NUMBER=names(basin),
                     basin_area=basin)
flow_data <- merge(flow_data,basin2,by="STATION_NUMBER")

flowdata <- dplyr::select(flow_data,Date,Value) %>%
  mutate(STATION_NUMBER="XXXX")





stations =c("XXXX","08HB048")# <- unique(flowdata$STATION_NUMBER)
basin2 <- data.frame(STATION_NUMBER=stations)
basin_area_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = stations)[,c(1,9)]) %>% 
  rename(basin_area=DRAINAGE_AREA_GROSS)
basin3 <- merge(basin2,basin_area_HYDAT, by="STATION_NUMBER", all.x = T)


basin <- c(STATION_NUMBER=c("08HB048","08NM116"), basin_area=c(10.1,975))
flow_data$basin <- basin[match(flow_data$STATION_NUMBER,names(basin))]
basin
