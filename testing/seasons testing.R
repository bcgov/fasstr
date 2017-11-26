flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=T #create another for own water year????
water_year_start=2
start_year=NULL
end_year=NULL
exclude_years=c(1972,1973) # list of stations
basin_area=NA # if na, then all Yield values == NA
transpose=FALSE
write_table=FALSE        # write out statistics on calendar year
report_dir="."
na.rm=list(na.rm.global=FALSE)
table_nddigits=3



# Remove first or last year if all NA
if (all(is.na(Qstat[1,-1])) & Qstat[1,1] %in% exclude_years) {
  Qstat_2 <- Qstat[-1,]
}
if (all(is.na(Qstat[nrow(Qstat),-1]))) {Qstat <- Qstat[-nrow(Qstat),]}
#######################################

# Message if start.end years are <> data
if (!is.null(start_year)) {
  if (start_year < min_year) {message("start_year (",start_year,") is less than the first ",ifelse(water_year,paste0("water year"),paste0("year"))," of the data (",min_year,"), ",min_year," will be used for start_year")}
}
if (!is.null(end_year)) {
  if (end_year > max_year) {message("end_year (",end_year,") is greater than the last ",ifelse(water_year,paste0("water year"),paste0("year"))," of the data (",max_year,"), ",max_year," will be used for end_year")}
}

###################

seasons_flowdata <- dplyr::mutate(flowdata,
                                  Seasons4= ifelse(Month<=3,"JFM",
                                                   ifelse(Month>=4&Month<=6,"AMJ",
                                                          ifelse(Month>=7&Month<=9,"JAS",
                                                                 ifelse(Month>=10,"OND",NA)))),
                                  Seasons2=ifelse(Month<=3|Month>=10,"ONDJFM",
                                                  ifelse(Month>=4&Month<=9,"AMJJAS",NA)),
                                  Seasons2_year=ifelse(flowdata$Month>=10,
                                                       flowdata$Year+1,
                                                       flowdata$Year),
                                  Seasons4_year=Year)

Seasons2_values <- dplyr::summarise(dplyr::group_by(data,Seasons2,Seasons2_year),
                                  TOTALQ_DAILY=mean(Value, na.rm=F)*length(Value)*60*60*24,
                                  YIELDMM_DAILY=TOTALQ_DAILY /basin_area/1000,
                                  Max_year=max(AnalysisYear)#,
                                  #Min_year=min(AnalysisYear)
                                  )
Seasons2_values <- dplyr::select(Seasons2_values,Year=Max_year,Seasons2,TOTALQ_DAILY,YIELDMM_DAILY)
Seasons4_values <- dplyr::summarise(dplyr::group_by(data,Seasons4,Seasons4_year),
                                  TOTALQ_DAILY=mean(Value, na.rm=F)*length(Value)*60*60*24,
                                  YIELDMM_DAILY=TOTALQ_DAILY*60*60*24 /basin_area/1000, 
                                  Max_year=max(AnalysisYear)#,
                                  #Min_year=min(AnalysisYear)
                                  )
Seasons4_values <- dplyr::select(Seasons4_values,Year=Max_year,Seasons4,TOTALQ_DAILY,YIELDMM_DAILY)




#data <- dplyr::mutate(data,
#                      season_year=pmax(Year,WaterYear))




#