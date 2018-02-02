








data <- fasstr::add_date_variables("08HB048")



water_year_start = 10


if(water_year_start %in% c(1, 4, 7, 10)) { 
  
  data <- dplyr::mutate(data, Seasons4 = ifelse(Month <= 3, "Jan-Mar",
                                                ifelse(Month %in% 4:6, "Apr-Jun",
                                                       ifelse(Month %in% 7:9, "Jul-Sep", "Oct-Dec"))))
  
  if(water_year_start %in% c(1,7)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month <= 6, "Jan-Jun", "Jul-Dec"))
  } else if(water_year_start %in% c(4,10)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month %in% 4:9, "Apr-Sep", "Oct-Mar"))
  }
  
}

if(water_year_start %in% c(2, 5, 8, 11)) { 
  
  data <- dplyr::mutate(data, Seasons4 = ifelse(Month %in% c(1, 11:12), "Nov-Jan",
                                                ifelse(Month %in% 2:4, "Feb-Apr",
                                                       ifelse(Month %in% 5:7, "May-Jul", "Aug-Oct"))))
  if(water_year_start %in% c(2,8)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month %in% 2:7, "Feb-Jul", "Aug-Jan"))
  } else if(water_year_start %in% c(5,11)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month %in% c(5:10), "May-Oct", "Nov-Apr"))
  }
  
}

if(water_year_start %in% c(3, 6, 9, 12)) { 
  
  data <- dplyr::mutate(data, Seasons4 = ifelse(Month %in% c(12, 1:2), "Dec-Feb",
                                                ifelse(Month %in% 3:5, "Mar-May",
                                                       ifelse(Month %in% 6:8, "Jun-Aug", "Sep-Nov"))))
  
  if(water_year_start %in% c(3,9)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month %in% 3:8, "Mar-Aug", "Sep-Apr"))
  } else if(water_year_start %in% c(6,12)) {
    data <- dplyr::mutate(data, Seasons2 = ifelse(Month %in% c(6:11), "Jun-Nov", "Dec-May"))
  }
  
}



