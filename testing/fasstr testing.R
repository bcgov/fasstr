

devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr", repos = NULL, type = "source",)
install.packages("C:/Users/jgoetz/R/fasstr_devel",repos = NULL, type = "source", build_vignettes = TRUE)
devtools::install_github("bcgov/fasstr", ref = "devel",  force = TRUE)
devtools::install_github("bcgov/fasstr",  build_vignettes = TRUE, force = TRUE)
remotes::install_github("bcgov/fasstr", , force = TRUE)
4444#devtools::check()

data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
data2 <- analysis_prep(data,  1)

library(fasstr)

t <- calc_monthly_cumulative_stats(station_number = "08HB048", start_year = 1980)

t <- compute_frequency_quantile(station_number = "08HB048", water_year_start = 1, roll_days = 7, return_period = 10)


flow_data <- tidyhydat::hy_daily_flows("08LE019")
percentiles = c(5,25,75,95)
use_yield = FALSE
basin_area = NA
water_year_start = 1
roll_days = c(1, 3, 7, 30)
roll_align = "right"
exclude_years = NULL
months = 1:12
start_year = 0
end_year =9999
ignore_missing=F
include_longterm=T
transpose = FALSE



plot_missing_dates(station_number = "08NM116", start_year = 1990)
library(fasstr)


t1 <- Sys.time()
plot_flow_duration(station_number = "08HB048", start_year = 1973)
t2 <- Sys.time()
t2-t1

all <- compute_full_analysis(station_number = "08HB048", sections = 3)

s <- plot_annual_cumulative_stats(station_number = "08HB048", include_seasons = TRUE)
s <- calc_annual_cumulative_stats(station_number = "08HB048", include_seasons = TRUE, 
                                  months = 1:6, use_yield =F)

all <- calc_all_annual_stats(station_number = "08HB048")

t <- add_seasons_new(station_number = "08HB048", seasons_length = 4, water_year = TRUE, water_year_start = 12) %>% 
  add_seasons_new(seasons_length = 6)
str(t)


add_seasons(station_number = "08HB048")

calc_annual_cumulative_stats(station_number = "08HB048", months = c(12,1), water_year = TRUE, water_year_start = 12)





l






add_daily_volume(station_number = "08HB048") %>% 
  write_results("volumetesting.xlsx")



library(fasstr)

test <- compute_full_analysis(station_number = "08HB048", start_year = 1973, sections = 5)

low_flows <- calc_annual_lowflows(station_number = "08NM116", 
                                  start_year = 1980, 
                                  end_year = 2000,
                                  roll_days = 7)
low_flows <- dplyr::select(low_flows, Year, Value = Min_7_Day)
low_flows <- dplyr::mutate(low_flows, Measure = "7-Day")

test <- compute_frequency_analysis(data = low_flows,
                           events = Year,
                           values = Value,
                           measure = Measure,
                           use_log = FALSE)
test$Freq_Fitted_Quantiles

low_flows <- calc_annual_lowflows(station_number = "08NM116", 
                                  start_year = 1980, 
                                  end_year = 2000,
                                  roll_days = 7)
low_flows <- dplyr::select(low_flows, Year, Value = Min_7_Day)
low_flows <- dplyr::mutate(low_flows, Measure = "7-Day")
test <- compute_frequency_analysis(data = low_flows)
test[[3]]



Q_stat <- add_date_variables(station_number = "08HB048") %>% 
  add_rolling_means(roll_days = 7) %>% 
  filter(DayofYear == 200) %>% 
  select(Year, value = Q7Day) %>% 
  mutate(Year = as.character(Year),
         Measure22 = "7-day") %>% 
  rename(TESTING=Year)

Q_stat2 <- Q_stat[1,]
Q_stat2$TESTING = "1988b"
Q_stat2$value = 100
Q_stat2$Measure22 = "7-day"


Q_stat <- bind_rows(Q_stat, Q_stat2)

test <- compute_frequency_analysis(data = Q_stat,
                                   events = TESTING,
                                   values = value,
                                   measures = Measure22)

test2 <- compute_annual_frequencies(station_number = "08HB048",
                                    plot_curve = TRUE)
test3 <- compute_hydat_peak_frequencies(station_number = "08HB048")[[3]]


library(fasstr)

library(dplyr)

bc_stns <- tidyhydat::hy_stn_data_range(prov_terr_state_loc = "BC") %>% 
  filter(DATA_TYPE == "Q",
         RECORD_LENGTH > 19) %>% 
  pull(STATION_NUMBER)

bc_stns2 <- tidyhydat::hy_annual_stats(station_number = bc_stns) %>% 
  filter(Parameter == "Flow",
         Sum_stat == "MEAN") %>% 
  group_by(STATION_NUMBER) %>% 
  summarise(n_year = sum(!is.na(Value))) %>% 
  filter(n_year > 19) %>% 
  pull(STATION_NUMBER)


#stns <- c("08HB048","08NM116")

time_1 <- Sys.time()
stat <- data.frame()
for (i in bc_stns2) {
  
  MAD <- calc_lt_mad(station_number = i, 
                     complete_years = TRUE,
                     percent_MAD = c(5,20))
  colnames(MAD) <- c("STATION_NUMBER", "LTMAD","MAD_5p", "MAD_20p")
  
  lowflow_1d <- compute_frequency_quantile(station_number = i,
                                        return_period = 5,
                                        roll_days = 1)
  lowflow_1d <- data.frame(STATION_NUMBER = i,
                        LF_1d = lowflow_1d)
  lowflow_30d <- compute_frequency_quantile(station_number = i,
                                           return_period = 5,
                                           roll_days = 30)
  lowflow_30d <- data.frame(STATION_NUMBER = i,
                           LF_30d = lowflow_30d)
  
  
  
  stat_stn <- merge(MAD, lowflow_1d, by = "STATION_NUMBER")
  stat_stn <- merge(stat_stn, lowflow_30d, by = "STATION_NUMBER")
  
  stat_stn$LTMAD_ptile <- as.numeric(calc_flow_percentile(station_number = i, 
                                                           complete_years = TRUE,
                                                           flow_value = stat_stn$LTMAD)[2])
  stat_stn$MAD_5p_ptile <- as.numeric(calc_flow_percentile(station_number = i, 
                                        complete_years = TRUE,
                                        flow_value = stat_stn$MAD_5p)[2])
  stat_stn$MAD_20p_ptile <- as.numeric(calc_flow_percentile(station_number = i, 
                                           complete_years = TRUE,
                                           flow_value = stat_stn$MAD_20p)[2])
  stat_stn$LF_1d_ptile <- ifelse(is.na(stat_stn$LF_1d), NA,
                              as.numeric(calc_flow_percentile(station_number = i, 
                                              complete_years = TRUE,
                                              flow_value = stat_stn$LF_1d)[2]))
  stat_stn$LF_30d_ptile <- ifelse(is.na(stat_stn$LF_30d), NA,
                                 as.numeric(calc_flow_percentile(station_number = i, 
                                                                 complete_years = TRUE,
                                                                 flow_value = stat_stn$LF_30d)[2]))
  
  
  stat <- rbind(stat, stat_stn)
}
time_2 <- Sys.time()


plotly::ggplotly(ggplot(data = stat, aes(x= MAD_20p, y= LF_30d))+
  geom_point())



stats2 <- stat %>% 
  mutate(area = substr(STATION_NUMBER, 1,3),
         ratio = LF_1d / MAD_20p)#%>% 
 # filter(ratio>=1)



ggplot(data = stats2)+
  geom_boxplot(aes(x=area, y=ratio))


#%>% 
 # filter(MAD_20p < 100)

plotly::ggplotly(ggplot(data = stats2, aes(x= MAD_20p, y= LF_30d, color = area))+
  geom_smooth(method = "lm", alpha = 0.5)+
geom_point())

stns <- c("08MA002","08MA001","08LF002","08LG010","08LF027")
stns <- c("08LG006")


for (i in stns) {
  data <- add_date_variables(station_number = i)
  data <- add_rolling_means(data, roll_days = 30)
  fasstr::write_flow_data(data)
}


data <- add_date_variables(station_number = stns)
data <- add_rolling_means(data, roll_days = 30)
fasstr::write_flow_data(data)

dat <- tidyhydat::hy_stations(station_number = stns)

min_30 <- calc_annual_lowflows(station_number = stns, roll_days = 30, months = 7:9, start_year = 1978, end_year = 2015)

### Westwold
t <- screen_flow_data(station_number = "08LE020")
falkland_trend <- fasstr::compute_full_analysis(station_number = "08LE020", water_year = TRUE,
                                          start_year = 1967, end_year = 2015,
                                          write_to_dir = T)#, foldername = "08LE020",
                                          #sections = 6, zyp_alpha = 0.05)




plot_flow_data(station_number = "08LE068", start_year = 1976, end_year = 1976)

lake <- tidyhydat::hy_daily_levels("08LE068")
plot_flow_data(lake)

plot_daily_stats(station_number = "08LE020", complete_years = T, log_discharge = F, water_year = T)

library(FlowScreen)

data <- tidyhydat::hy_daily_flows("08LE020") %>% 
 # fill_missing_dates() %>% 
  rename(ID = STATION_NUMBER,
         Flow = Value,
         SYM = Symbol) %>% 
  select(-Parameter) %>% 
  mutate(Agency = "WSC",
         PARAM = 1)
ts <- create.ts(data)
ts <- ts %>% 
  filter(hyear >= 1976)

regime(ts)
metrics <- metrics.all(ts)

jknjkn <- pk.cov(ts)



###
test <- fasstr::compute_annual_trends(station_number = "08NM116",
                                      zyp_method = "yuepilon",
                                      start_year = 1973, end_year = 2013)
freq <- compute_annual_frequencies(station_number = "08NA024")


library(fasstr)
library(dplyr)
start_time <- Sys.time()
test <- compute_full_analysis(data = flow_data,
                              #station_number = "08HB048", 
                               water_year = FALSE, 
                               #start_year = 2008, 
                               #end_year = 2017, 
                               #exclude_years = c(1982:1985, 1987),
                              # table_filetype = "csv",
                               #plot_filetype = "png",,
                             #  sections = c(1),
                               write_to_dir = TRUE,
                               foldername = "Bertrand",
                               ignore_missing = F,
                             zyp_alpha = 0.05)
end_time <- Sys.time()


flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% dplyr::select(-STATION_NUMBER)
test <- compute_full_analysis(data = flow_data, ignore_missing = TRUE, foldername = "TEST FOLDER", write_to_dir = T, sections = 3, plot_filetype = "pdf")

test <- compute_full_analysis(station_number = "08HB048", foldername = "Carnation", write_to_dir = F, start_year = 1973)
end_time <- Sys.time()







bert <- compute_annual_trends(flow_data, zyp_method = "yuepilon", zyp_alpha = 0.05)



freq <- compute_annual_frequencies(station_number = "08HB048")



write_objects_list(freq, foldername = "testt", plot_type = "png", table_type = "xlsx")


dirs <- list.files(path = "Carn/")
data <- data.frame()
for (i in dirs) {
  subdirs <- list.files(path = paste0("Carn/", i))
  
  dir_data <- data.frame("Subdirectory" = subdirs)
  dir_data$Directory <- i
  data <- rbind(data,dir_data)
}


data$fileExt <- sub('.*\\.', '', data$Subdirectory)
data$FileType <- ifelse(data$fileExt %in% c("xlsx", "xls", "csv"), "Table", 
                    ifelse(data$fileExt %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg"), "Plot",
                           "Folder with Plots"))

data <- data[,c(2,1,4)]



library(fasstr)
library(ggplot2)
HistoricQ_07CD001 <-plot_daily_stats(station_number = "07CD001",
                                     start_year = 1957,
                                     end_year = 2016,
                                     months = 4:11,
                                     log_discharge = TRUE,
                                     include_year = 2016,
                                     ignore_missing = TRUE,
                                     include_title = FALSE)
HistoricQ_07CD001$Daily_Stats + 
  ggtitle("Clearwater River at Draper")+
  ggplot2::scale_y_log10(expand = c(0, 0), breaks = scales::log_breaks(n = 5, base = 10), bre)
  
  
  ggplot2::theme(axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                 axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                 axis.ticks = ggplot2::element_line(size = .1, colour = "grey25"),
                 axis.ticks.length = ggplot2::unit(0.05, "cm"),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,0,0,0)),
                 panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(size = .1),
                 legend.text = ggplot2::element_text(size = 5, colour = "grey25"),
                 legend.title = ggplot2::element_text(size = 7, colour = "grey25"),
                 legend.box = "vertical",
                 legend.justification = "top",
                 legend.key.size = ggplot2::unit(0.4, "cm"),
                 legend.spacing = ggplot2::unit(0, "cm"))
ggsave("lewis_plot.png", plot, height = 8, width = 14)
ggsave("lewis_plot.png", plot, height = 4, width = 7)




plot <- 


test <- calc_daily_stats(stati="08HB048", ignore_missing = TRUE)
  test$dates2 <- as.Date.character(test$Date, "%b-%d")
test <- test %>% 
  add_date_variables(dates = "dates2")

test

x <- list(data = NULL,
              dates = as.character(substitute(Date)),
              values = as.character(substitute(Value)),
              groups = as.character(substitute(STATION_NUMBER)),
              station_number = NULL,
              foldername = NULL,
              sections = 1:7,
              table_filetype = "xlsx",
              plot_filetype = "png",
              basin_area = NA,
              water_year = FALSE,
              water_year_start = 10,
              start_year = 0,
              end_year = 3000,
              exclude_years = NULL,
              ignore_missing = FALSE,
              zyp_method = 'yuepilon',
              zyp_alpha = NA,
              write_to_dir = FALSE)



x <- data.frame("Argument" = names(x),
           "Option" = as.character(unname(x)))
x <- x[c(1,5,2:4,6:nrow(x)),]



# metadata <- list(data = as.character(substitute(data)),
#                  dates = as.character(substitute(Date)),
#                  values = as.character(substitute(Value)),
#                  groups = as.character(substitute(STATION_NUMBER)),
#                  station_number = station_number,
#                  foldername = foldername,
#                  sections = sections,
#                  table_filetype = table_filetype,
#                  plot_filetype = plot_filetype,
#                  basin_area = basin_area,
#                  water_year = water_year,
#                  water_year_start = water_year_start,
#                  start_year = start_year,
#                  end_year = end_year,
#                  exclude_years = exclude_years,
#                  ignore_missing = ignore_missing,
#                  zyp_method = zyp_method,
#                  zyp_alpha = zyp_alpha,
#                  write_to_dir = write_to_dir)
# 
# metadata <- data.frame("Argument" = names(metadata),
#                        "Option" = as.character(unname(metadata)))
# 
# write_results(data = metadata,
#               file = paste0(main_dir, "MetaData.", table_filetype))

# return(list("Screening" = list("Daily_Flows" = flow_data,
#                                "Daily_Flows_Plot" = ts_full_plot,
#                                "Daily_Flows_by_Year_Plot" = ts_annual_plot,
#                                "Flow_Screening" = flow_screening,
#                                "Flow_Screening_Plot" = ts_screen_plot,
#                                "Missing_Dates_Plot" = ts_missing_plot),
#             "Longterm" = list("Longterm_Summary_Stats_Percentiles" = lt_stats,
#                               "Longterm_Summary_Stats_Plot" = lt_stats_plot,
#                               "Flow_Duration_Curves" = lt_flowduration_plot),
#             "Annual" = list("Annual_Summary_Stats" = ann_stats,
#                             "Annual_Summary_Stats_Plot" = ann_stats_plot,
#                             "Annual_Cumul_Volume_Stats_m3" = ann_vol,
#                             "Annual_Cumul_Volume_Stats_m3_Plot" = ann_vol_plot,
#                             "Annual_Cumul_Yield_Stats_mm" = ann_yield,
#                             "Annual_Cumul_Yield_Stats_mm_Plot" = ann_yield_plot,
#                             "Annual_Flow_Timing" = ann_timing,
#                             "Annual_Flow_Timing_Plot" = ann_timing_plot,
#                             "Annual_Days_Outside_Normal" = ann_norm,
#                             "Annual_Days_Outside_Normal_Plot" = ann_norm_plot,
#                             "Annual_Low_Flows" = ann_lowflow,
#                             "Annual_Low_Flows_Plot" = ann_lowflow_plot),
#             "Monthly" = list("Monthly_Summary_Stats" = mon_stats,
#                              "Monthly_Summary_Stats_Plot" = mon_stats_plot,
#                              "Monthly_Total_Cumul_Volumes_m3" = mon_vol,
#                              "Monthly_Total_Cumul_Volumes_m3_Plot" = mon_vol_plot,
#                              "Monthly_Total_Cumul_Yield_mm" = mon_yield,
#                              "Monthly_Total_Cumul_Yield_mm_Plot" = mon_yield_plot),
#             "Daily" = list("Daily_Summary_Stats" = day_stats,
#                            "Daily_Summary_Stats_Plot" = day_stats_plot,
#                            "Daily_Total_Cumul_Volumes_m3" = day_vol,
#                            "Daily_Total_Cumul_Volumes_m3_Plot" = day_vol_plot,
#                            "Daily_Total_Cumul_Yield_mm" = day_yield,
#                            "Daily_Total_Cumul_Yield_mm_Plot" = day_yield_plot,
#                            "Daily_Total_Cumul_Volumes_m3_with_Years" = day_stats_year_plots,
#                            "Daily_Summary_Stats_with_Years" = day_vol_year_plots,
#                            "Daily_Total_Cumul_Yield_mm_with_Years" = day_yield_year_plots),
#             "Trending" = list("Annual_Trends_Data" = ann_data,
#                               "Annual_Trends_Results" = ann_results,
#                               "Annual_Trends_Plots" = ann_trends_plots),
#             "Lowflow_Frequencies" = list()
# )
# )



list.dirs()

class(test[[1]][[1]])

## Attributes


# Recheck if station_number/grouping was in original data and rename or remove as necessary
if(as.character(substitute(groups)) %in% orig_cols) {
  names(annual_stats)[names(annual_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
} else {
  annual_stats <- dplyr::select(annual_stats, -STATION_NUMBER)
}

if (is.null(station_number)) {
  attr(annual_stats, "data.source") <- paste0("data = ", as.character(substitute(data)))
} else {
  attr(annual_stats, "data.source") <- paste0("station_number = ", paste0(station_number))
}




attr(annual_stats, "rolling.days") <- paste0("roll_days = ", roll_days, " & roll_align = ", roll_align)
attr(results, "year.period") <- ifelse(!water_year | (water_year & water_year_start == 1), "Jan-Dec",
                                            ifelse(water_year_start == 2, "Feb-Jan",
                                                   ifelse(water_year_start == 3, "Mar-Feb",
                                                          ifelse(water_year_start == 4, "Apr-Mar",
                                                                 ifelse(water_year_start == 5, "May-Apr",
                                                                        ifelse(water_year_start == 6, "Jun-May", 
                                                                               ifelse(water_year_start == 7, "Jul-Jun", 
                                                                                      ifelse(water_year_start == 8, "Aug-Jul", 
                                                                                             ifelse(water_year_start == 9, "Sep-Aug",
                                                                                                    ifelse(water_year_start == 10, "Oct-Sep",
                                                                                                           ifelse(water_year_start == 11, "Nov-Oct", "Dec-Nov")))))))))))


# data source
# rolling days and alignment
# water year and month
# start, end, excluded years
# months
# ignore missing












daily_stats <- dplyr::summarize(dplyr::group_by(test, STATION_NUMBER, AnalysisDate, AnalysisDoY),
                                Mean = ifelse mean(Cumul_Flow, na.rm = TRUE),
                                Median = stats::median(Cumul_Flow, na.rm = TRUE),
                                Minimum = min(Cumul_Flow, na.rm = TRUE),
                                Maximum = max(Cumul_Flow, na.rm = TRUE))

test <- plot_daily_stats(station_number = "08HB048", include_year = 1999, ignore_missing = T)

test2 <- test$data

results <- calc_annual_lowflows(station_number = "08HA066", ignore_missing = F)
results <- dplyr::select(results, Min_1_Day, Min_3_Day, Min_7_Day, Min_30_Day)
any(as.numeric(colSums(!is.na(results))) < 3)



logical_cols <- sapply(test, is.logical) 
test[logical_cols] <- lapply(test[logical_cols], as.numeric) 


for (i in names(test)) {
  test[[i]] <- test[[i]] + ggplot2::ylab("YAHOOO")
}



flow_data <- read.csv("testing/08MH152_with_USA_DATA.csv") %>% 
  fill_missing_dates(groups = ID) %>% 
  add_basin_area(groups = ID) %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means(groups = ID) %>%
  add_daily_volume() %>%
  add_cumulative_volume(groups = ID) %>% 
  add_daily_yield(basin_area = 10.3, groups = ID) %>%
  add_cumulative_yield(basin_area = 10.3, groups = ID) %>% 
  add_seasons()

str(data)
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


# Writes all data and plots


library(fasstr)


flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
start_year = 1980
end_year = 2010
water_year = FALSE
water_year_start = 9
exclude_years = 1990:1993
#include_year = 2000
folder = "testing/Carnation/"
main_dir = folder
table_filetype = "xlsx"
plot_filetype = "png"

# 
# folder <- "testing/MissionCreek/"
# # Parameters
# stn_number <- "08NM116"
# start_year = 1981 #NULL
# end_year = 2000 #NULL

t <- compute_full_analysis(station_number = "08HB048", start_year = 1980, write_to_dir = T, foldername = "Carn_test")



devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr", repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")


start_time <- Sys.time()
write_full_analysis(data = flow_data,
                    groups = ID,
                    #water_year = TRUE, 
                    start_year = 2007, 
                    #end_year = 2010, 
                    #exclude_years = c(1995:1997, 1999),
                    #table_filetype = "xlsx",
                    #plot_filetype = "png",
                    foldername = "Bertrand"#,
                   # ignore_missing = TRUE,
                    #sections = 7
                   )
end_time <- Sys.time()



devtools::install_github("bcgov/fasstr")
library(fasstr)
write_full_analysis(station_number = "08HB048", 
                    #water_year = TRUE, 
                    #start_year = 1980, 
                    #end_year = 2010, 
                    #exclude_years = c(1995:1997, 1999),
                    #table_filetype = "xlsx",
                    #plot_filetype = "png",
                    foldername = "Carn",
                    ignore_missing = TRUE)



##


### FLOW_DATA
### ----------

library(fasstr)
library(dplyr)



# One station with Date and Value
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()

results <- screen_flow_data(data = flow_data)
results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data)
results <- calc_monthly_stats(data = flow_data)


plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, include_seasons = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_annual_stats(data = flow_data, values = Volume_m3)
plot_annual_stats(data = flow_data, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data)
plot_daily_stats(data = flow_data)
plot_daily_stats(data = flow_data, values = Volume_m3)
plot_daily_stats(data = flow_data, values = Yield_mm)
plot_data_screening(data = flow_data)
plot_data_screening(data = flow_data, values = Volume_m3)
plot_data_screening(data = flow_data, values = Yield_mm)
plot_flow_duration(data = flow_data)
plot_flow_duration(data = flow_data, values = Volume_m3)
plot_flow_duration(data = flow_data, values = Yield_mm)
plot_longterm_stats(data = flow_data)
plot_longterm_stats(data = flow_data, values = Volume_m3)
plot_longterm_stats(data = flow_data, values = Yield_mm)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data)
plot_monthly_stats(data = flow_data)
plot_monthly_stats(data = flow_data, values = Volume_m3)
plot_monthly_stats(data = flow_data, values = Yield_mm)
plot_annual_cumulative_stats(data = flow_data, include_seasons = T)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon")

# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss = Date, Valuesss = Value) %>% 
  fill_missing_dates(dates = Datesss, values = Valuesss, groups = Station) %>% 
  add_basin_area(groups = Station) %>% 
  add_date_variables(dates = Datesss) %>% 
  add_rolling_means(dates = Datesss, values = Valuesss) %>% 
  add_daily_volume(values = Valuesss) %>% 
  add_cumulative_volume(dates = Datesss, values = Valuesss) %>% 
  add_daily_yield(values = Valuesss) %>%
  add_cumulative_yield(dates = Datesss, values = Valuesss) %>% 
  add_seasons(dates = Datesss)

results <- calc_longterm_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_all_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss, include_seasons = T)
results <- calc_annual_flow_timing(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_lowflows(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_outside_normal(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_daily_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_daily_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801, dates = Datesss, values = Valuesss)
results <- calc_lt_mad(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_lt_percentile(data = flow_data, percentiles =  50, dates = Datesss, values = Valuesss)
results <- calc_monthly_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_monthly_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- screen_flow_data(data = flow_data, dates = Datesss, values = Valuesss)

plot_flow_data(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_cumulative_stats(data = flow_data, include_seasons = T, dates = Datesss, values = Valuesss)
plot_annual_flow_timing(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_outside_normal(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_stats(data = flow_data, dates = Datesss, values = Volume_m3)
plot_annual_stats(data = flow_data, dates = Datesss, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_daily_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_daily_stats(data = flow_data, dates = Datesss, values = Volume_m3)
plot_daily_stats(data = flow_data, dates = Datesss, values = Yield_mm)
plot_data_screening(data = flow_data, dates = Datesss, values = Valuesss)
plot_data_screening(data = flow_data, dates = Datesss, values = Volume_m3)
plot_data_screening(data = flow_data, dates = Datesss, values = Yield_mm)
plot_flow_duration(data = flow_data, dates = Datesss, values = Valuesss)
plot_flow_duration(data = flow_data, dates = Datesss, values = Volume_m3)
plot_flow_duration(data = flow_data, dates = Datesss, values = Yield_mm)
plot_longterm_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_longterm_stats(data = flow_data, dates = Datesss, values = Volume_m3)
plot_longterm_stats(data = flow_data, dates = Datesss, values = Yield_mm)
plot_missing_dates(data = flow_data, dates = Datesss, values = Valuesss)
plot_monthly_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_monthly_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_monthly_stats(data = flow_data, dates = Datesss, values = Volume_m3)
plot_monthly_stats(data = flow_data, dates = Datesss, values = Yield_mm)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon",dates = Datesss, values = Valuesss)


# Station no STATION_NUMBER
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  fill_missing_dates() %>%
  add_basin_area(basin_area = 10.555) %>%
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()

results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_monthly_stats(data = flow_data)
results <- screen_flow_data(data = flow_data)

plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, include_seasons = T, use_yield = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_daily_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_daily_stats(data = flow_data, include_year = 1990, complete_years = T, include_title = T)
plot_data_screening(data = flow_data)
plot_flow_duration(data = flow_data)
plot_longterm_stats(data = flow_data)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_monthly_stats(data = flow_data)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon")




### HYDAT
### ----------

# Single stations
flow_data <- fill_missing_dates(station_number = "08LE019") 
flow_data <- add_basin_area(station_number = "08LE019")
flow_data <- add_seasons(station_number = "08LE019", seasons_length = 6)
flow_data <- add_date_variables(station_number = "08LE019", water_year = 2)
flow_data <- add_rolling_means(station_number = "08LE019")
flow_data <- add_daily_volume(station_number = "08LE019")
flow_data <- add_cumulative_volume(station_number = "08LE019")
flow_data <- add_daily_yield(station_number = "08LE019")
flow_data <- add_cumulative_yield(station_number = "08LE019", basin_area = 10.2)

results <- calc_longterm_stats(station_number = "08LE019")
results <- calc_annual_stats(station_number = "08LE019", percentiles = 1:20)
results <- calc_all_annual_stats(station_number = "08LE019")
results <- calc_annual_cumulative_stats(station_number = "08LE019", use_yield = T, include_seasons = T)
results <- calc_annual_flow_timing(station_number = "08LE019", water_year = 1)
results <- calc_annual_lowflows(station_number = "08LE019")
results <- calc_annual_outside_normal(station_number = "08LE019")
results <- calc_daily_stats(station_number = "08LE019")
results <- calc_daily_cumulative_stats(station_number = "08LE019")
results <- calc_flow_percentile(station_number = "08LE019", flow_value = 10000)
results <- calc_longterm_mad(station_number = "08LE019")
results <- calc_longterm_percentile(station_number = "08LE019", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08LE019")
results <- calc_monthly_stats(station_number = "08LE019")
results <- screen_flow_data(station_number = "08LE019")

plot_flow_data(station_number = "08LE019")
plot_annual_cumulative_stats(station_number = "08LE019", water_year_start = 2)
plot_annual_flow_timing(station_number = "08LE019", water_year_start = 1)
plot_annual_outside_normal(station_number = "08LE019")
plot_annual_stats(station_number = "08LE019", percentiles = 1:20)
plot_annual_lowflows(station_number = "08LE019")
plot_daily_cumulative_stats(station_number = "08LE019",water_year_start = 9)
plot_daily_stats(station_number = "08LE019")
plot_flow_duration(station_number = "08LE019", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = F, log_discharge = T)
plot_flow_data(station_number = "08LE019")
plot_longterm_stats(station_number = "08LE019", ignore_missing = T, water_year_start = 5)
plot_data_screening(station_number = "08LE019")
plot_missing_dates(station_number = "08LE019",,water_year_start = 9)
plot_monthly_cumulative_stats(station_number = "08LE019", use_yield = T, log_discharge = F,water_year_start = 9)
plot_monthly_stats(station_number = "08LE019", log_discharge = F)
plot_annual_cumulative_stats(station_number = "08LE019", use_yield = T)
plot_annual_means(station_number = "08LE019")
trending <- compute_annual_trends(station_number = "08LE019", zyp_method = "yuepilon", ignore_missing = T)


write_flow_data(station_number = c("08HB048","08NM116"))



data <- calc_annual_stats(station_number = "08HB048", start_year = 2014)
data <- calc_annual_stats(station_number = "08HB048")

ggplot(data = data, aes(x= Year, y=Mean))+
  geom_point()+
  geom_line()+
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  {if(length(unique(Year)) < 4) ggplot2::scale_x_continuous(breaks = unique(Year), na.value =TRUE)}
  


ggplot(data = data, aes(x= as.Date(paste(Year, "01", "01", sep = "-")), y=Mean))+
  geom_point()+
  geom_line()+
  ggplot2::scale_x_date(date_labels = "%Y", breaks = scales::pretty_breaks())
  
# Multiple stations
flow_data <- fill_missing_dates(station_number = c("08HB048","08NM116"))
flow_data <- add_basin_area(station_number = c("08HB048","08NM116"))
flow_data <- add_seasons(station_number = c("08HB048","08NM116"))
flow_data <- add_date_variables(station_number = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_yield(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(station_number = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))

results <- calc_longterm_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_stats(station_number = c("08HB048","08NM116"))
results <- calc_all_annual_stats(station_number = c("08HB048","08NM116"), transpose = T)
results <- calc_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_flow_timing(station_number = c("08HB048","08NM116"))
results <- calc_annual_lowflows(station_number = c("08HB048","08NM116"))
results <- calc_annual_outside_normal(station_number = c("08HB048","08NM116"))
results <- calc_daily_stats(station_number = c("08HB048","08NM116"))
results <- calc_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_flow_percentile(station_number = c("08HB048","08NM116"), flow_value = 10000)
results <- calc_lt_mad(station_number = c("08HB048","08NM116"))
results <- calc_lt_percentile(station_number = c("08HB048","08NM116"), percentiles = 50)
results <- calc_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_monthly_stats(station_number = c("08HB048","08NM116"))
results <- screen_flow_data(station_number = c("08HB048","08NM116"))

plot_flow_data(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_annual_flow_timing(station_number = c("08HB048","08NM116"), percent_total = 1:20)
plot_annual_outside_normal(station_number = c("08HB048","08NM116"))
plot_annual_stats(station_number = c("08HB048","08NM116"), include_title = T)
plot_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
test <- plot_daily_stats(station_number = c("08HB048","08NM116"), complete_years = T, include_title = T)
plot_data_screening(station_number = c("08HB048","08NM116"))
plot_flow_duration(station_number = c("08HB048","08NM116"))
plot_longterm_stats(station_number = c("08HB048","08NM116"))
plot_missing_dates(station_number = c("08HB048","08NM116"))
plot_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_monthly_stats(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))

trending <- compute_annual_trends(station_number = c("08HB048","08NM116"), zyp_method = "yuepilon")




devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr devel", repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")
#devtools::check()

library(fasstr)





#### TRENDING


trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon", start_year = 1973)


alldata <- fasstr::calc_annual_flow_timing("08HB048", transpose = T)
alldata <- alldata[,2:ncol(alldata)]



### FREQUENCY

data <- compute_annual_frequencies(station_number = "08HB048", water_year = F, start_year = 1980, end_year = 2010, exclude_years = 1999)
data <- compute_hydat_peak_frequencies(station_number = "08NM116", use_max = TRUE)

data <- compute_annual_frequencies(station_number = c("08HB048","08NM116"))
data <- compute_frequency_stat(station_number = "08NM116", roll_day = 7, return_period = 10)




data = tidyhydat::hy_daily_flows(station_number = "08HB048")
dates = "Date"
values = "Value"
station_number = NULL
roll_days = c(1, 3, 7, 30)
roll_align = "right"
use_max = TRUE
use_log = FALSE
prob_plot_position = c("weibull", "median", "hazen")
prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)
fit_distr = c("PIII", "weibull")
fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE")
fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)
water_year = FALSE
water_year_start = 10
start_year = 0
end_year = 9999
exclude_years = NULL
months = 1:12
ignore_missing = FALSE


# PLOTS TESTING


year_data <- fill_missing_dates(station_number = c("08HB048","08NM116"), water_year = water_year, water_year_start = water_year_start)

daily_stats <- results
water_year=F
water_year_start=9
use_yield=F
origin_date <- as.Date("1899-12-31")




flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  calc_annual_stats()

annual_stats <- calc_annual_stats(station_number = c("08HB048","08NM116","08NM242","08NM241","08HB069"), ignore_missing = T)
annual_stats <- calc_annual_stats(station_number = "08HB048")
annual_stats <- tidyr::gather(annual_stats, Statistic, Value, -Year, -STATION_NUMBER)

plots <- annual_stats %>%
  dplyr::group_by(STATION_NUMBER) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    plot = purrr::map2(
      data, STATION_NUMBER, 
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, color = Statistic)) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::geom_line(alpha = 0.5) +
        ggplot2::geom_point() +
        {if(!log_discharge) ggplot2::expand_limits(y = c(0, max(.$Value, na.rm = T) * 1.05))}+
        {if(log_discharge) ggplot2::expand_limits(y = c(min(.$Value, na.rm = T) * .95, max(.$Value, na.rm = T) * 1.05))} +
        {if(log_discharge) ggplot2::scale_y_log10(expand = c(0,0))} +
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = c(0,0))} +
        {if(log_discharge) ggplot2::annotation_logticks(base = 10, "l", colour = "grey25", size = 0.3, short = ggplot2::unit(.07, "cm"), 
                                                        mid = ggplot2::unit(.15, "cm"), long = ggplot2::unit(.2, "cm"))} +
        ggplot2::expand_limits(y = 0) +
        ggplot2::ylab("Discharge (cms)")+
        ggplot2::xlab("Year") +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme_bw() +
        ggplot2::labs(color='Annual Statistics') +    
        ggplot2::theme(legend.position = "right", 
                       legend.spacing = ggplot2::unit(0, "cm"),
                       legend.justification = "top",
                       legend.text = ggplot2::element_text(size = 9),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10))))

plotss <- plots$plot
if (nrow(plots) == 1) {
  names(plotss) <- "Annual_Stats"
} else {
  names(plotss) <- paste0(plots$STATION_NUMBER, "_Annual_Stats")
}

plotss


names(plot)



if (inherits(plot$`08HB048_Annual_Stats`,"gg")) {
  gg
}

length(plot2)


# Save list of gg plots

write_plots(foldername = "GUUUURL/", 
               plots = plot, 
               type = "pdf",
               height = 4,
               width = 11,
               dpi = 100,
               combined_pdf =)

# save_plot_list <- function(plots = NULL,
#                            foldername = "",
#                            type = NULL, 
#                            width = NA,
#                            height = NA,
#                            units = "in",
#                            dpi = 300,
#                            combined_pdf = FALSE){
#   
#   # Check list of plots
#   if (is.null(plots)) stop("Must provice a list of plots.", call. = FALSE)
#   if (!is.list(plots)) stop("Object provided is a not a list.", call. = FALSE)
#   if (!all(sapply(plots, inherits, what = "gg"))) stop("Not all objects in list are plots.", call. = FALSE)
#   
#   # Check device type
#   if (!combined_pdf) {
#     if (is.null(type)) stop("Must provide an image type to save.", call. = FALSE)
#     if (!type %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg")) 
#       stop("Use of the file types required.", call. = FALSE)
#   }
#   
#   # Check dimensions
#   if ((!is.na(height) & !is.numeric(height)))
#     stop("height argument must be numeric.", call. = FALSE)
#   if (length(height) !=1) stop("Only one height value can be provided.", call. = FALSE)
#   
#   if ((!is.na(width) & !is.numeric(width)))
#     stop("width argument must be numeric.", call. = FALSE)
#   if (length(width) !=1) stop("Only one width value can be provided.", call. = FALSE)
#   
#   if (length(units) != 1)  stop("only one unit type can be provided.", call. = FALSE)
#   if (!units %in% c("in", "cm", "mm"))  stop("Only units of 'in', 'cm', or 'mm' can be provided.", call. = FALSE)
#   
#   # Create a single PDF document
#   if(combined_pdf) { 
#     
#     # Remove slash if foldername ends with it
#     if (substr(foldername, nchar(foldername), nchar(foldername)) == "/") {
#       foldername <- substr(foldername, 1, nchar(foldername)-1)
#     }
#     
#     # Check dimensions for PDF device
#     if(is.na(width)) {
#       width <- grDevices::dev.size(units = units)[1]
#     }
#     
#     if(is.na(height)) {
#       height <- grDevices::dev.size(units = units)[2]
#     }
#     
#     # Plot plots to PDF device
#     grDevices::pdf(file = paste0(foldername, ".pdf"), width = width, height = height)
#     for (i in names(plots)) {
#       suppressWarnings(plot(plots[[i]]))
#     }
#     invisible(grDevices::dev.off())
#     
#   } else {
#     
#   # Create a folder of plots
#     
#     # Check if folder exists, create if not
#     dir.create(foldername, showWarnings = FALSE)
#     
#     # Add the slash to foldername if it doesn't exist
#     if (!substr(foldername, nchar(foldername), nchar(foldername)) == "/") {
#       foldername <- paste0(foldername, "/")
#     }
#     
#     # Filter through each plot
#     for (i in names(plots)) {
#       suppressWarnings(ggplot2::ggsave(filename = paste0(foldername, i, ".", type), 
#                                        plot = plots[[i]],
#                                        width = width,
#                                        height = height,
#                                        units = units,
#                                        dpi = dpi))
#     }
#   }
#   
#   
# }



# 
# 
# 
# 
# type ="pdf"
# lapply(names(plot), 
#        function(x, type = "pdf") ggplot2::ggsave(filename = paste(x, ".", type, sep = ""), plot = plot[[x]]))
# 
# 
# lapply(names(plot), 
#        function(x, ) ggplot2::ggsave(filename = paste(x, ".jpeg", sep=""), plot = plot[[x]]))
# 
# 
# if (inherits(plot, what = "gg") {
#   # PLOT THIS SINGLE PLOT
#   
# } else {
#   # If is a list and none of them are gg
#   if (is.list(plot) & !all(sapply(plot, inherits, what = "gg"))) {
#     stop("none are gg plots")
#   }
#   
# }
# 
# # Round any numeric column to the specified digits
# if (!all(sapply(plot, inherits, what = "gg"))) {
#   stop("Not all objects in list are ggplot2 plots.", call. = FALSE)
#   } else {
#   #PLOT SOME SHIT
# }
# 
# 
# 
# 
# for (i in plot) {
#   
#   ggplot2::ggsave(filename = "waaaht", plot = i, device = "pdf")
# }

######## WRITING


fasstr::plot_annual_flow_timing("08HB048")

write_plots(month_plot, file = "pdftestkkkk", format = "jpeg")

month_plot <- plot_monthly_stats("08HB048")$Monthly_Mean
ggplot2::ggsave(month_plot, filename = "test.pdf")
lowflows <- calc_annual_lowflows("08HB048", transpose = T)

data <- compute_annual_trends("08HB048", zyp_method = "yuepilon", incl_data=F)

data <- calc_all_annual_stats("08HB048", transpose = T)
write_results(data = data, file = "all.xlsx", digits = 3)
calc_annual_flow_timing("08HB048")

fasstr::calc_monthly_cumulative_stats("08HB048", use_yield = T)
write_flow_data(data = "08HB048", file = "d.xls", digits = 1)

write_results(data = calc_longterm_stats(data = c("08HA002", "08HA011"),
                                         start_year = 1971, end_year = 2000), 
              file = "Cowichan River Long-term Flows (1971-2000).xlsx", 
              digits = 1)

ggplot2::ggsave("month",month_plots, device = "pdf")

data <- fasstr::calc_annual_cumulative_stats(c("08NM116","08HB048"), water_year = T, water_year_start = 3, include_seasons = T, use_yield = T)

writexl::write_xlsx(data, "c.xls")


test <- data %>% filter(Year==1973, Month %in% c(1:3)) %>% 
  summarise(sum=sum(daily_total))



fasstr::write_flow_data(flow_data, file = "test2.xlsx", value_digits = 1)
data <- fasstr::calc_annual_stats("08HB048", start_year = 1973)

writexl::write_xlsx(data, path = "test.xlsx")

fasstr::plot_flow_data(c("08HA002","08HA011"), plot_by_year = T, start_year = 1971, end_year = 1975)


fasstr::plot_daily_stats("08HB048", start_year = 1980, end_year = 2000)

data + ggplot2::ylab("Depth Below Surface (m)")




fasstr::plot_daily_stats("08NM116", start_year = 1980, include_year = 1990, water_year = T, water_year_start = 5)








####PLOTS

library(ggthemes)


data <- fasstr::compute_frequency_stat(flow_data, use_hydat_peaks = T, use_max = T,return_period = 10)
data <- fasstr::compute_frequency_analysis("08HB048")
theme_gdocs()
theme_calc()

### MAKE LINES ON ANNUAL LIGHTER/ALPHA.5
#### theme_calc with themegdoc axis font size

data <- fasstr::plot_daily_stats(HYDAT = "08HB048", log_discharge = T)$daily_statisitics


data + theme_calc()  #+ scale_colour_calc() #+ ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "grey80"))
data + theme_calc()  + ggplot2::theme(panel.grid = ggplot2::element_blank())
data + theme_calc() + scale_colour_calc() + ggplot2::theme(panel.grid = ggplot2::element_blank())
data + theme_gdocs() + scale_color_gdocs()+ ggplot2::theme(panel.grid = ggplot2::element_blank())+ ggplot2::ylab("YESSSS")







#### BASIN AREA



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


