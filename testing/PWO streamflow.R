

## WO Streamflow Indicator
library(fasstr)
library(dplyr)
library(tidyr)



flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM241")
trending_data <- merge(fasstr_add_rolling_means(flow_data, days = c(3,7)) %>% 
                         fasstr_add_missing_dates() %>% 
                         fasstr_add_date_vars() %>%
                         filter(Year>=1986) %>% 
                         group_by(Year) %>% 
                         summarize(Mean_1day=mean(Value),
                                   Max_3day=max(Q3Day),
                                   Min_7day=min(Q7Day)),
                       fasstr_annual_days_outside_normal(flow_data) %>% select(Year,Days_Below_Normal, Days_Above_Normal),
                       by="Year") %>% 
  gather(Metric,Value,-1) %>% spread(Year,Value)
trends <- fasstr_annual_trends_analysis(trendsdata = trending_data, zyp_method = "yuepilon")





testdata <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  fasstr_add_rolling_means() %>% 
  fasstr_add_missing_dates() %>% 
  fasstr_add_date_vars() %>% 
  group_by(STATION_NUMBER,DayofYear) %>% 
  filter(Year>1972) %>% 
  summarize(MEAN=mean(Q7Day))

library(ggplot2)
ggplot(data = testdata, aes(x=DayofYear,y=MEAN, colour=STATION_NUMBER))+
  geom_line()





testdata <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  #fasstr_add_daily_volume() %>% 
  #fasstr_add_daily_yield() %>% 
  #fasstr_add_cumulative_volume() %>% 
  #fasstr_add_cumulative_yield() %>% 
  #fasstr_add_rolling_means() %>% 
  fasstr_add_missing_dates() %>% 
  fasstr_add_date_vars() %>% 
  group_by(STATION_NUMBER,Year) %>% 
  summarize(MEAN=mean(Value))






