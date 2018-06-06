library(ggplot2)
library(fasstr)

hght <- 1.8
wdth <- 3.4

mgins <- c(0.5,0.5,0.5,0.5)

trending <- compute_annual_trends(station_number = "08NM116", zyp_method = "yuepilon", start_year = 1974)

trending_plots <- plot_annual_trends(trending, zyp_alpha = 0.05)

trends <- trending_plots$Sep_Mean
trends + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_trends.png",
         height = hght,
         width = wdth,
         units = "cm")


daily <- plot_daily_stats(station_number = "08NM116", start_year = 1974)[[1]]
daily + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_daily.png",
         height = hght,
         width = wdth,
         units = "cm")

daily_cumul <- plot_daily_cumulative_stats(station_number = "08NM116", start_year = 1974)[[1]]
daily_cumul + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_daily_cumul.png",
         height = hght,
         width = wdth,
         units = "cm")

month_cumul <- plot_monthly_cumulative_stats(station_number = "08NM116", start_year = 1974)[[1]]
month_cumul + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_month_cumul.png",
         height = hght,
         width = wdth,
         units = "cm")

duration <- plot_flow_duration(station_number = "08NM116", start_year = 1974)[[1]]
duration + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_duration.png",
         height = hght,
         width = wdth,
         units = "cm")

data <- plot_flow_data(station_number = "08NM116", start_year = 1974)[[1]]
data + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_flowdata.png",
         height = hght,
         width = wdth,
         units = "cm")


annual <- plot_annual_stats(station_number = "08NM116", start_year = 1974, log_discharge = F)[[1]]
annual + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_annual.png",
         height = hght,
         width = wdth,
         units = "cm")

freq <- compute_annual_frequencies(station_number = "08NM116", start_year = 1974, roll_days = c(1,30))[[3]]
freq + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_freq.png",
         height = hght,
         width = wdth,
         units = "cm")

lt <- plot_longterm_stats(station_number = "08NM116", start_year = 1974)[[1]]
lt + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm")) +
  ggsave("cheatsheet_longterm.png",
         height = hght,
         width = wdth,
         units = "cm")

month <- plot_monthly_stats(station_number = "08NM116", start_year = 1974, months = 4)[[1]]
month + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.x = element_text(margin = margin(.01,0,0.04,0, "cm"), size = 7))+
  ggsave("cheatsheet_monthly.png",
         height = hght,
         width = wdth,
         units = "cm")



lowflows <- plot_annual_lowflows(station_number = "08NM116", start_year = 1974, roll_days = c(3,30))[[1]]
lowflows + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.y = element_text(margin = margin(0.03,0.01,0.01,0.05, "cm"), vjust=1, size = 7),
        panel.spacing = unit(.05, "lines"))+
  ggsave("cheatsheet_lowflows.png",
         height = hght,
         width = wdth,
         units = "cm")

timing <- plot_annual_flow_timing(station_number = "08NM116", start_year = 1974, percent_total = c(25,50))[[1]]
timing + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.y = element_text(margin = margin(0.03,0.01,0.01,0.05, "cm"), vjust=1, size = 7),
        panel.spacing = unit(.05, "lines"))+
  ggsave("cheatsheet_timing.png",
         height = hght,
         width = wdth,
         units = "cm")

normal <- plot_annual_outside_normal(station_number = "08NM116", start_year = 1974)[[1]]
normal + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.y = element_text(margin = margin(0.03,0.01,0.01,0.05, "cm"), vjust=1, size = 7),
        panel.spacing = unit(.001, "lines"))+
  ggsave("cheatsheet_normal.png",
         height = hght,
         width = wdth,
         units = "cm")

annual_cumul <- plot_annual_cumulative_stats(station_number = "08NM116", start_year = 1974, incl_seasons = T)[[1]]
annual_cumul + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.y = element_text(margin = margin(0.03,0.01,0.01,0.05, "cm"), vjust=1, size = 7),
        panel.spacing = unit(.05, "lines"))+
  ggsave("cheatsheet_annual_cumul.png",
         height = hght,
         width = wdth,
         units = "cm")

screening <- plot_data_screening(station_number = "08NM116", start_year = 1974)[[1]]
screening + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.x = element_text(margin = margin(.01,0,0.04,0, "cm"), size = 7),
        panel.spacing = unit(.05, "lines"))+
  ggsave("cheatsheet_screening.png",
         height = hght,
         width = wdth,
         units = "cm")

missing <- plot_missing_dates(station_number = "08NM116", mon)[[1]]
missing + 
  ylab(NULL) +
  xlab(NULL) +
  ggtitle(NULL) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none",
        panel.border = element_rect(size=.5),
        plot.margin=grid::unit(mgins, "mm"),
        strip.text.x = element_text(margin = margin(.01,0,0.04,0, "cm"), size = 7),
        panel.spacing = unit(.05, "lines"))+
  ggsave("cheatsheet_missing.png",
         height = hght,
         width = wdth,
         units = "cm")



