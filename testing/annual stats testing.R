
devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")

fasstr::fasstr_annual_stats(HYDAT = "08HB048",write_table = T,exclude_years = c(1972:1975),start_year = 1974)
low.flows <- fasstr::fasstr_annual_lowflows(HYDAT = "08HB048",water_year = T)
low.flows <- dplyr::select(low.flows,-dplyr::contains("Date"))


months <- fasstr::fasstr_monthly_stats(HYDAT = "08HB048",percentiles = c(10,25,75,90),water_year = T,water_year_start = 5)
months2 <- with(months, months[order(Year, Month),])

flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=T
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
percentiles=c(25,75)
transpose=FALSE
write_table=FALSE
report_dir="."
na.rm=list(na.rm.global=FALSE)
table_nddigits=3



fasstr::fasstr_add_date_vars(HYDAT = "08HB048")
