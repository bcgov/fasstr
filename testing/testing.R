station_name=NULL
#flow.data=NULL,
HYDAT="08HA002"
water_year=FALSE #create another for own water year????
start_year=1965
end_year=2015
exclude_years=NULL # list of stations
basin_area=594 # if na, then all Yield values == NA
zyp_trending="yuepilon"
zyp_alpha=0.05
write_table=FALSE        # write out statistics on calendar year
write_transposed.table=FALSE  # write out statistics in transposed format (cy & wy)
write_summary_table=FALSE # write out a summary of period of record
write_lowflow_table=FALSE      # write out a summary of low flows
write_zyp_table=TRUE
write_zyp_plots="png"
report_dir="Upper"
na.rm=list(na.rm.global=FALSE)
table_nddigits=3   


data <- fasstr::fasstr_add_rolling_means(HYDAT = "08HB048")
data <- fasstr_add_total_volume(HYDAT = "08HB048")

