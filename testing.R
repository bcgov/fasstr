station.name="TEST"
#flow.data=NULL,
HYDAT="08HB048"
water.year=FALSE #create another for own water year????
start.year=1975
end.year=2000
exclude.years=c(1979,1981) # list of stations
basin.area=10.1 # if na, then all Yield values == NA
zyp.trending=NA
zyp.alpha=0.05
write.table=FALSE        # write out statistics on calendar year
write.transposed.table=FALSE  # write out statistics in transposed format (cy & wy)
write.summary.table=FALSE # write out a summary of period of record
write.lowflow.table=FALSE      # write out a summary of low flows
plot.cumdepart=FALSE         # plot cumulative departure curves
write.zyp.table=FALSE
write.zyp.plots=FALSE
report.dir="."
na.rm=list(na.rm.global=FALSE)
csv.nddigits=3   