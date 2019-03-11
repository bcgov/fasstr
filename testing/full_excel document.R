

# Excel Data

library(openxlsx)

output_excel <- createWorkbook()

sheet_names <- c("Analysis Information",  ### include copy of code so it can be re-run if necessary
                 "Data Timeseries",
                 "Data Screening",
                 "Long-term Statistics",
                 "Annual Statistics",
                 "Annual Cumulative Statistics",
                 "Annual Statistics Other",
                 "Monthly Statistics",
                 "Monthly Cumulative Statistics",
                 "Daily Statistics",
                 "Daily Cumulative Statistics",
                 "Annual Trends",
                 "Low-flow Frequency Analysis")


for (i in sheet_names) {
  addWorksheet(output_excel, i)
}

###


saveWorkbook(output_excel,"fasstr_analysis.xlsx", overwrite = T)





writeData(output_excel,"Streamflow Data", x = "Salmon River (08LE020) - 1988-2017", startCol = 2, startRow = 2)
writeData(output_excel,"Streamflow Data", x = stats_08LE020, startCol = 2, startRow = 3)

writeData(output_excel,"Streamflow Data", x = "Salmon River (08LE019) - 1988-2017 (extrapolated from 08LE020 and 08LE075 1970-1978)", startCol = 2, startRow = 8)
writeData(output_excel,"Streamflow Data", x = stats_08LE019, startCol = 2, startRow = 9)

writeData(output_excel,"Streamflow Data", x = "Pringle Creek (08LE006) 1988-2017 (extrapolated from 08LE020 1945-1953)", startCol = 2, startRow = 14)
writeData(output_excel,"Streamflow Data", x = stats_08LE006, startCol = 2, startRow = 15)

writeData(output_excel,"Streamflow Data", x = "Ingram Creek (08LE008) 1988-2017 (extrapolated from 08LE020 1911-1921)", startCol = 2, startRow = 20)
writeData(output_excel,"Streamflow Data", x = stats_08LE008, startCol = 2, startRow = 21)

writeData(output_excel,"Streamflow Data", x = "Bolean Creek (08LE094) 1988-2017 (extrapolated from 08LE020 1974-1984)", startCol = 2, startRow = 26)
writeData(output_excel,"Streamflow Data", x = stats_08LE094, startCol = 2, startRow = 27)

writeData(output_excel,"Streamflow Data", x = "Ungauged Watersheds 1988-2017 (Area-Ratio method from other stations)", startCol = 2, startRow = 32)
writeData(output_excel,"Streamflow Data", x = stats_ungauged, startCol = 2, startRow = 33)

writeData(output_excel,"Streamflow Data", x = "Balance of SWin and SWout", startCol = 2, startRow = 38)
writeData(output_excel,"Streamflow Data", x = stats_combined, startCol = 2, startRow = 39)

# Plots
writeData(output_excel,"Streamflow Plots", x = "Daily flows at Salmon River at Falkland", startCol = 2, startRow = 2)
print(salmon_plot[[1]])
insertPlot(output_excel,"Streamflow Plots", startCol = 2, startRow = 3,height = 4.5, width = 10)

writeData(output_excel,"Streamflow Plots", x = "Monthly flows at all stations", startCol = 2, startRow = 26)
print(monthly_plot)
insertPlot(output_excel,"Streamflow Plots", startCol = 2, startRow = 27, height = 6, width = 10)

writeData(output_excel,"Streamflow Plots", x = "Monthly SWin vs SWout", startCol = 2, startRow = 57)
print(combined_plot)
insertPlot(output_excel,"Streamflow Plots", startCol = 2, startRow = 58, height = 7, width = 6)


source("flow analysis correlations.R")
source("salmon baseflow.R")

