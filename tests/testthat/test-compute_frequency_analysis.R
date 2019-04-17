context("Compute frequency analysis")

test_that("creates a list with the proper objects", {
  skip_on_cran()
  skip_on_travis()
  
  # Calculate some values to use for a frequency analysis 
  # (requires years, values for those years, and the name of the measure/metric)
  low_flows <- calc_annual_lowflows(station_number = "08NM116", 
                                    start_year = 1980, 
                                    end_year = 2000,
                                    roll_days = 7)
  low_flows <- dplyr::select(low_flows, Year, Value = Min_7_Day)
  low_flows <- dplyr::mutate(low_flows, Measure = "7-Day")
  
  # Compute the frequency analysis using the default parameters
  data <- suppressMessages(suppressWarnings(
    compute_frequency_analysis(data = low_flows,
                                     events = Year,
                                     values = Value,
                                     measure = Measure)))
  
  expect_true("list" %in% class(data) &
                all(c("Freq_Analysis_Data","Freq_Plot_Data","Freq_Plot","Freq_Fitting","Freq_Fitted_Quantiles") %in% names(data)))
})

test_that("creates proper object classes", {
  skip_on_cran()
  skip_on_travis()
  
  # Calculate some values to use for a frequency analysis 
  # (requires years, values for those years, and the name of the measure/metric)
  low_flows <- calc_annual_lowflows(station_number = "08NM116", 
                                    start_year = 1980, 
                                    end_year = 2000,
                                    roll_days = 7)
  low_flows <- dplyr::select(low_flows, Year, Value = Min_7_Day)
  low_flows <- dplyr::mutate(low_flows, Measure = "7-Day")
  
  # Compute the frequency analysis using the default parameters
  data <- suppressMessages(suppressWarnings(
    compute_frequency_analysis(data = low_flows,
                                     events = Year,
                                     values = Value,
                                     measure = Measure)))
  
  expect_true("data.frame" %in% class(data$Freq_Analysis_Data))
  expect_true("data.frame" %in% class(data$Freq_Plot_Data))
  expect_true("gg" %in% class(data$Freq_Plot))
  expect_true("list" %in% class(data$Freq_Fitting))
  expect_true("data.frame" %in% class(data$Freq_Fitted_Quantiles))
})
