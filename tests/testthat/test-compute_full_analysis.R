context("Compute full analysis")

test_that("creates a list with the proper named lists", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressMessages(suppressWarnings(
    compute_full_analysis(station_number = "08NM116", start_year = 1980, end_year = 1985)))
  expect_true("list" %in% class(data) &
                "list" %in% sapply(data, class) &
                all(names(data) %in% c("Screening","Longterm","Annual","Monthly","Daily","Trending","Lowflow_Frequencies")))
})

test_that("creates a list with proper analyses", {
  skip_on_cran()
  skip_on_travis()
  data <- suppressMessages(suppressWarnings(
    compute_full_analysis(station_number = "08NM116", start_year = 1980, end_year = 1985,
                                                 analyses = c(1:2,7))))
  expect_true("list" %in% class(data) &
                "list" %in% sapply(data, class) &
                all(names(data) %in% c("Screening","Longterm","Lowflow_Frequencies")))
})
