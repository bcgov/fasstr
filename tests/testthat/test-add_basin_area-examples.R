context("Add basin area")

test_that("`add_basin_area()` works", {
  skip_on_travis()
  
  expect_s3_class("gg", class(plot_object))
  
  df <- fastrr_function
  
  expect_true(colnames(df), c("whatecver teh cols should"))
})
