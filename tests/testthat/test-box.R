library(testthat)

context("test-cat")

df <- pmplots_data_obs()

test_that("vector inputs for cont_cat", {
  y <- c("ALB", "BMI", "SCR")
  x <- c("RF", "CPc", "STUDYc")
  ans <- cont_cat(df,x,y[1])
  expect_length(ans,length(x))
  ans <- cont_cat(df,x[1],y)
  expect_length(ans,length(y))
  ans <- cont_cat(df,x,y)
  expect_length(ans,length(x)*length(y))
})

test_that("cont_cat jitters points in x-direction only", {
  id <- pmplots::pmplots_data_id()
  p <- cont_cat(id, x = "STUDYc", y = "WT", points = TRUE)
  d <- suppressMessages(layer_data(p, 1L))
  expect_equal(d$y, id$WT, tolerance = 1e-5)
})
