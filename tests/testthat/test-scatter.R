library(testthat)

context("test-scatter")

df <- pmplots_data_obs()

test_that("vector inputs for cont_cont", {
  x <- c("ALB", "BMI", "SCR")
  y <- c("CRCL", "AAG")
  ans <- cont_cont(df,x,y[1])
  expect_length(ans,length(x))
  ans <- cont_cont(df,x[1],y)
  expect_length(ans,length(y))
  ans <- cont_cont(df,x,y)
  expect_length(ans,length(x)*length(y))
})



