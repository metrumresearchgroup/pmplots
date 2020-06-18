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

