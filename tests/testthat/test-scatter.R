library(testthat)

df <- pmplots_data_obs()

test_that("vector inputs for cont_cont [PMP-TEST-055]", {
  x <- c("ALB", "BMI", "SCR")
  y <- c("CRCL", "AAG")
  ans <- cont_cont(df,x,y[1])
  expect_length(ans,length(x))
  ans <- cont_cont(df,x[1],y)
  expect_length(ans,length(y))
  ans <- cont_cont(df,x,y)
  expect_length(ans,length(x)*length(y))
})

test_that("set alpha in scatter plot [PMP-TEST-056]", {
  p1 <- dv_pred(df[1:20,], alpha = 0.25)
  layerd <- suppressMessages(layer_data(p1))
  expect_true(all(layerd$alpha==0.25))

  pm_opts$set(scatter.alpha = 0.75)
  p1 <- dv_time(df[1:20,])
  pm_opts$reset()
  layerd <- suppressMessages(layer_data(p1))
  expect_true(all(layerd$alpha==0.75))
})
