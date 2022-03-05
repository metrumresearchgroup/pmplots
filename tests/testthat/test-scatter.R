library(testthat)

context("test-scatter")

df <- pmplots_data_obs()
id <- pmplots_data_id()

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

test_that("scatt: pass color name", {
  ans <- scatt(id, col = NULL, x = "ALB", y = "WT")
  ans <- suppressMessages(layer_data(ans, i = 1))
  expect_true(all(ans$colour=="black"))
})

test_that("scatt: pass color mapping", {
  ans <- scatt(id, col = "red4", x = "ALB", y = "WT")
  ans <- suppressMessages(layer_data(ans, i = 1))
  expect_true(all(ans$colour=="red4"))
})

test_that("scatt: set via opts", {
  pm_opts$set(scatter.col = "blue4")
  ans <- scatt(id, x = "ALB", y = "WT")
  pm_opts$reset()
  ans <- suppressMessages(layer_data(ans, i = 1))
  expect_true(all(ans$colour=="blue4"))
  pm_opts$reset()
})

test_that("scatt: pass color error", {
  expect_error(
    scatt(id, x = "ALB", y = "WT", col = 1),
    regexp="col should have type character or NULL"
  )
})
