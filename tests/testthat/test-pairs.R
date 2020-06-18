library(testthat)

context("test-pairs")

df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

test_that("pairs plots y", {
  p <- pairs_plot(df, y = c("ALB","WT", "SCR"))
  expect_is(p, "gg")
})

test_that("eta pairs plots etas", {
  p <- eta_pairs(df, etas = etas)
  expect_is(p, "gg")
})

