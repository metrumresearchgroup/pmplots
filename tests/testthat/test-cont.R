library(testthat)

context("test-cont")

df <- pmplots_data_obs()

test_that("", {
  gg <- dv_pred(df, plot_id = TRUE, group = "ID", size = 4)
  expect_is(gg, "gg")
})


