library(testthat)

context("test-cont")

df <- pmplots_data_obs()

test_that("scatter plot IDs [PMP-TEST-007]", {
  gg <- dv_pred(df, plot_id = TRUE, group = "ID", size = 4, add_layers = FALSE)
  expect_is(gg, "gg")
})


