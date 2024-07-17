library(testthat)

df <- pmplots_data_obs()

test_that("scatter plot IDs [PMP-TEST-007]", {
  gg <- dv_pred(df, plot_id = TRUE, group = "ID", size = 4)
  expect_is(gg, "gg")
})


