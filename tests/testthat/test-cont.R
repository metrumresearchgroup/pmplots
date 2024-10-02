library(testthat)

df <- pmplots_data_obs()

test_that("scatter plot IDs [PMP-TEST-007]", {
  gg <- dv_pred(df, plot_id = TRUE, group = "ID", size = 4)
  expect_is(gg, "gg")
})

test_that("cont_cont works when receiving integer data for x", {
  df$AGE <- as.integer(df$AGE)
  ans <- cont_cont(df, x = "AGE", y = "WT")
  expect_is(ans, "gg")

  tib <- dplyr::as_tibble(df)
  ans <- cont_cont(tib, x = "AGE", y = "WT")
  expect_is(ans, "gg")
})
