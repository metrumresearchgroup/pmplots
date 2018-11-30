library(testthat)

context("test-layer")

df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- dv_pred(df)
p <- ggplot(df, aes(TIME,DV)) + geom_point()

test_that("pm_grid", {
  expect_is(pm_grid(list(p,p)), "gg")
})

x <- c("WT", "CRCL", "ALB")
y <- c("SCR", "AAG")

test_that("list_plot_x", {
  p <- list_plot_x(df, x, "WT")
  expect_equal(length(p), length(x))
})

test_that("list_plot_y", {
  p <- list_plot_y(df, "WT", y)
  expect_equal(length(p), length(y))
})

test_that("list_plot_xy", {
  p <- list_plot_xy(df, x, y)
  expect_equal(length(p), length(y)*length(x))
})


