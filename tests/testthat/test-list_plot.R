library(testthat)

context("test-layer")

df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- dv_pred(df)
p <- ggplot(df, aes(TIME,DV)) + geom_point()

test_that("pm_grid [PMP-TEST-015]", {
  expect_is(pm_grid(list(p,p)), "gg")
})

test_that("pm_grid tag levels", {
  a <- pm_grid(list(p,p), tag_levels = "i")
  expect_equal(a$patches$annotation$tag_levels, "i")
})

x <- c("WT", "CRCL", "ALB")
y <- c("SCR", "AAG")

test_that("list_plot_x [PMP-TEST-016]", {
  p <- list_plot_x(df, x, "WT")
  expect_equal(length(p), length(x))
})

test_that("list_plot_y [PMP-TEST-017]", {
  p <- list_plot_y(df, "WT", y)
  expect_equal(length(p), length(y))
})

test_that("list_plot_xy [PMP-TEST-018]", {
  p <- list_plot_xy(df, x, y)
  expect_equal(length(p), length(y)*length(x))
})

test_that("list_plot accepts vector or list", {
  x <- as.list(x)
  y <- as.list(y)

  expect_type(list_plot_x(df, "WT", y), "list")
  expect_type(list_plot_y(df, x, y[1]), "list")
  expect_type(list_plot_xy(df, x, y), "list")

  x <- as.list("STUDYc", "CPc")
  expect_type(list_plot_x(df, x, y[1], cont_cat), "list")
  expect_type(list_plot_y(df, x[1], y, cont_cat), "list")
  expect_type(list_plot_xy(df, x, y, cont_cat), "list")
})
