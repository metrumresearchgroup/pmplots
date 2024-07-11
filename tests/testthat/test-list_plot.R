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
  expect_named(p)
  expect_equal(names(p), paste0("WTv", x))
})

test_that("list_plot_y [PMP-TEST-017]", {
  p <- list_plot_y(df, "WT", y)
  expect_equal(length(p), length(y))
  expect_named(p)
  expect_equal(names(p), paste0(y, "vWT"))
})

test_that("list_plot_xy [PMP-TEST-018]", {
  p <- list_plot_xy(df, x, y)
  expect_equal(length(p), length(y)*length(x))
  expect_named(p)
  expect_equal(names(p)[1:3], c("SCRvWT", "AAGvWT", "SCRvCRCL"))
  expect_equal(names(p)[4:6], c("AAGvCRCL", "SCRvALB", "AAGvALB"))
})

test_that("list_plot_yx", {
  p <- list_plot_yx(df, x, y)
  expect_equal(length(p), length(y)*length(x))
  expect_named(p)
  expect_equal(names(p)[1:3], c("SCRvWT", "SCRvCRCL", "SCRvALB"))
  expect_equal(names(p)[4:6], c("AAGvWT", "AAGvCRCL", "AAGvALB"))
})
