library(testthat)

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

test_that("list_plot_x, single y [PMP-TEST-016]", {
  p <- list_plot_x(df, x, "WT")
  expect_equal(length(p), length(x))
  expect_named(p)
  expect_equal(names(p), paste0("WTv", x))
})

test_that("list_plot_x, multiple y", {
  p <- list_plot_x(df, x, y)
  expect_equal(length(p), length(x))
  expect_named(p)
  expect_equal(names(p), x)
  expect_equal(names(p$WT), y)
})

test_that("list_plot_y, single x [PMP-TEST-017]", {
  p <- list_plot_y(df, "WT", y)
  expect_equal(length(p), length(y))
  expect_named(p)
  expect_equal(names(p), paste0(y, "vWT"))
})

test_that("list_plot_y with multiple x", {
  p <- list_plot_y(df, x, y)
  expect_equal(length(p), length(y))
  expect_named(p)
  expect_equal(names(p), y)
  expect_equal(names(p$AAG), x)
})

test_that("list_plot_xy [PMP-TEST-018]", {
  p <- list_plot_xy(df, x, y)
  expect_equal(length(p), length(y)*length(x))
  expect_named(p)
  expect_equal(names(p)[1:3], c("SCRvWT", "AAGvWT", "SCRvCRCL"))
  expect_equal(names(p)[4:6], c("AAGvCRCL", "SCRvALB", "AAGvALB"))
})

test_that("list_plot_xy, single x and y", {
  p <- list_plot_xy(df, x[1], y[1])
  expect_identical(names(p), "SCRvWT")
})

test_that("list_plot_yx", {
  p <- list_plot_yx(df, x, y)
  expect_equal(length(p), length(y)*length(x))
  expect_named(p)
  expect_equal(names(p)[1:3], c("SCRvWT", "SCRvCRCL", "SCRvALB"))
  expect_equal(names(p)[4:6], c("AAGvWT", "AAGvCRCL", "AAGvALB"))
})

test_that("list_plot_yx, single x and y", {
  p <- list_plot_yx(df, x[1], y[1])
  expect_identical(names(p), "SCRvWT")
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
