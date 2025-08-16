library(testthat)

df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

test_that("pairs plots y [PMP-TEST-027]", {
  p <- pairs_plot(df, y = c("ALB", "WT", "SCR"))
  expect_is(p, "ggmatrix")
})

test_that("eta pairs plots etas [PMP-TEST-028]", {
  p <- eta_pairs(df, etas = etas)
  expect_is(p, "ggmatrix")
})

test_that("upper and lower funs are used in pairs plots [PMP-TEST-029]", {
  lower <- function(data, mapping, ...) {
    stop("passing lower plot")
  }
  p <- eta_pairs(df, etas = etas, lower_fun = lower)
  expect_error(print(p), "passing lower plot")
  upper <- function(data, mapping, ...) {
    stop("passing upper plot")
  }
  p <- eta_pairs(df, etas = etas, upper_fun = upper)
  expect_error(print(p), "passing upper plot")
})

test_that("use lower_plot to draw scatter plot in lower panels [PMP-TEST-030]", {
  lower <- function(p) {
    stop("draw using lower_plot")
  }
  p <- eta_pairs(df, etas = etas, lower_plot = lower)
  expect_error(print(p), "draw using lower_plot")
  lower2 <- function(p,q) {}
  expect_error(
    eta_pairs(df, etas = etas, lower_plot = lower2),
    "the `lower_plot` function should have exactly one argument"
  )
})

test_that("select different diagonal renderings [PMP-TEST-031]", {
  p <- pairs_plot(df, y = etas, diag = "barDiag")
  expect_is(p, "ggmatrix")
  p <- pairs_plot(df, y = etas, diag = "densityDiag")
  expect_is(p, "ggmatrix")
})

test_that("options to control correlation panels [PMP-TEST-032]", {
  p0 <- eta_pairs(df, etas = etas)
  pm_opts$pairs.cor.col <- "black"
  p <- eta_pairs(df, etas = etas)
  expect_false(identical(p0,p))
  pm_opts$reset()
  pm_opts$pairs.cor.size <- 10
  p <- eta_pairs(df, etas = etas)
  expect_false(identical(p0,p))
  pm_opts$reset()
  pm_opts$pairs.cor.shown <- TRUE
  p <- eta_pairs(df, etas = etas)
  expect_false(identical(p0,p))
  pm_opts$reset()
  pm_opts$pairs.cor.digits <- 3
  p <- eta_pairs(df, etas = etas)
  expect_false(identical(p0,p))
  pm_opts$reset()
  pm_opts$pairs.cor.fontface <- "plain"
  p <- eta_pairs(df, etas = etas)
  expect_false(identical(p0,p))
  pm_opts$reset()
})
