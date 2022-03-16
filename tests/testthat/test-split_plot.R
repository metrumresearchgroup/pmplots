library(testthat)

context("test-split_plot")

df <- pmplots_data_obs()

test_that("split_plot succeeds when using numeric split", {
  out <- split_plot(df, fun=dv_pred, "STUDY")
  expect_equal(length(out), 4)

})

test_that("split_plot succeeds when using factor", {
  out <- split_plot(df,  fun=dv_pred, "STUDYc")
  expect_equal(length(out), 4)

})

#' Issue #3
test_that("split_plot succeeds when using factor with not all levels there", {
  df <- dplyr::filter(df, STUDY < 3)
  out <- split_plot(df, fun=dv_pred, "STUDYc")
  expect_equal(length(out), 2)
})

test_that("grouped", {
  df <- dplyr::group_by(df,STUDYc)
  p <- split_plot(df, x = "WT", y = "CWRESI", fun = cont_cont)
  expect_equal(length(p), 4)
  expect_error(split_plot(ungroup(df),x = "WT", y = "CWRESI", fun = cont_cont))
})

test_that("split_plot uses TeX labeller by default", {
  skip_if_not_installed("latex2exp")
  x <- split_plot(df, fun = cont_cat, sp = "RF", x = "STUDYc", y = "WT")
  x <- ggplot_build(x[[1]])
  labeller <- x$layout$facet_params$labeller
  df <- data.frame(a = c("$\\mu$"), b = "a")
  ans <- labeller(df)
  expect_is(ans$a[[1]], "expression")
  expect_is(ans$b[[1]], "character")
})
