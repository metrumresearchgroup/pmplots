# library(testthat)
#
# context("test-pm_browser")
#
# data <- pmplots_data_obs()
# id <- pmplots_data_id()
# etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
#
# x <- pm_browser()
#
# test_that("all plots are made", {
#   expect_is(x, "tbl_df")
# })
#
# test_that("a plot can be displayed", {
#   a <- pm_browser_show(x, "cwres_time")
#   expect_is(a, "tbl_df")
# })
#
