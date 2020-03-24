
library(testthat)

context("test-opts")

def <- pm$defaults
pm$reset()

test_that("opt objects", {
  expect_is(pm_opts, "environment")
  expect_is(pm, "environment")
  expect_is(pmplots:::opts, "environment")
})

test_that("opt defaults", {
  expect_is(pm$defaults, "list")
})

test_that("opt get", {
  lwd_a <- pm$smooth.lwd
  lwd_b <- pm$get("smooth.lwd")
  expect_identical(lwd_a,lwd_b)
  expect_identical(lwd_a,1.35)
})

test_that("opt set", {
  pm$set(smooth.lwd = 2)
  expect_identical(pm$smooth.lwd,2)
  pm$smooth.lwd <- 3
  expect_identical(pm$smooth.lwd,3)
})

test_that("opt reset", {
  a <- pm$smooth.lwd
  pm$reset()
  b <- pm$smooth.lwd
  expect_identical(b,def$smooth.lwd)
  expect_false(a==b)
})

test_that("set internal data", {
  expect_warning(pm$self <- "foobar", "is not a valid option to set")
  expect_warning(pm$set <- 1235, "is not a valid option to set")
})

test_that("as.list", {
  a <- as.list(pm)
  b <- pm$defaults[names(a)]
  expect_identical(a,b)
})

test_that("bracket dot pm_opts", {
  a <- pm["scatter.size", "smooth.lty"]
  b <- pm$defaults[names(a)]
  expect_identical(a,b)
})

