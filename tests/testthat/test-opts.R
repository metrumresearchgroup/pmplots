
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

