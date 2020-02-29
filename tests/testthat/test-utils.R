library(testthat)

context("test-utils")
data <- pmplots_data_obs()
p <- dv_time(data)

test_that("CWRESI gets subbed for CWRES", {
  data$CWRES <- NULL
  expect_is(cwres_time(data), "gg")
  expect_message(cwres_time(data),"Creating CWRES column from CWRESI")
})

test_that("log scale", {
  x <- log_scale()
  expect_is(x, "list")
  expect_equal(x$trans, "log10")
  expect_equal(x$breaks, NULL)
  x <- log_scale(logbr())
  expect_equal(x$breaks, 10^seq(-10,10))
})

test_that("rot_x and rot_y", {
  expect_is(p + rot_x(), "gg")
  expect_is(p + rot_y(), "gg")
})

test_that("def", {
  x <- defx(breaks = c(1,2,3))
  expect_equal(x$breaks, c(1,2,3))
  expect_equal(x$position, "bottom")
  x <- defy(breaks = c(1,2,3))
  expect_equal(x$breaks, c(1,2,3))
  expect_equal(x$position, "left")
  x <- defcx()
  expect_equal(x$position, "bottom")
})

test_that("logbr", {
  x <- logbr()
  expect_equal(x, 10^seq(-10,10))
  y <- logbr3()
  expect_equal(y, sort(c(x,3*x)))

})

test_that("char", {
  expect_true(pmplots:::charthere("kyle", "k"))
  expect_equal(pmplots:::charcount("mississippi", "i"),4)
})

test_that("search col name", {
  a <- pmplots:::search_cwres_i("CWRES", data)
  expect_equal(a,"CWRESI")
})

test_that("args are passed to rot_x and rot_y", {
  p <- dv_time(data) + rot_x(hjust = 0, vjust = 0)
  expect_is(p, "gg")
  p <- dv_time(data) + rot_y(hjust = 0, vjust = 0)
  expect_is(p, "gg")
  p <- dv_time(data) + rot_x(vert = TRUE)
  expect_is(p, "gg")
})

