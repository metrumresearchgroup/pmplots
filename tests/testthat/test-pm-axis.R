library(testthat)

context("test-pm-axis")

test_that("cwresi plots use cwresi titles [PMP-TEST-033]", {
  call_formals_y <- function(fun) {
    eval(formals(fun)$y)
  }
  tgt <- "with interaction"
  x <- call_formals_y(cwresi_time)
  expect_true(grepl(tgt, x))
  x <- call_formals_y(cwresi_tad)
  expect_true(grepl(tgt, x))
  x <- call_formals_y(cwresi_tafd)
  expect_true(grepl(tgt, x))
  x <- call_formals_y(cwresi_cat)
  expect_true(grepl(tgt, x))
  x <- call_formals_y(cwresi_cont)
  expect_true(grepl(tgt, x))
  x <- call_formals_y(cwresi_pred)
  expect_true(grepl(tgt, x))
  x <- eval(formals(cwresi_hist)$x)
  expect_true(grepl(tgt, x))
  x <- eval(formals(cwresi_q)$x)
  expect_true(grepl("CWRESI", x))
})
