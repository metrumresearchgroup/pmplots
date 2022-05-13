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

test_that("pm-axis glue in axis data [PMP-TEST-080]", {
  a <- pm_axis_time("hr")
  expect_equal(a, "TIME//Time (hr)")
  b <- pm_axis_tad("hour")
  expect_equal(b, "TAD//Time after dose (hour)")
  c <- pm_axis_tafd("h")
  expect_equal(c, "TAFD//Time after first dose (h)")
  d <- pm_axis_dv("conc (ng/mL)")
  expect_equal(d, "DV//Observed conc (ng/mL)")
  e <- pm_axis_pred("conc (ng/mL)")
  expect_equal(e, "PRED//Population predicted conc (ng/mL)")
  f <- pm_axis_ipred("conc (ng/mL)")
  expect_equal(f, "IPRED//Individual predicted conc (ng/mL)")
})

test_that("pm-axis error when asking for undefined item", {
  expect_error(
    pm_axis("fop"),
    regexp = "cannot find axis data for `fop`"
  )
})
