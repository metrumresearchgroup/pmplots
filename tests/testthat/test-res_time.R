library(testthat)

context("test-pm")

df <- pmplots_data_obs()
df[["TAFD"]] <- df[["TIME"]]
df[["CWRES"]] <- df[["CWRESI"]]
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
require(rlang)


expect_labels <- function(object, x, y) {
  expected <- c(x,y)
  mp <- object$mapping
  act <- c(quo_name(mp$x), quo_name(mp$y))
  expect(identical(act,expected), "Plot labels are not correct.")
}

expect_titles <- function(object, x, y) {
  expected <- c(x,y)
  act <- c(object$labels$x,object$labels$y)
  expect(identical(act,expected), "Plot title is not correct.")
}

expect_x <- function(object, x, name) {
  expected <- c(x,name)
  mp <- object$mapping
  act <- c(quo_name(mp$x),object$labels$x)
  expect(identical(act,expected), "x-axis elements are not correct.")
}

expect_y <- function(object, y, name) {
  expected <- c(y,name)
  mp <- object$mapping
  act <- c(quo_name(mp$y),object$labels$y)
  expect(identical(act,expected), "y-axis elements are not correct.")
}


test_that("res time", {

  p <- res_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "RES")
  expect_titles(p, "Time (hr)", "Residual")

  p <- wres_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "WRES")
  expect_titles(p, "Time (hr)", "Weighted residual")

  p <- cwresi_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "CWRESI")
  expect_titles(p, "Time (hr)", "Conditional weighted residual")

  p <- cwres_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "CWRES")
  expect_titles(p, "Time (hr)", "Conditional weighted residual")

  p <- res_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "RES")
  expect_titles(p, "Time after dose (hr)", "Residual")

  p <- wres_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "WRES")
  expect_titles(p, "Time after dose (hr)", "Weighted residual")

  p <- cwresi_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "CWRESI")
  expect_titles(p, "Time after dose (hr)", "Conditional weighted residual")

  p <- cwres_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "CWRES")
  expect_titles(p, "Time after dose (hr)", "Conditional weighted residual")

  p <- res_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "RES")
  expect_titles(p, "Time after first dose (hr)", "Residual")

  p <- wres_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "WRES")
  expect_titles(p, "Time after first dose (hr)", "Weighted residual")

  p <- cwresi_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "CWRESI")
  expect_titles(p, "Time after first dose (hr)", "Conditional weighted residual")

  p <- cwres_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "CWRES")
  expect_titles(p, "Time after first dose (hr)", "Conditional weighted residual")


})

test_that("npde", {
  p <- npde_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "NPDE")
  expect_titles(p, "Time (hr)", "NPDE")

  p <- npde_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "NPDE")
  expect_titles(p, "Time after dose (hr)", "NPDE")

  p <- npde_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "NPDE")
  expect_titles(p, "Time after first dose (hr)", "NPDE")
})
