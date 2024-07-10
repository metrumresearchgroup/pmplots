library(testthat)

context("test-col_label")

test_that("col_label [PMP-TEST-005]", {

  x <- col_label("WT // Weight (kg)")
  expect_length(x,2)
  expect_identical(x[1], "WT")
  expect_identical(x[2], "Weight (kg)")

  x <- col_label("WT // Weight (kg)")
  expect_length(x,2)

  x <- col_label("WT !! Weight (kg)")
  expect_length(x,2)

  x <- col_label("WT $$ Weight (kg)")
  expect_length(x,2)

  x <- col_label("WT @@ Weight (kg)")
  expect_length(x,2)

  expect_error(col_label("WT / Weight (kg)"))

  x <- col_label("WT")
  expect_length(x,2)
  expect_identical(x, c("WT", "WT"))

  expect_is(pmplots:::parse_label("!! foo"), "expression")
  expect_is(pmplots:::parse_label("foo $\\mu$"), "expression")
})

test_that("punctuation in col-label issue-72 [PMP-TEST-006]", {
  x <- col_label("wt_baseline")
  expect_equal(x[1], "wt_baseline")
  expect_equal(x[2], "wt_baseline")
  expect_equal(
    col_label("wt.baseline//Baseline Weight"),
    c("wt.baseline", "Baseline Weight")
  )
})

test_that("get col part from a col-label vector", {
  x <- "a//first"
  expect_identical(pmplots:::col_label_col(x), "a")

  x <- c("a//first", "b@@second", "c//third")
  expect_identical(pmplots:::col_label_col(x), c("a", "b", "c"))
})

test_that("col_label accepts list or vector", {
  x <- c("a//b", "c@@d")
  a <- col_label(x)
  b <- col_label(as.list(x))
  expect_identical(a,b)
})

test_that("col_labels accepts list or vector", {
  x <- c("a//b", "c@@d")
  a <- pmplots:::col_labels(x)
  b <- pmplots:::col_labels(as.list(x))
  expect_identical(a,b)
})
