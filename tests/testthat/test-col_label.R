library(testthat)

context("test-col_label")

test_that("col_label", {

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
})


