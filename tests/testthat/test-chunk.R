library(testthat)

context("chunk data sets")

data <- expand.grid(amt = seq(25))
data$ID  <- seq(nrow(data))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]

test_that("test-chunk chunk data", {
  x <- pmplots:::chunk_by_id(data, id_per_chunk=5)
  expect_identical(length(x), 5L)

  x2 <- pmplots:::chunk_by_id(data2, id_per_chunk = 5, id_col = "SUBJ")
  expect_identical(length(x), 5L)

  x2 <- pmplots:::chunk_by_id(data2, id_per_chunk = 5, mark = "test")
  expect_true(exists("test", x2[[3]]))
  expect_true(all(x[[4]][["test"]]==4))
})

test_that("test-chunk chunk data by multiple cols", {
  data <- data.frame(a = c(rep("a", 3), rep("b", 4)),
                     b = c(rep("a", 4), rep("b", 2), "c"))
  chunked <- pmplots:::chunk_by_cols(data, id_per_chunk = 2, cols = c("a", "b"))
  expect_equal(length(chunked), 2)
  tot <- sum(vapply(chunked, nrow, 1L))
  expect_equal(tot, nrow(data))
})

test_that("test-chunk chunk bad input", {
  expect_error(pmplots:::chunk_by_id(list(), 5))
  expect_error(pmplots:::chunk_by_id(matrix(0), 5))
  expect_error(pmplots:::chunk_by_id(data, 0))
  expect_error(pmplots:::chunk_by_id(data, "A"))
  expect_error(pmplots:::chunk_by_id(data, "kyletbaron"))
  expect_error(pmplots:::chunk_by_id(data, 26))
  expect_is(pmplots:::chunk_by_id(data,25),"list")
  expect_error(pmplots:::chunk_by_id(data,4,id_col="FOO"))
})
