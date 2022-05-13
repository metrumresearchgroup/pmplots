library(testthat)

context("chunk data sets")

data <- expand.grid(amt = seq(25))
data$ID  <- seq(nrow(data))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]

test_that("test-chunk chunk data", {
  x <- pmplots:::chunk_by_cols(data, cols = "ID", id_per_chunk = 5)
  expect_identical(length(x), 5L)
  x2 <- pmplots:::chunk_by_cols(data2, id_per_chunk = 5, cols = "SUBJ")
  expect_identical(length(x), 5L)
})

test_that("test-chunk chunk data by multiple cols", {
  data <- data.frame(a = c(rep("a", 3), rep("b", 4)),
                     b = c(rep("a", 4), rep("b", 2), "c"))
  chunked <- pmplots:::chunk_by_cols(data, id_per_chunk = 2, cols = c("a", "b"))
  expect_equal(length(chunked), 2)
  tot <- sum(vapply(chunked, nrow, 1L))
  expect_equal(tot, nrow(data))
})
