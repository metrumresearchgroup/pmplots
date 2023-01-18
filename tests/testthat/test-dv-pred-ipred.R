library(testthat)
library(pmplots)

context("test-dv-pred-ipred")

df <- pmplots_data_obs()

data <- subset(df, ID <= 9*2)
data$OCC <- as.integer(data$TIME > 12)
data1 <- subset(data, ID <= 9)

test_that("dv-pred-ipred basics [PMP-TEST-081]", {
  nid <- length(unique(df$ID))
  x <- dv_pred_ipred(df, id_per_plot = 5)
  expect_is(x, "list")
  expect_equal(length(x), nid/5)

  p2 <- suppressMessages(ggplot_build(x[[2]]))
  data1 <- p2$data[[1]]
  data2 <- p2$data[[2]]
  expect_equal(unique(data1$PANEL), factor(c(1, 2, 3, 4, 5)))

  dd1 <- dplyr::distinct(data1, colour, linetype, group, linewidth)
  expect_true(all(dd1$linewidth==0.5))
  expect_equal(dd1$group, c(1, 2, 3))
  expect_equal(dd1$colour, c("red2", "blue2", "black"))
  expect_equal(dd1$linetype, c(1, 2, 0))

  dd2 <- dplyr::distinct(data2, colour, shape, group, size)
  expect_true(all(dd2$shape==19))
  expect_equal(as.integer(dd2$group), c(1, 2, 3))
  expect_equal(dd2$colour, c("red2", "blue2", "black"))
  expect_true(all(dd2$size ==1.5))
})

test_that("dv-pred-ipred - multiple facet [PMP-TEST-082]", {
  # expecting 2 occasions per ID
  nid <- length(unique(data$ID))
  ans <- dv_pred_ipred(
    data,
    facets = c("ID", "OCC"),
    id_per_plot = 4, ncol = 2
  )
  expect_equal(length(ans), nid*2/4)
})

test_that("dv-pred-ipred glue facet  [PMP-TEST-083]", {
  ans <- dv_pred_ipred(
    data1,
    facets = c("ID//ID: {ID}", "OCC//Occasion: {OCC}"),
    id_per_plot = 4, ncol = 2
  )
  expect_is(ans[[1]], "gg")
})

test_that("dv-pred-ipred lines  [PMP-TEST-084]", {
  ans <- dv_pred_ipred(data1, ipred_lty = 0, pred_lty = 0)
  ans <- ggplot_build(ans[[1]])
  # lines is layer 1
  expect_true(all(ans$data[[1]]$linetype==0))
  # points layer 2
  expect_true(all(ans$data[[2]]$shape==19))
})

test_that("dv-pred-ipred points  [PMP-TEST-085]", {
  ans <- dv_pred_ipred(data1, ipred_point = FALSE, pred_point = FALSE)
  ans <- ggplot_build(ans[[1]])
  u <- unique(ans$data[[1]][, c("colour", "linetype")])
  # lines is layer 1
  expect_equal(u$colour, c("red2", "blue2", "black"))
  expect_equal(u$linetype, c(1, 2, 0))
  # points layer 2
  u <- unique(ans$data[[2]][, c("colour", "shape")])
  expect_equal(u$colour, c("red2", "blue2", "black"))
  expect_equal(u$shape, c(NA, NA, 19))
})

test_that("dv-pred-ipred margin  [PMP-TEST-086]", {
  ans <- dv_pred_ipred(data1, plot.margin = margin(0, 2, 0, 0, unit = "in"))
  ans <- ggplot_build(ans[[1]])
  mar <- ans$plot$theme$plot.margin
  expect_equal(as.numeric(mar[1]), 0)
  expect_equal(as.numeric(mar[2]), 2)
  expect_equal(as.numeric(mar[3]), 0)
  expect_equal(as.numeric(mar[4]), 0)
})

test_that("dv-pred-ipred chunking even [PMP-TEST-087]", {
  nid <- length(unique(df$ID))
  .nn <- 16
  x <- dv_pred_ipred(df, id_per_plot = .nn)
  d <- lapply(x, "[[", "data")
  nn <- sapply(d, function(dd) length(unique(dd$ID)))
  expect_equal(length(x), length(nn))
  expect_equal(sum(nn), nid)
  m <- length(x) -1
  # We filled up the first n-1 chunks with 11
  expect_true(all(nn[seq(m)]==.nn))
  remainder <- nid - sum(nn[seq(m)])
  # The last page has the rest
  expect_equal(unname(nn[length(x)]), remainder)
})

test_that("dv-pred-ipred chunking odd [PMP-TEST-088]", {
  nid <- length(unique(df$ID))
  .nn <- 11
  x <- dv_pred_ipred(df, id_per_plot = .nn)
  d <- lapply(x, "[[", "data")
  nn <- sapply(d, function(dd) length(unique(dd$ID)))
  expect_equal(length(x), length(nn))
  expect_equal(sum(nn), nid)
  m <- length(x) -1
  # We filled up the first n-1 chunks with 11
  expect_true(all(nn[seq(m)]==.nn))
  remainder <- nid - sum(nn[seq(m)])
  # The last page has the rest
  expect_equal(unname(nn[length(x)]), remainder)
})
