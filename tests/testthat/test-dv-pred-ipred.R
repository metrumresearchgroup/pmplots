library(testthat)

context("test-dv-pred-ipred")

df <- pmplots_data_obs()

test_that("dv-pred-ipred basics", {
  nid <- length(unique(df$ID))
  x <- dv_pred_ipred(df, id_per_plot = 5)
  expect_is(x, "list")
  expect_equal(length(x), nid/5)

  p2 <- suppressMessages(ggplot_build(x[[2]]))
  data1 <- p2$data[[1]]
  data2 <- p2$data[[2]]
  expect_equal(unique(data1$PANEL), factor(c(1, 2, 3, 4, 5)))

  dd1 <- dplyr::distinct(data1, colour, linetype, group, size)
  expect_true(all(dd1$size==0.5))
  expect_equal(dd1$group, c(1, 2, 3))
  expect_equal(dd1$colour, c("red2", "blue2", "black"))
  expect_equal(dd1$linetype, c(1, 2, 0))

  dd2 <- dplyr::distinct(data2, colour, shape, group, size)
  expect_true(all(dd2$shape==19))
  expect_equal(as.integer(dd2$group), c(1, 2, 3))
  expect_equal(dd2$colour, c("red2", "blue2", "black"))
  expect_true(all(dd2$size ==1.5))
})

test_that("dv-pred-ipred chunking", {
  nid <- length(unique(df$ID))
  x <- dv_pred_ipred(df, id_per_plot = 11)
  d <- lapply(x, "[[", "data")
  nn <- sapply(d, function(dd) length(unique(dd$ID)))
  expect_equal(length(x), length(nn))
  expect_equal(sum(nn), nid)
  m <- length(x) -1
  # We filled up the first n-1 chunks with 11
  expect_true(all(nn[seq(m)]==11))
  remainder <- nid - sum(nn[seq(m)])
  # The last page has the rest
  expect_equal(unname(nn[length(x)]), remainder)
})
