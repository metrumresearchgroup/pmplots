library(testthat)

context("test-data")

test_that("data [PMP-TEST-008]", {
  expect_is(pmplots_data(), "data.frame")
  expect_is(pmplots_data_obs(), "data.frame")
  expect_is(pmplots_data_id(), "data.frame")
})
