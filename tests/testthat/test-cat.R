library(testthat)

context("test-cat")

df <- pmplots_data_obs()
id <- pmplots_data_id()

test_that("points on top of box plot issue-13 [PMP-TEST-003]", {
  a <- cont_cat(id, y = "WT", x = "STUDYc")
  b <- cont_cat(id, y = "WT", x = "STUDYc", points = TRUE)
  points <- list(alpha = 0.8, position = position_jitter(width = 0.1))
  c <- cont_cat(id, y = "WT", x = "STUDYc", points = points)
  expect_is(b, "gg")
  expect_is(c, "gg")
  expect_false(identical(a,b))
  expect_false(identical(a,c))
})

test_that("vector inputs for cont_cat [PMP-TEST-001]", {
  y <- c("ALB", "BMI", "SCR")
  x <- c("RF", "CPc", "STUDYc")
  ans <- cont_cat(df,x,y[1])
  expect_length(ans,length(x))
  ans <- cont_cat(df,x[1],y)
  expect_length(ans,length(y))
  ans <- cont_cat(df,x,y)
  expect_length(ans,length(x)*length(y))
})

