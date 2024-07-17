library(testthat)

df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- dv_pred(df)
p <- ggplot(df, aes(TIME,DV)) + geom_point()

test_that("layer_xx [PMP-TEST-009]", {
  x0 <- layer_hs(p)
  x <- layer_hs(p, hline = NULL)
  y <- layer_s(p)
  expect_equal(length(x0$layers), 3)
  expect_equal(length(x$layers), 2)
  expect_equal(length(y$layers), 2)

  x0 <- layer_hs(p)
  x <- layer_hs(p, smooth = NULL)
  y <- layer_h(p)
  expect_equal(length(x0$layers), 3)
  expect_equal(length(x$layers), 2)
  expect_equal(length(y$layers), 2)

  x0 <- layer_sh(p)
  x <- layer_sh(p, hline = NULL)
  y <- layer_s(p)
  expect_equal(length(x0$layers), 3)
  expect_equal(length(x$layers), 2)
  expect_equal(length(y$layers), 2)

  x <- layer_a(p)
  expect_is(x,"gg")

  x <- layer_sa(p)
  expect_is(x,"gg")

  x <- layer_as(p)
  expect_is(x,"gg")
})

test_that("extra layers [PMP-TEST-010]", {
  x <- cwresi_hist(df)  %>% layer_dnorm()
  expect_is(x,"gg")

  x <- cwresi_time(df) + geom_3s()
  expect_is(x,"gg")

  x <- cwresi_time(df) %>% layer_3s()
  expect_is(x,"gg")

})


test_that("gh [PMP-TEST-011]", {
  x <- pmplots:::gh()
  expect_identical(names(x), c("yintercept", "linewidth", "col", "lty"))
  expect_identical(x$yintercept,0)
  expect_identical(x$linewidth,1.35)
  expect_identical(x$col,"darkgrey")
})


test_that("gs [PMP-TEST-012]", {
  x <- pmplots:::gs()
  expect_is(x,"list")
  expect_identical(names(x), c("method", "se", "lty", "linewidth", "col"))
  expect_identical(x$se,FALSE)
  expect_identical(x$lty,2)
  expect_identical(x$linewidth,1.35)
  expect_identical(x$col,"#3366FF")
})

test_that("ga [PMP-TEST-013]", {
  x <- pmplots:::ga()
  expect_is(x,"list")
  expect_identical(names(x), c("intercept", "slope", "col", "linewidth", "lty"))
  expect_identical(x$linewidth,1.35)
  expect_identical(x$col,"darkgrey")
  expect_identical(x$slope,1)
})

test_that("npde_ref [PMP-TEST-014]", {
  x <- npde_ref()
  expect_is(x,"list")
  expect_identical(names(x), c("yintercept", "linewidth"))
  expect_identical(x$yintercept, 0)
})
