library(testthat)

context("test-layer")


df <- pmplots_data_obs()
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- dv_pred(df)
p <- ggplot(df, aes(TIME,DV)) + geom_point()

test_that("layer_xx [PMP-TEST-009]", {
  x <- layer_hs(p, hline = NULL)
  expect_equal(x, layer_s(p))
  x <- layer_hs(p, smooth = NULL)
  expect_equal(x, layer_h(p))
  x <- layer_sh(p, hline = NULL)
  expect_equal(x,layer_s(p))
  expect_is(layer_sa(p),"gg")

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
  expect_identical(names(x), c("yintercept", "lwd", "col", "lty"))
  expect_identical(x$yintercept,0)
  expect_identical(x$lwd,1.35)
  expect_identical(x$col,"darkgrey")
})


test_that("gs [PMP-TEST-012]", {
  x <- pmplots:::gs()
  expect_is(x,"list")
  expect_identical(names(x), c("method", "se", "lty", "lwd", "col"))
  expect_identical(x$se,FALSE)
  expect_identical(x$lty,2)
  expect_identical(x$lwd,1.35)
  expect_identical(x$col,"#3366FF")
})

test_that("ga [PMP-TEST-013]", {
  x <- pmplots:::ga()
  expect_is(x,"list")
  expect_identical(names(x), c("intercept", "slope", "col", "lwd", "lty"))
  expect_identical(x$lwd,1.35)
  expect_identical(x$col,"darkgrey")
  expect_identical(x$slope,1)
})

test_that("npde_ref [PMP-TEST-014]", {
  x <- npde_ref()
  expect_is(x,"list")
  expect_identical(names(x), c("yintercept", "lwd"))
  expect_identical(x$yintercept, 0)
})
