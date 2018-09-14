library(testthat)

context("test-pm")

df <- pmplots_data() %>% dplyr::filter(EVID==0)
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

test_that("every function", {

  p <- dv_time(df)
  expect_is(p, "gg")

  p <- dv_pred(df, what="NoDoze (ng/ml)")
  expect_is(p, "gg")

  p <- dv_ipred(df)
  expect_is(p, "gg")

  p <- dv_pred(df,loglog=TRUE)
  expect_is(p, "gg")

  p <- dv_ipred(df,loglog=TRUE)
  expect_is(p, "gg")

  p <- res_time(df)
  expect_is(p, "gg")

  p <- wres_time(df)
  expect_is(p, "gg")

  p <- cwresi_time(df)
  expect_is(p, "gg")

  p <- res_pred(df)
  expect_is(p, "gg")

  p <- wres_pred(df)
  expect_is(p, "gg")

  p <- cwresi_pred(df)
  expect_is(p, "gg")

  p <- cwresi_cont(df, x="WT//Weight (kg)")
  expect_is(p, "gg")

  expect_error(cwres_cont(df, x="WT/Weight (kg)"))

  p <- cwresi_cat(df, x="STUDYc//Study")
  expect_is(p, "gg")

  p <- eta_hist(df,etas)
  expect_is(p, "list")

  p <- eta_cont(df, x="WT//Weight (kg)", y=etas)
  expect_is(p, "list")

  p <- eta_cat(df, x="STUDYc//Study", y=etas)
  expect_is(p, "list")

})


