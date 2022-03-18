library(testthat)

context("test-wrap")

df <- pmplots_data_obs()
df[["TAFD"]] <- df[["TIME"]]
df[["CWRES"]] <- df[["CWRESI"]]
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")


test_that("wrap_cont_cont", {
  p <- wrap_cont_cont(df, x = "WT", y = c("RES", "WRES"))
  expect_is(p, "gg")

  p <- wrap_cont_cont(df, y = "WT",x = c("RES", "WRES"))
  expect_is(p, "gg")

  expect_error(wrap_cont_cont(df, y = etas, x = etas))

  expect_error(wrap_cont_cont(df, x = c("WT", "BMI"), y = c("CWRES", "RES")))

})

test_that("wrap_eta_cont", {
  p <- wrap_eta_cont(df, x = "WT", y = etas)
  expect_is(p, "gg")
})

test_that("use labels", {
  p <- wrap_eta_cont(df, x = "WT", y = etas, use_labels=TRUE)
  expect_is(p, "gg")
})

test_that("fill in title", {
  p <- wrap_eta_cont(df, x = "WT", y = etas, title="etas")
  expect_is(p, "gg")
})

test_that("wrap_hist", {
  p <- wrap_hist(df, x = etas)
  expect_is(p, "gg")
})

test_that("wrap_res_time", {
  p <- wrap_res_time(df, y = c("WRES", "NPDE"))
  expect_is(p, "gg")
})

test_that("wrap_cont_time", {
  p <- wrap_cont_time(df, y = c("WRES", "NPDE"))
  expect_is(p, "gg")

  p <- wrap_cont_time(df, y = c("WT//WT (kg)", "BMI//BMI (kg/m$^2$)"))
  expect_is(p,"gg")
})

test_that("wrap_dv_preds", {
  p <- wrap_dv_preds(df)
  expect_is(p, "gg")
})

test_that("wrap with expression", {
  p <- wrap_cont_cont(df, x = "TIME", y = c("CWRES", "DV//Conc ($\\mu$M)"))
  expect_is(p,"gg")
})

test_that("wrap_cont_cat", {
  p <- wrap_cont_cat(df, y = c("WRES", "NPDE"), x = "STUDYc")
  expect_is(p, "gg")
  ans <- wrap_cont_cat(df, y = c("WRES", "NPDE"), x = c("STUDYc", "CPc"))
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_equal(p,ans[[1]])
})
