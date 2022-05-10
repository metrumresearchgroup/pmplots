library(testthat)

context("test-wrap")

df <- pmplots_data_obs()
df[["TAFD"]] <- df[["TIME"]]
df[["CWRES"]] <- df[["CWRESI"]]
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")


test_that("wrap_cont_cont [PMP-TEST-070]", {
  p <- wrap_cont_cont(df, x = "WT", y = c("RES", "WRES"))
  expect_is(p, "gg")

  p <- wrap_cont_cont(df, y = "WT",x = c("RES", "WRES"))
  expect_is(p, "gg")

  expect_error(wrap_cont_cont(df, y = etas, x = etas))

  expect_error(wrap_cont_cont(df, x = c("WT", "BMI"), y = c("CWRES", "RES")))

})

test_that("wrap_eta_cont [PMP-TEST-071]", {
  p <- wrap_eta_cont(df, x = "WT", y = etas)
  expect_is(p, "gg")
})

test_that("use labels [PMP-TEST-072]", {
  p <- wrap_eta_cont(df, x = "WT", y = etas, use_labels=TRUE)
  expect_is(p, "gg")
})

test_that("fill in title [PMP-TEST-073]", {
  p <- wrap_eta_cont(df, x = "WT", y = etas, title="etas")
  expect_is(p, "gg")
})

test_that("wrap_hist [PMP-TEST-074]", {
  p <- wrap_hist(df, x = etas)
  expect_is(p, "gg")
})

test_that("wrap_res_time [PMP-TEST-075]", {
  p <- wrap_res_time(df, y = c("WRES", "NPDE"))
  expect_is(p, "gg")
})

test_that("wrap_cont_time [PMP-TEST-076]", {
  p <- wrap_cont_time(df, y = c("WRES", "NPDE"))
  expect_is(p, "gg")

  p <- wrap_cont_time(df, y = c("WT//WT (kg)", "BMI//BMI (kg/m$^2$)"))
  expect_is(p,"gg")
})

test_that("wrap_dv_preds [PMP-TEST-077]", {
  p <- wrap_dv_preds(df)
  expect_is(p, "gg")
})

test_that("wrap with expression [PMP-TEST-078]", {
  p <- wrap_cont_cont(df, x = "TIME", y = c("CWRES", "DV//Conc ($\\mu$M)"))
  expect_is(p,"gg")
})

test_that("wrap_cont_cat [PMP-TEST-079]", {
  p <- wrap_cont_cat(df, y = c("WRES", "NPDE"), x = "STUDYc")
  expect_is(p, "gg")
  ans <- wrap_cont_cat(df, y = c("WRES", "NPDE"), x = c("STUDYc", "CPc"))
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_equal(p,ans[[1]])
})
