library(testthat)

context("test-pm")

df <- pmplots_data_obs()
df[["CWRES"]] <- df[["CWRESI"]]
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
require(rlang)

expect_labels <- function(object, x, y) {
  expected <- c(x,y)
  mp <- object$mapping
  act <- c(quo_name(mp$x), quo_name(mp$y))
  expect(identical(act,expected), "Plot labels are not correct.")
}

expect_titles <- function(object, x, y) {
  expected <- c(x,y)
  act <- c(object$labels$x,object$labels$y)
  expect(identical(act,expected), "Plot title is not correct.")
}

expect_x <- function(object, x, name) {
  expected <- c(x,name)
  mp <- object$mapping
  act <- c(quo_name(mp$x),object$labels$x)
  expect(identical(act,expected), "x-axis elements are not correct.")
}

expect_y <- function(object, y, name) {
  expected <- c(y,name)
  mp <- object$mapping
  act <- c(quo_name(mp$y),object$labels$y)
  expect(identical(act,expected), "y-axis elements are not correct.")
}

test_that("check", {
  p <- dv_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "DV")
  expect_titles(p, "Time (hr)", "Observed DV")
})


test_that("every function", {

  p <- dv_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "DV")
  expect_titles(p, "Time (hr)", "Observed DV")

  p <- dv_tad(df)
  expect_is(p,"gg")
  p <- dv_tafd(df)
  expect_is(p,"gg")
  p <- dv_tafd(df,log = TRUE)
  expect_is(p,"gg")
  p <- dv_tad(df,log = TRUE)
  expect_is(p,"gg")



  p <- dv_pred(df, yname="NoDoze (ng/ml)")
  expect_is(p, "gg")
  expect_labels(p, "PRED", "DV")
  expect_titles(p, "Population predicted NoDoze (ng/ml)", "Observed NoDoze (ng/ml)")

  p <- dv_ipred(df)
  expect_is(p, "gg")
  expect_labels(p, "IPRED", "DV")
  expect_titles(p, "Individual predicted value", "Observed value")


  p <- dv_pred(df,loglog=TRUE)
  expect_is(p, "gg")
  expect_labels(p, "PRED", "DV")
  expect_titles(p, "Population predicted value", "Observed value")

  p <- dv_ipred(df,loglog=TRUE)
  expect_is(p, "gg")
  expect_labels(p, "IPRED", "DV")
  expect_titles(p, "Individual predicted value", "Observed value")

  p <- dv_preds(df)
  expect_is(p,"list")
  expect_equal(length(p),2)

  p <- res_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "RES")
  expect_titles(p, "Time (hr)", "Residual")

  p <- wres_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "WRES")
  expect_titles(p, "Time (hr)", "Weighted residual")

  p <- cwresi_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "CWRESI")
  expect_titles(p, "Time (hr)", "Conditional weighted residual")

  p <- res_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "RES")
  expect_titles(p, "Time after dose (hr)", "Residual")

  p <- wres_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "WRES")
  expect_titles(p, "Time after dose (hr)", "Weighted residual")

  p <- cwresi_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "CWRESI")
  expect_titles(p, "Time after dose (hr)", "Conditional weighted residual")

  p <- res_pred(df)
  expect_is(p, "gg")
  expect_labels(p, "PRED", "RES")
  expect_titles(p, "Population predicted value", "Residual")

  p <- wres_pred(df)
  expect_is(p, "gg")
  expect_labels(p, "PRED", "WRES")
  expect_titles(p, "Population predicted value", "Weighted residual")

  p <- cwresi_pred(df)
  expect_is(p, "gg")
  expect_labels(p, "PRED", "CWRESI")
  expect_titles(p, "Population predicted value", "Conditional weighted residual")

  p <- cwresi_cont(df, x="WT//Weight (kg)")
  expect_is(p, "gg")
  expect_labels(p, "WT", "CWRESI")
  expect_titles(p, "Weight (kg)", "Conditional weighted residual")

  expect_error(cwres_cont(df, x="WT/Weight (kg)"))

  p <- res_cont(df, "WT")
  expect_is(p,"gg")
  p <- cwres_cont(df,"WT")
  expect_is(p,"gg")

  p <- res_cat(df, x="STUDYc//Study")
  expect_is(p, "gg")
  expect_labels(p, "STUDYc", "RES")
  expect_titles(p, "Study", "Residual")

  p <- wres_cat(df, x="STUDYc//Study")
  expect_is(p, "gg")
  expect_labels(p, "STUDYc", "WRES")
  expect_titles(p, "Study", "Weighted residual")

  p <- cwresi_cat(df, x="STUDYc//Study")
  expect_is(p, "gg")
  expect_labels(p, "STUDYc", "CWRESI")
  expect_titles(p, "Study", "Conditional weighted residual")

  p <- eta_hist(df,etas)
  expect_is(p, "list")
  p <- p[[1]]
  expect_titles(p, "ETA-CL", "count")

  p <- eta_cont(df, x="WT//Weight (kg)", y=etas)
  expect_is(p, "list")
  expect_labels(p[[1]], "WT", "ETA1")
  expect_titles(p[[1]], "Weight (kg)", "ETA-CL")

  p <- eta_cat(df, x="STUDYc//Study", y=etas)
  expect_is(p, "list")
  expect_labels(p[[2]], "STUDYc", "ETA2")
  expect_titles(p[[2]], "Study", "ETA-V2")
  p <- eta_cat(df, x = "STUDYc", y = etas[1])
  expect_is(p, "gg")



  p <- npde_time(df)
  expect_is(p, "gg")
  expect_labels(p, "TIME", "NPDE")
  expect_titles(p, "Time (hr)", "NPDE")

  p <- npde_tad(df)
  expect_is(p, "gg")
  expect_labels(p, "TAD", "NPDE")
  expect_titles(p, "Time after dose (hr)", "NPDE")

  p <- npde_tafd(df)
  expect_is(p, "gg")
  expect_labels(p, "TAFD", "NPDE")
  expect_titles(p, "Time after first dose (hr)", "NPDE")

  p <- wres_hist(df)
  expect_is(p, "gg")
  expect_x(p, "WRES", "Weighted residual")

  p <- cwresi_hist(df)
  expect_is(p, "gg")
  expect_x(p, "CWRESI", "Conditional weighted residual")

  p <- eta_pairs(df, "ETA1//ETA-CL")
  expect_is(p, "gg")
  expect_x(p, "ETA1", "ETA-CL")
  p2 <- pairs_plot(df, c("ETA1", "ETA2"))
  expect_is(p2,"gg")

  p <- eta_pairs(
    df,
    c("ETA1", "ETA2"),
    smooth_color = "red",
    smooth_lty=1
  )
  expect_is(p, "gg")

  expect_is(cwresi_q(df),"gg")
  expect_is(wres_q(df),"gg")
  expect_is(npde_q(df),"gg")

  p <- res_cont(df, x = c("WT", "ALB"))
  expect_is(p, "list")
  expect_equal(length(p), 2)

  p <- eta_pairs(df, c("ETA1//ETA-CL", "ETA2//ETA-V2"))
  expect_is(p,"ggmatrix")
  p2 <- pairs_plot(df, c("ETA1//ETA-CL", "ETA2//ETA-V2"))
  expect_equal(p,p2)
})




test_that("Axis title customization", {
  p <- cwresi_time(df, xunit="min")
  expect_titles(p, "Time (min)", "Conditional weighted residual")

  p <- cwresi_time(df, x = "TIME//Study time {xunit}")
  expect_titles(p, "Study time (hr)", "Conditional weighted residual")

  p <- cwresi_time(df, x = "TIME//Study time (seconds)")
  expect_titles(p, "Study time (seconds)", "Conditional weighted residual")

})


test_that("pm theme", {

  expect_identical(pm_theme(), theme_bw())

  p <- ggplot(df, aes(PRED,DV)) + geom_point()
  expect_is(p+pm_theme(), "gg")
  expect_is(p+pm_smooth(), "gg")
  expect_is(p+pm_hline(), "gg")
  expect_is(p+pm_abline(), "gg")
  expect_is(p+theme_plain(), "gg")
  expect_is(p+pm_smooths(), "gg")
})


test_that("list plots", {
  x <- c("WT", "CRCL", "ALB")
  etas <- c("ETA1", "ETA2")

  p <- list_plot_x(df, x, "ETA1")
  expect_is(p, "list")
  expect_equal(length(p),length(x))

  p <- list_plot_y(df, x[1], etas)
  expect_is(p, "list")
  expect_equal(length(p),length(etas))

  p <- list_plot_xy(df, x, etas)
  expect_is(p, "list")
  expect_equal(length(p),length(etas)*length(x))

})


test_that("eta labs", {
  x <- eta_labs(CL,V2,KA)
  expect_identical(x, c("ETA-CL", "ETA-V2", "ETA-KA"))

  x <- eta_col_labs(CL, KA)
  expect_identical(x, c(`ETA-CL` = "ETA1//ETA-CL", `ETA-KA` = "ETA2//ETA-KA"))
})

