library(testthat)
library(pmplots)

spec <- list(DV = "Concentration (ng/mL)", WT = "Weight (kg)", TAFD = "Time after first dose (hr)")
dv_pred_spec <- list(DV = "Concentration (ng/mL)", PRED = "Population prediction (ng/mL)")

test_that("pm_label_columns sets pmp.axis.label attributes on named columns", {
  data <- pmplots_data_obs()
  out <- pm_label_columns(data, spec)
  expect_equal(attr(out[["DV"]], "pmp.axis.label"), "Concentration (ng/mL)")
  expect_equal(attr(out[["WT"]], "pmp.axis.label"), "Weight (kg)")
})

test_that("pm_label_columns does not label columns absent from spec", {
  data <- pmplots_data_obs()
  out <- pm_label_columns(data, spec)
  unlabeled <- names(data)[!names(data) %in% names(spec)]
  for(col in unlabeled) {
    expect_null(attr(out[[col]], "pmp.axis.label"), label = col)
  }
})

test_that("pm_label_columns ignores spec entries not in the data frame", {
  data <- pmplots_data_obs()
  extra_spec <- c(spec, list(NOTACOL = "Not a real column"))
  expect_warning(
    out <- pm_label_columns(data, extra_spec[c("NOTACOL")]),
    regexp = "No columns were labeled"
  )
  expect_null(attr(out[["DV"]], "pmp.axis.label"))
})

test_that("pm_label_columns labs argument overrides spec", {
  data <- pmplots_data_obs()
  override <- list(DV = "Observed Drug X (ng/mL)")
  out <- pm_label_columns(data, spec, labs = override)
  expect_equal(attr(out[["DV"]], "pmp.axis.label"), "Observed Drug X (ng/mL)")
})

test_that("pm_label_columns requires a data frame", {
  expect_error(pm_label_columns(list(DV = 1), spec))
})

test_that("pm_label_rm removes pmp.axis.label from all columns", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, spec)
  expect_equal(attr(labeled[["DV"]], "pmp.axis.label"), "Concentration (ng/mL)")

  cleaned <- pm_label_rm(labeled)
  for(col in names(cleaned)) {
    expect_null(attr(cleaned[[col]], "pmp.axis.label"), label = col)
  }
})

test_that("pm_label_rm is a no-op on a data frame with no labels", {
  data <- pmplots_data_obs()
  out <- pm_label_rm(data)
  expect_equal(out, data)
})

# validate_label_list (exercised via pm_gg_labs and pm_label_columns) ----------

test_that("pm_gg_labs errors when spec is not a list", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pm_gg_labs(spec = c(DV = "Concentration (ng/mL)")), regexp = "spec")
})

test_that("pm_gg_labs errors when spec is an unnamed list", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pm_gg_labs(spec = list("Concentration (ng/mL)")), regexp = "spec")
})

test_that("pm_gg_labs errors when a spec value is not a length-1 character", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pm_gg_labs(spec = list(DV = c("a", "b"))), regexp = "spec")
  expect_error(dv_pred(data) + pm_gg_labs(spec = list(DV = 1L)), regexp = "spec")
})

test_that("pm_gg_labs errors when labs fails validation", {
  data <- pmplots_data_obs()
  expect_error(
    dv_pred(data) + pm_gg_labs(spec = dv_pred_spec, labs = list(DV = c("a", "b"))),
    regexp = "labs"
  )
})

test_that("pm_label_columns errors when spec values are not length-1 characters", {
  data <- pmplots_data_obs()
  expect_error(pm_label_columns(data, spec = list(DV = c("a", "b"))), regexp = "spec")
})

# pm_gg_labs -------------------------------------------------------------------

test_that("pm_gg_labs sets x and y axis labels from spec", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pm_gg_labs(dv_pred_spec)
  expect_equal(p$labels$x, "Population prediction (ng/mL)")
  expect_equal(p$labels$y, "Concentration (ng/mL)")
})

test_that("pm_gg_labs leaves labels unchanged for columns not in spec", {
  data <- pmplots_data_obs()
  p_base <- dv_pred(data)
  p_labeled <- p_base + pm_gg_labs(list(DV = "Concentration (ng/mL)"))
  # y (DV) should update; x (PRED) should stay as the default
  expect_equal(p_labeled$labels$y, "Concentration (ng/mL)")
  expect_equal(p_labeled$labels$x, p_base$labels$x)
})

test_that("pm_gg_labs labs argument overrides spec for the same column", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pm_gg_labs(
    spec = dv_pred_spec,
    labs = list(DV = "Observed Drug X (ng/mL)")
  )
  expect_equal(p$labels$y, "Observed Drug X (ng/mL)")
})

test_that("pm_gg_labs labs overrides spec for the same column", {
  data <- pmplots_data_obs()
  labs_override <- list(DV = "Observed Drug X (ng/mL)")
  p <- dv_pred(data) + pm_gg_labs(spec = dv_pred_spec, labs = labs_override)
  expect_equal(p$labels$y, "Observed Drug X (ng/mL)")
  # PRED was only in spec, not labs — should resolve normally
  expect_equal(p$labels$x, dv_pred_spec[["PRED"]])
})

test_that("pm_gg_labs errors on a standard ggplot (not a pmplots output)", {
  data <- pmplots_data_obs()
  p <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  expect_error(p + pm_gg_labs(dv_pred_spec), regexp = "pmplots")
})

test_that("pm_gg_labs passes extra arguments through to ggplot2::labs", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pm_gg_labs(dv_pred_spec, title = "DV vs PRED")
  expect_equal(p$labels$title, "DV vs PRED")
})

test_that("pm_gg_labs x argument overrides the mapped column for label lookup", {
  data <- pmplots_data_obs()
  # npde_time maps x to TIME, but we look up spec$TAFD for the x label
  p <- npde_time(data) + pm_gg_labs(spec, x = "TAFD")
  expect_equal(p$labels$x, spec[["TAFD"]])
})

test_that("pm_gg_labs x = I() uses the string literally without a spec lookup", {
  data <- pmplots_data_obs()
  p <- npde_time(data) + pm_gg_labs(spec, x = I("Literal title"))
  expect_equal(p$labels$x, "Literal title")
})

# pm_relabel -------------------------------------------------------------------

test_that("pm_relabel relabels a single pmplot", {
  data <- pmplots_data_obs()
  p <- pm_relabel(dv_pred(data), dv_pred_spec)
  expect_equal(p$labels$x, dv_pred_spec[["PRED"]])
  expect_equal(p$labels$y, dv_pred_spec[["DV"]])
})

test_that("pm_relabel errors on a non-pmplot gg object", {
  data <- pmplots_data_obs()
  p <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  expect_error(pm_relabel(p, dv_pred_spec))
})

test_that("pm_relabel applies spec to every plot in a list", {
  data <- pmplots_data_obs()
  plots <- pm_relabel(dv_preds(data), dv_pred_spec)
  expect_true(is.list(plots))
  expect_length(plots, 2)
  for(p in plots) {
    expect_equal(p$labels$y, dv_pred_spec[["DV"]])
  }
})
