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

# validate_label_list (exercised via pmp_gg_labs and pm_label_columns) ----------

test_that("pmp_gg_labs errors when spec is not a list", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pmp_gg_labs(spec = c(DV = "Concentration (ng/mL)")), regexp = "spec")
})

test_that("pmp_gg_labs errors when spec is an unnamed list", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pmp_gg_labs(spec = list("Concentration (ng/mL)")), regexp = "spec")
})

test_that("pmp_gg_labs errors when a spec value is not a length-1 character", {
  data <- pmplots_data_obs()
  expect_error(dv_pred(data) + pmp_gg_labs(spec = list(DV = c("a", "b"))), regexp = "spec")
  expect_error(dv_pred(data) + pmp_gg_labs(spec = list(DV = 1L)), regexp = "spec")
})

test_that("pmp_gg_labs errors when labs fails validation", {
  data <- pmplots_data_obs()
  expect_error(
    dv_pred(data) + pmp_gg_labs(spec = dv_pred_spec, labs = list(DV = c("a", "b"))),
    regexp = "labs"
  )
})

test_that("pm_label_columns errors when spec values are not length-1 characters", {
  data <- pmplots_data_obs()
  expect_error(pm_label_columns(data, spec = list(DV = c("a", "b"))), regexp = "spec")
})

# pmp_gg_labs -------------------------------------------------------------------

test_that("pmp_gg_labs sets x and y axis labels from spec", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pmp_gg_labs(dv_pred_spec)
  expect_equal(p$labels$x, "Population prediction (ng/mL)")
  expect_equal(p$labels$y, "Concentration (ng/mL)")
})

test_that("pmp_gg_labs leaves labels unchanged for columns not in spec", {
  data <- pmplots_data_obs()
  p_base <- dv_pred(data)
  p_labeled <- p_base + pmp_gg_labs(list(DV = "Concentration (ng/mL)"))
  # y (DV) should update; x (PRED) should stay as the default
  expect_equal(p_labeled$labels$y, "Concentration (ng/mL)")
  expect_equal(p_labeled$labels$x, p_base$labels$x)
})

test_that("pmp_gg_labs labs overrides spec for the same column", {
  data <- pmplots_data_obs()
  labs_override <- list(DV = "Observed Drug X (ng/mL)")
  p <- dv_pred(data) + pmp_gg_labs(spec = dv_pred_spec, labs = labs_override)
  expect_equal(p$labels$y, "Observed Drug X (ng/mL)")
  # PRED was only in spec, not labs — should resolve normally
  expect_equal(p$labels$x, dv_pred_spec[["PRED"]])
})

test_that("pmp_gg_labs errors on a standard ggplot (not a pmplots output)", {
  data <- pmplots_data_obs()
  p <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  expect_error(p + pmp_gg_labs(dv_pred_spec), regexp = "pmplots")
})

test_that("pmp_gg_labs passes extra arguments through to ggplot2::labs", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pmp_gg_labs(dv_pred_spec, title = "DV vs PRED")
  expect_equal(p$labels$title, "DV vs PRED")
})

test_that("pmp_gg_labs x argument overrides the mapped column for label lookup", {
  data <- pmplots_data_obs()
  # npde_time maps x to TIME, but we look up spec$TAFD for the x label
  p <- npde_time(data) + pmp_gg_labs(spec, x = "TAFD")
  expect_equal(p$labels$x, spec[["TAFD"]])
})

test_that("pmp_gg_labs x = I() uses the string literally without a spec lookup", {
  data <- pmplots_data_obs()
  p <- npde_time(data) + pmp_gg_labs(spec, x = I("Literal title"))
  expect_equal(p$labels$x, "Literal title")
})

test_that("pmp_gg_labs x_break inserts newline in x label at word boundary", {
  data <- pmplots_data_obs()
  # "Population prediction (ng/mL)" — spaces at 11 and 22; x_break 15 picks 11
  p <- dv_pred(data) + pmp_gg_labs(dv_pred_spec, x_break = 15)
  expect_equal(p$labels$x, "Population\nprediction (ng/mL)")
})

test_that("pmp_gg_labs y_break inserts newline in y label at word boundary", {
  data <- pmplots_data_obs()
  # "Concentration (ng/mL)" — space at 14; y_break 15 picks 14
  p <- dv_pred(data) + pmp_gg_labs(dv_pred_spec, y_break = 15)
  expect_equal(p$labels$y, "Concentration\n(ng/mL)")
})

test_that("pmp_gg_labs x_break = Inf (default) does not insert newlines", {
  data <- pmplots_data_obs()
  p <- dv_pred(data) + pmp_gg_labs(dv_pred_spec)
  expect_equal(p$labels$x, dv_pred_spec[["PRED"]])
  expect_equal(p$labels$y, dv_pred_spec[["DV"]])
})

# pmp_relabel -------------------------------------------------------------------

test_that("pmp_relabel relabels a single pmplot", {
  data <- pmplots_data_obs()
  p <- pmp_relabel(dv_pred(data), dv_pred_spec)
  expect_equal(p$labels$x, dv_pred_spec[["PRED"]])
  expect_equal(p$labels$y, dv_pred_spec[["DV"]])
})

test_that("pmp_relabel errors on a non-pmplot gg object", {
  data <- pmplots_data_obs()
  p <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  expect_error(pmp_relabel(p, dv_pred_spec))
})

test_that("pmp_relabel applies spec to every plot in a list", {
  data <- pmplots_data_obs()
  plots <- pmp_relabel(dv_preds(data), dv_pred_spec)
  expect_true(is.list(plots))
  expect_length(plots, 2)
  for(p in plots) {
    expect_equal(p$labels$y, dv_pred_spec[["DV"]])
  }
})

# pm_save_xy / pm_get_data_x / pm_get_data_y ----------------------------------

test_that("pm_label_columns labels flow into pmp.data.axis.x and pmp.data.axis.y", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, dv_pred_spec)
  p <- dv_pred(labeled)
  expect_equal(pmplots:::pm_get_data_x(p), dv_pred_spec[["PRED"]])
  expect_equal(pmplots:::pm_get_data_y(p), dv_pred_spec[["DV"]])
})

test_that("pmp.data.axis labels are NULL when data has no pmp.axis.label attributes", {
  data <- pmplots_data_obs()
  p <- dv_pred(data)
  expect_null(pmplots:::pm_get_data_x(p))
  expect_null(pmplots:::pm_get_data_y(p))
})

test_that("pm_label_rm clears labels so they no longer flow into the plot", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, dv_pred_spec)
  cleaned <- pm_label_rm(labeled)
  p <- dv_pred(cleaned)
  expect_null(pmplots:::pm_get_data_x(p))
  expect_null(pmplots:::pm_get_data_y(p))
})

test_that("pm_get_data_x and pm_get_data_y error on a non-pmplot", {
  data <- pmplots_data_obs()
  p <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  expect_error(pmplots:::pm_get_data_x(p), regexp = "pmplot")
  expect_error(pmplots:::pm_get_data_y(p), regexp = "pmplot")
})

# Labeling via pm_label_columns flows into npde_hist, npde_q, npde_covariate --

npde_spec <- list(NPDE = "Normalized prediction distribution error", WT = "Weight (kg)")

test_that("npde_hist: data labels flow into pmp.data.axis.x", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, npde_spec)
  p <- npde_hist(labeled)
  expect_equal(pmplots:::pm_get_data_x(p), npde_spec[["NPDE"]])
})

test_that("npde_q: data labels flow into pmp.data.axis.x", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, npde_spec)
  p <- npde_q(labeled)
  expect_equal(pmplots:::pm_get_data_x(p), npde_spec[["NPDE"]])
})

test_that("npde_covariate_list: data labels flow into pmp.data.axis.y", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, npde_spec)
  plots <- npde_covariate_list(labeled, x = "WT//Weight (kg)")
  expect_equal(pmplots:::pm_get_data_y(plots[[1]]), npde_spec[["NPDE"]])
})

test_that("pmp_gg_labs overrides x label in npde_q", {
  data <- pmplots_data_obs()
  p <- npde_q(data) + pmp_gg_labs(npde_spec)
  expect_equal(p$labels$x, npde_spec[["NPDE"]])
})

test_that("pmp_gg_labs overrides x label in npde_hist", {
  data <- pmplots_data_obs()
  p <- npde_hist(data) + pmp_gg_labs(npde_spec)
  expect_equal(p$labels$x, npde_spec[["NPDE"]])
})

# pmp_relabel_wrap --------------------------------------------------------------

wrap_spec <- list(WT = "Weight (kg)", ALB = "Albumin (mg/dL)")

test_that("pmp_relabel_wrap applies spec labels to facet strips", {
  data <- pmplots_data_obs()
  p <- wrap_eta_cont(data, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
  p2 <- pmp_relabel_wrap(p, wrap_spec)
  lbl <- ggplot2::ggplot_build(p2)$layout$facet_params$labeller
  mapped <- lbl(data.frame(variable = factor(c("WT", "ALB"))))
  expect_equal(unlist(mapped$variable), c("Weight (kg)", "Albumin (mg/dL)"))
})

test_that("pmp_relabel_wrap leaves unlabeled variables unchanged", {
  data <- pmplots_data_obs()
  p <- wrap_eta_cont(data, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
  p2 <- pmp_relabel_wrap(p, list(WT = "Weight (kg)"))
  lbl <- ggplot2::ggplot_build(p2)$layout$facet_params$labeller
  mapped <- lbl(data.frame(variable = factor(c("WT", "ALB"))))
  expect_equal(unlist(mapped$variable), c("Weight (kg)", "ALB"))
})

test_that("pmp_relabel_wrap labs overrides spec", {
  data <- pmplots_data_obs()
  p <- wrap_eta_cont(data, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
  p2 <- pmp_relabel_wrap(p, wrap_spec, labs = list(WT = "Body weight (kg)"))
  lbl <- ggplot2::ggplot_build(p2)$layout$facet_params$labeller
  mapped <- lbl(data.frame(variable = factor(c("WT", "ALB"))))
  expect_equal(unlist(mapped$variable)[1], "Body weight (kg)")
})

test_that("pmp_relabel_wrap preserves scales from original plot", {
  data <- pmplots_data_obs()
  p <- wrap_eta_cont(data, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
  p2 <- pmp_relabel_wrap(p, wrap_spec)
  expect_true(p2$facet$params$free$x)
  expect_false(p2$facet$params$free$y)
})

test_that("pmp_relabel_wrap errors when plot has no variable column", {
  data <- pmplots_data_obs()
  p <- dv_pred(data)
  expect_error(pmp_relabel_wrap(p, wrap_spec), regexp = "wrapped pmplots")
})

# pmp_relabel_pairs -------------------------------------------------------------

pairs_spec <- list(WT = "Weight (kg)", HT = "Height (cm)")

test_that("pmp_relabel_pairs relabels matched columns", {
  data <- pmplots_data_id()
  p <- pairs_plot(data, c("WT", "HT", "SCR"))
  p2 <- pmp_relabel_pairs(p, pairs_spec)
  expect_equal(p2$yAxisLabels[1], "Weight\n(kg)")
  expect_equal(p2$yAxisLabels[2], "Height\n(cm)")
  expect_equal(p2$yAxisLabels[3], "SCR")
  expect_equal(p2$xAxisLabels[1], "Weight\n(kg)")
  expect_equal(p2$xAxisLabels[2], "Height\n(cm)")
  expect_equal(p2$xAxisLabels[3], "SCR")
})

test_that("pmp_relabel_pairs with unit_break = FALSE does not insert newlines", {
  data <- pmplots_data_id()
  p <- pairs_plot(data, c("WT", "HT", "SCR"))
  p2 <- pmp_relabel_pairs(p, pairs_spec, unit_break = FALSE)
  expect_equal(p2$yAxisLabels[1], "Weight (kg)")
  expect_equal(p2$yAxisLabels[2], "Height (cm)")
  expect_equal(p2$yAxisLabels[3], "SCR")
})

test_that("pmp_relabel_pairs labs overrides spec", {
  data <- pmplots_data_id()
  p <- pairs_plot(data, c("WT", "HT", "SCR"))
  p2 <- pmp_relabel_pairs(p, pairs_spec, labs = list(WT = "Body weight (kg)"), unit_break = FALSE)
  expect_equal(p2$yAxisLabels[1], "Body weight (kg)")
  expect_equal(p2$yAxisLabels[2], "Height (cm)")
})

test_that("pmp_relabel_pairs leaves columns absent from spec unchanged", {
  data <- pmplots_data_id()
  p <- pairs_plot(data, c("WT", "HT", "SCR"))
  p2 <- pmp_relabel_pairs(p, list(WT = "Weight (kg)"), unit_break = FALSE)
  expect_equal(p2$yAxisLabels[2], "HT")
  expect_equal(p2$yAxisLabels[3], "SCR")
})

test_that("pmp_relabel_pairs errors on a non-pairs plot", {
  data <- pmplots_data_obs()
  p <- dv_pred(data)
  expect_error(pmp_relabel_pairs(p, pairs_spec), regexp = "pm pairs plots")
})

test_that("pmp_relabel_pairs: data labels from pm_label_columns flow into yAxisLabels", {
  data <- pmplots_data_id()
  labeled <- pm_label_columns(data, list(WT = "Weight (kg)"))
  p <- pairs_plot(labeled, c("WT", "HT", "SCR"))
  expect_equal(p$yAxisLabels[1], "Weight\n(kg)")
  expect_equal(p$yAxisLabels[2], "HT")
})

# col_labels_from_data ----------------------------------------------------------

test_that("col_labels_from_data returns pmp.axis.label when present", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, list(WT = "Weight (kg)", ALB = "Albumin (g/dL)"))
  result <- pmplots:::col_labels_from_data(labeled, c("WT", "ALB"))
  expect_equal(unname(result), c("Weight (kg)", "Albumin (g/dL)"))
})

test_that("col_labels_from_data falls back to column name when no attribute", {
  data <- pmplots_data_obs()
  result <- pmplots:::col_labels_from_data(data, c("WT", "SCR"))
  expect_equal(unname(result), c("WT", "SCR"))
})

test_that("col_labels_from_data uses wrap label labels in wrapped plots", {
  data <- pmplots_data_obs()
  labeled <- pm_label_columns(data, list(WT = "Weight (kg)", ALB = "Albumin (g/dL)"))
  p <- wrap_eta_cont(labeled, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
  expect_equal(levels(p$data$variable), c("Weight (kg)", "Albumin (g/dL)"))
})

# relabel_at --------------------------------------------------------------------

relabel_spec <- list(DV = "Concentration (ng/mL)", PRED = "Population prediction (ng/mL)")

test_that("relabel_at with at relabels only the named elements", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data), p2 = dv_ipred(data))
  out <- relabel_at(plots, at = "p1", spec = relabel_spec)
  expect_equal(out$p1$labels$y, relabel_spec[["DV"]])
  expect_equal(out$p1$labels$x, relabel_spec[["PRED"]])
  expect_equal(out$p2$labels$y, plots$p2$labels$y)
})

test_that("relabel_at with re relabels elements matching the pattern", {
  data <- pmplots_data_obs()
  plots <- list(dv1 = dv_pred(data), dv2 = dv_ipred(data), eta1 = npde_pred(data))
  out <- relabel_at(plots, re = "^dv", spec = relabel_spec)
  expect_equal(out$dv1$labels$y, relabel_spec[["DV"]])
  expect_equal(out$dv2$labels$y, relabel_spec[["DV"]])
  expect_equal(out$eta1$labels$y, plots$eta1$labels$y)
})

test_that("relabel_at re takes precedence over at", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data), p2 = dv_ipred(data))
  out <- relabel_at(plots, at = "p1", re = "p2", spec = relabel_spec)
  expect_equal(out$p2$labels$y, relabel_spec[["DV"]])
  expect_equal(out$p1$labels$y, plots$p1$labels$y)
})

test_that("relabel_at applies to all elements when at = names(x) (default)", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data), p2 = dv_ipred(data))
  out <- relabel_at(plots, spec = relabel_spec)
  expect_equal(out$p1$labels$y, relabel_spec[["DV"]])
  expect_equal(out$p2$labels$y, relabel_spec[["DV"]])
})

test_that("relabel_at works on a list of plain gg plots", {
  data <- pmplots_data_obs()
  gg1 <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  gg2 <- ggplot2::ggplot(data, ggplot2::aes(IPRED, DV)) + ggplot2::geom_point()
  plots <- list(a = gg1, b = gg2)
  out <- relabel_at(plots, spec = relabel_spec)
  expect_equal(out$a$labels$y, relabel_spec[["DV"]])
  expect_equal(out$a$labels$x, relabel_spec[["PRED"]])
  expect_equal(out$b$labels$y, relabel_spec[["DV"]])
})

test_that("relabel_at works on a list of pmplot outputs", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data), p2 = dv_ipred(data))
  out <- relabel_at(plots, spec = relabel_spec)
  expect_equal(out$p1$labels$y, relabel_spec[["DV"]])
  expect_equal(out$p1$labels$x, relabel_spec[["PRED"]])
  expect_equal(out$p2$labels$y, relabel_spec[["DV"]])
})

test_that("relabel_at works on a mixed list of pmplot and plain gg objects", {
  data <- pmplots_data_obs()
  pmp <- dv_pred(data)
  plain <- ggplot2::ggplot(data, ggplot2::aes(PRED, DV)) + ggplot2::geom_point()
  plots <- list(pmp = pmp, plain = plain)
  out <- relabel_at(plots, spec = relabel_spec)
  expect_equal(out$pmp$labels$y, relabel_spec[["DV"]])
  expect_equal(out$plain$labels$y, relabel_spec[["DV"]])
  expect_equal(out$plain$labels$x, relabel_spec[["PRED"]])
})

test_that("relabel_at errors when x is unnamed", {
  data <- pmplots_data_obs()
  plots <- list(dv_pred(data), dv_ipred(data))
  expect_error(relabel_at(plots, spec = relabel_spec), regexp = "named")
})

test_that("relabel_at errors when at contains names not in x", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data))
  expect_error(relabel_at(plots, at = "p_missing", spec = relabel_spec))
})

test_that("relabel_at warns and returns x unchanged when re matches nothing", {
  data <- pmplots_data_obs()
  plots <- list(p1 = dv_pred(data))
  expect_warning(
    out <- relabel_at(plots, re = "^zzz", spec = relabel_spec),
    regexp = "relabel"
  )
  expect_equal(out$p1$labels$y, plots$p1$labels$y)
})
