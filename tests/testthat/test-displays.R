library(testthat)
library(pmplots)
local_edition(3)

data <- pmplots_data_obs()
id <- pmplots_data_id()

covs <- c("AAG", "WT//Weight", "CPc//Child-Pugh", "STUDYc")
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

etas1 <- pmplots:::col_label_col(etas)
covs1 <- pmplots:::col_label_col(covs)

cats <- c("CPc//Child-Pugh", "STUDYc//Study", "RF//Renal function")
cont <- c("AAG", "WT//Weight (kg)", "AGE//Age (years)", "CRCL")
cats1 <- pmplots:::col_label_col(cats)
cont1 <- pmplots:::col_label_col(cont)


test_that("eta_covariate", {
  a <- eta_covariate(data, covs, etas)
  expect_length(a, length(etas))
  expect_length(a[[1]], length(covs))
  expect_named(a)
  expect_identical(names(a), etas1)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
})

test_that("cont_cat_panel", {
  a <- cont_cat_panel(data, x = cats, y = cont)
  expect_length(a, length(cont))
  expect_length(a[[1]], length(cats))
  expect_named(a)
  expect_identical(names(a), cont1)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
})

test_that("npde_covariate", {
  a <- npde_covariate(data, covs)
  expect_length(a, length(covs))
  expect_s3_class(a, "patchwork")
})

test_that("cwres_covariate", {
  a <- cwres_covariate(data, covs)
  expect_length(a, length(covs))
  expect_s3_class(a, "patchwork")
})

test_that("eta_covariate_list", {
  a <- eta_covariate_list(data, covs, etas)
  expect_length(a, length(etas))
  expect_length(a[[1]], length(covs))
  expect_named(a)
  expect_named(a[[1]])
  expect_identical(names(a), etas1)
  expect_identical(names(a[[1]]), covs1)
  expect_identical(names(a[[1]]), names(a[[2]]))
  expect_type(a, "list")
  expect_s3_class(a[[1]], "pm_display")
  expect_s3_class(a[[1]][[1]], "gg")
})

test_that("cont_cat_panel_list", {
  a <- cont_cat_panel_list(data, cats, cont)
  expect_length(a, length(cont))
  expect_length(a[[1]], length(cats))
  expect_named(a)
  expect_named(a[[1]])
  expect_identical(names(a), cont1)
  expect_identical(names(a[[1]]), cats1)
  expect_identical(names(a[[1]]), names(a[[2]]))
  expect_type(a, "list")
  expect_s3_class(a[[1]], "pm_display")
  expect_s3_class(a[[1]][[1]], "gg")
})

test_that("npde_covariate_list", {
  a <- npde_covariate_list(data, covs)
  expect_length(a, length(covs))
  expect_named(a)
  expect_identical(names(a), covs1)
  expect_s3_class(a, "pm_display")
  expect_s3_class(a[[1]], "gg")
})

test_that("cwres_covariate_list", {
  a <- cwres_covariate_list(data, covs)
  expect_length(a, length(covs))
  expect_named(a)
  expect_identical(names(a), covs1)
  expect_s3_class(a, "pm_display")
  expect_s3_class(a[[1]], "gg")
})

# ---------------------------------------------------------------------------

test_that("eta_covariate transpose", {
  a <- eta_covariate(data, covs, etas, transpose = TRUE)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
  expect_identical(names(a), covs1)
})

test_that("cont_cat_panel transpose", {
  a <- cont_cat_panel(data, cats, cont, transpose = TRUE)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
  expect_identical(names(a), cats1)
})

test_that("eta_covariate_list transpose", {
  a <- eta_covariate_list(data, covs, etas, transpose = TRUE)
  expect_equal(names(a), covs1)
  expect_equal(names(a[[1]]), etas1)

  p <- with(a$WT, ETA1/ETA2/ETA3)
  expect_s3_class(p, "patchwork")
})

test_that("cont_cat_panel_list transpose", {
  a <- cont_cat_panel_list(data, cats, cont, transpose = TRUE)
  expect_equal(names(a), cats1)
  expect_equal(names(a[[1]]), cont1)

  p <- with(a$RF, (WT/CRCL) | AGE)
  expect_s3_class(p, "patchwork")
})

# ---------------------------------------------------------------------------

test_that("npde_panel", {
  a <- npde_panel(data)
  expect_s3_class(a, "patchwork")
})

test_that("npde_panel customized", {
  a <- npde_panel(
    data,
    xby_time = 120,
    xby_tad = 2,
    unit_time = "minutes",
    unit_tad = "years",
    xname = "unit test"
  )
  expect_s3_class(a, "patchwork")
})

test_that("cwres_panel", {
  a <- cwres_panel(data)
  expect_s3_class(a, "patchwork")
})

test_that("cwres_panel customized", {
  a <- cwres_panel(
    data,
    xby_time = 120,
    xby_tad = 2,
    unit_time = "minutes",
    unit_tad = "years",
    xname = "unit test"
  )
  expect_s3_class(a, "patchwork")
})

test_that("npde_hist_q", {
  a <- npde_hist_q(data)
  expect_s3_class(a, "patchwork")
})

test_that("cwres_hist_q", {
  a <- cwres_hist_q(data)
  expect_s3_class(a, "patchwork")
})

test_that("npde_scatter", {
  a <- npde_scatter(data)
  expect_length(a, 3)
  expect_s3_class(a, "patchwork")
})

test_that("npde_scatter compact", {
  a <- npde_scatter(data, compact = TRUE)
  expect_length(a, 2)
  expect_s3_class(a, "patchwork")
})

test_that("npde_scatter customized", {
  a <- npde_scatter(
    data,
    xby_time = 24,
    xby_tad = 4,
    unit_time = "seconds",
    unit_tad = "moments",
    xname = "something"
  )
  expect_s3_class(a, "patchwork")
})

test_that("cwres_scatter", {
  a <- cwres_scatter(data)
  expect_length(a, 3)
  expect_s3_class(a, "patchwork")
})

test_that("cwres_scatter customized", {
  a <- cwres_scatter(
    data,
    xby_time = 24,
    xby_tad = 4,
    unit_time = "seconds",
    unit_tad = "moments",
    xname = "something"
  )
  expect_s3_class(a, "patchwork")
})

test_that("cwres_scatter compact", {
  a <- cwres_scatter(data, compact = TRUE)
  expect_length(a, 2)
  expect_s3_class(a, "patchwork")
})

test_that("npde_panel_list", {
  a <- npde_panel_list(data)
  expect_s3_class(a, "pm_display")
  expect_identical(names(a), c("time", "tad", "hist", "q", "pred"))

  p <- with(a, (tad+q)/(time|tad))
  expect_s3_class(p, "patchwork")
})

test_that("cwres_panel_list", {
  a <- cwres_panel_list(data)
  expect_s3_class(a, "pm_display")
  expect_identical(names(a), c("time", "tad", "hist", "q", "pred"))

  p <- with(a, (tad+q)/(time|tad))
  expect_s3_class(p, "patchwork")
})

# ---------------------------------------------------------------------------

test_that("covariate plots - arrange by column", {
  a <- eta_covariate(data, covs[1], etas[1:2], byrow = FALSE)
  expect_false(a$ETA1$patches$layout$byrow)

  a <- cont_cat_panel(data, cats, cont, byrow = FALSE)
  expect_false(a$WT$patches$layout$byrow)

  a <- npde_covariate(data, covs, byrow = FALSE)
  expect_false(a$patches$layout$byrow)

  a <- cwres_covariate(data, covs, byrow = FALSE)
  expect_false(a$patches$layout$byrow)
})

test_that("covariate plots - set ncol", {
  a <- eta_covariate(data, covs, etas, ncol = 3)
  expect_equal(a$ETA1$patches$layout$ncol, 3)

  a <- cont_cat_panel(data, cats, cont, ncol = 2)
  expect_equal(a$WT$patches$layout$ncol, 2)

  a <- npde_covariate(data, covs, ncol = 2)
  expect_equal(a$patches$layout$ncol, 2)

  a <- cwres_covariate(data, covs, ncol = 1)
  expect_equal(a$patches$layout$ncol, 1)
})

# ---------------------------------------------------------------------------

test_that("tag levels via function", {

  a <- eta_covariate(data, covs, etas, tag_levels = "A")
  expect_equal(a$ETA1$patches$annotation$tag_levels, "A")

  a <- cont_cat_panel(data, cats, cont, tag_levels = "A")
  expect_equal(a$WT$patches$annotation$tag_levels, "A")

  a <- npde_covariate(data, covs, tag_levels = "1")
  expect_equal(a$patches$annotation$tag_levels, "1")

  a <- cwres_covariate(data, covs, tag_levels = "a")
  expect_equal(a$patches$annotation$tag_levels, "a")

  a <- npde_panel(data, tag_levels = "i")
  expect_equal(a$patches$annotation$tag_levels, "i")

  a <- cwres_panel(data, tag_levels = "I")
  expect_equal(a$patches$annotation$tag_levels, "I")

  a <- npde_hist_q(data, tag_levels = "A")
  expect_equal(a$patches$annotation$tag_levels, "A")

  a <- cwres_hist_q(data, tag_levels = "A")
  expect_equal(a$patches$annotation$tag_levels, "A")

  a <- npde_scatter(data, tag_levels = "A")
  expect_equal(a$patches$annotation$tag_levels, "A")

  a <- cwres_scatter(data, tag_levels = "A")
  expect_equal(a$patches$annotation$tag_levels, "A")
})

test_that("tag levels via with()", {

  a <- eta_covariate_list(data, covs, etas)
  p <- with(a$ETA1, WT, tag_levels = "A")
  expect_equal(p$patches$annotation$tag_levels, "A")

  a <- cont_cat_panel_list(data, cats, cont)
  p <- with(a$WT, (RF + CPc)/STUDYc, tag_levels = "A")
  expect_equal(p$patches$annotation$tag_levels, "A")

  a <- npde_covariate_list(data, covs)
  p <- with(a, WT + AAG, tag_levels = "a")
  expect_equal(p$patches$annotation$tag_levels, "a")

  a <- cwres_covariate_list(data, covs)
  p <- with(a, WT + AAG, tag_levels = "1")
  expect_equal(p$patches$annotation$tag_levels, "1")

  a <- npde_panel_list(data)
  p <- with(a, time + pred, tag_levels = "i")
  expect_equal(p$patches$annotation$tag_levels, "i")

  a <- cwres_panel_list(data)
  p <- with(a, time + pred, tag_levels = "A")
  expect_equal(p$patches$annotation$tag_levels, "A")
})

# ---------------------------------------------------------------------------


