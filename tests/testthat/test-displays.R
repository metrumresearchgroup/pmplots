library(testthat)

local_edition(3)

data <- pmplots_data_obs()
id <- pmplots_data_id()

covs <- c("AAG", "WT//Weight", "CPc//Child-Pugh", "STUDYc")
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

etas1 <- lapply(etas, col_label) %>% sapply("[",1)
covs1 <- lapply(covs, col_label) %>% sapply("[", 1)

test_that("eta_covariate", {
  a <- eta_covariate(data, covs, etas)
  expect_length(a, length(etas))
  expect_named(a)
  expect_identical(names(a), etas1)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")

  a <- eta_covariate(data, covs, etas, ncol = 3)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
})

test_that("eta_covariate transpose", {
  a <- eta_covariate(data, covs, etas, transpose = TRUE)
  expect_type(a, "list")
  expect_s3_class(a[[1]], "patchwork")
  expect_identical(names(a), covs1)
})

test_that("npde_covariate", {
  a <- npde_covariate(data, covs)
  expect_s3_class(a, "patchwork")
})

test_that("cwres_covariate", {
  a <- cwres_covariate(data, covs)
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

  p <- with(a$ETA1, WT/AAG, tag_levels = 1)
  expect_s3_class(p, "gg")
})

test_that("eta_covariate_list transpose", {
  a <- eta_covariate_list(data, covs, etas, transpose = TRUE)
  expect_equal(names(a), covs1)
  expect_equal(names(a[[1]]), etas1)

  p <- with(a$WT, ETA1/ETA2/ETA3, tag_levels = "a")
  expect_s3_class(p, "patchwork")
})

test_that("eta_covariate by column ", {
  a <- eta_covariate(data, covs, etas, byrow = FALSE)
  b <- eta_covariate(data, covs, etas, byrow = TRUE)
  expect_false(identical(a,b))
  expect_equal(names(a), etas1)
})

test_that("npde_covariate_list", {
  a <- npde_covariate_list(data, covs)
  expect_length(a, length(covs))
  expect_named(a)
  expect_identical(names(a), covs1)
  expect_s3_class(a, "pm_display")
  expect_s3_class(a[[1]], "gg")

  p <- with(a, WT/AAG, tag_levels = 1)
  expect_s3_class(p, "gg")
})

test_that("cwres_covariate_list", {
  a <- cwres_covariate_list(data, covs)
  expect_length(a, length(covs))
  expect_named(a)
  expect_identical(names(a), covs1)
  expect_s3_class(a, "pm_display")
  expect_s3_class(a[[1]], "gg")

  p <- with(a, CPc+STUDYc, tag_levels = 1)
  expect_s3_class(p, "gg")
})

test_that("npde_panel", {
  a <- npde_panel(data, tag_levels = "i")
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
  a <- cwres_panel(data, tag_levels = "i")
  expect_s3_class(a, "patchwork")
})

test_that("npde_hist_q", {
  a <- npde_hist_q(data, tag_levels = "i")
  expect_s3_class(a, "patchwork")
})

test_that("cwres_hist_q", {
  a <- cwres_hist_q(data, tag_levels = "i")
  expect_s3_class(a, "patchwork")
})

test_that("npde_scatter", {
  a <- npde_scatter(data, tag_levels = "i")
  expect_s3_class(a, "patchwork")
})

test_that("npde_scatter compact", {
  a <- npde_scatter(data, compact = TRUE)
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
  a <- cwres_scatter(data, tag_levels = "i")
  expect_s3_class(a, "patchwork")
})

test_that("cwres_scatter compact", {
  a <- cwres_scatter(data, compact = TRUE)
  expect_s3_class(a, "patchwork")
})

test_that("npde_panel_list", {
  a <- npde_panel_list(data)
  expect_s3_class(a, "pm_display")
  expect_identical(names(a), c("time", "tad", "hist", "q", "pred"))

  p <- with(a, (tad+q)/(time|tad), tag_levels = 'a')
  expect_s3_class(p, "patchwork")
})

test_that("cwres_panel_list", {
  a <- cwres_panel_list(data)
  expect_s3_class(a, "pm_display")
  expect_identical(names(a), c("time", "tad", "hist", "q", "pred"))

  p <- with(a, (tad+q)/(time|tad), tag_levels = 'a')
  expect_s3_class(p, "patchwork")
})
