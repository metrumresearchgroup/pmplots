library(testthat)

pm_clear_aliases()

test_that("pm_show_canonical returns expected column names", {
  cols <- pm_show_canonical()
  expect_type(cols, "character")
  expect_true(all(c("TIME", "TAD", "TAFD", "DV", "PRED", "IPRED",
                    "CWRES", "CWRESI", "RES", "WRES", "NPDE") %in% cols))
})

test_that("pm_set_aliases registers aliases (unquoted and quoted)", {
  on.exit(pm_clear_aliases())
  pm_set_aliases(NOMTIME = TIME)
  expect_equal(pmplots:::substitute_alias("TIME"), "NOMTIME")
  pm_clear_aliases()
  pm_set_aliases(conc = "DV")
  expect_equal(pmplots:::substitute_alias("DV"), "conc")
})

test_that("pm_set_aliases errors on unnamed arguments", {
  on.exit(pm_clear_aliases())
  expect_error(pm_set_aliases(TIME), "all arguments must be named")
})

test_that("pm_set_aliases errors on non-canonical column names", {
  on.exit(pm_clear_aliases())
  expect_error(pm_set_aliases(foo = NOTACOL), "only certain columns can be aliased")
})

test_that("pm_clear_aliases removes all aliases", {
  pm_set_aliases(NOMTIME = TIME)
  pm_clear_aliases()
  expect_equal(pmplots:::substitute_alias("TIME"), "TIME")
})

test_that("pm_aliases prints message when no aliases are set", {
  pm_clear_aliases()
  expect_message(pm_aliases(), "no aliases were found")
})

test_that("pm_aliases prints active aliases", {
  on.exit(pm_clear_aliases())
  pm_set_aliases(NOMTIME = TIME)
  expect_message(pm_aliases(), "NOMTIME")
})

test_that("aliases flow through to pm_axis functions", {
  on.exit(pm_clear_aliases())
  pm_set_aliases(NOMTIME = TIME)
  expect_match(pm_axis_time(), "^NOMTIME//")
  pm_clear_aliases()
  expect_match(pm_axis_time(), "^TIME//")
})

test_that("substitute_alias is a no-op for unregistered columns", {
  pm_clear_aliases()
  expect_equal(pmplots:::substitute_alias("DV"), "DV")
})

test_that("TAFD = TIME alias redirects pm_axis_time (alias name is itself canonical)", {
  on.exit(pm_clear_aliases())
  pm_set_aliases(TAFD = TIME)
  expect_match(pm_axis_time(), "^TAFD//")
  # pm_axis_tafd is unaffected; its own canonical (TAFD) has no alias
  expect_match(pm_axis_tafd(), "^TAFD//")
})

test_that("alias is not applied when column name is passed directly to a plot function", {
  on.exit(pm_clear_aliases())
  data <- pmplots_data_obs()
  pm_set_aliases(TAFD = TIME)
  # default argument pm_axis_time() applies the alias
  expect_equal(rlang::as_label(dv_time(data)$mapping$x), "TAFD")
  # explicit x = "TIME" bypasses pm_axis_time() and skips alias substitution
  expect_equal(rlang::as_label(dv_time(data, x = "TIME")$mapping$x), "TIME")
})

test_that("alias TAD when asking for column name", {
  pm_clear_aliases()
  expect_identical(pm_col_tad(), "TAD")
  pm_set_aliases(TALD = TAD)
  expect_identical(pm_col_tad(), "TALD")
  pm_clear_aliases()
})

test_that("alias ID when asking for column name", {
  expect_identical(pm_col_id(), "ID")
  withr::with_options(
    list(mrg.id_col = "USUBJID"),
    expect_identical(pm_col_id(), "USUBJID")
  )
})
