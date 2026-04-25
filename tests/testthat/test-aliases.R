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
