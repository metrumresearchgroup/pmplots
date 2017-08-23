Sys.setenv("R_TESTS" = "")
library(testthat)
library(pmplots)

test_check("pmplots", reporter="summary")

