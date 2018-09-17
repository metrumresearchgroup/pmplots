library(testthat)

context("test-required")

df <- dplyr::filter(pmplots_data(),EVID==0)

test_that("Functions fail when col doesn't exist", {

  df$CWRES <- NULL
  df$IPRED <- NULL
  expect_error(cwres_time(df),regexp = "CWRES is required in the data set")

  expect_error(dv_ipred(df), regexp = "IPRED is required in the data set")

  expect_error(cont_cont(df, x="WT//Weight", y="Metrum//RG"),
               regexp="Metrum is required in the data set")

  expect_is(dv_ipred(df, x="PRED//PRED"),"gg")

})


test_that("Column type is correct", {

  expect_error(cont_cat(df, x="WT//Weight", y="AST//AST"),
               "is required to be character")

  expect_error(cwresi_cat(df, x="WT//Weight"),
               "is required to be character")

  df$PRED <- as.character(df$PRED)
  expect_error(dv_pred(df),
               "is required to be numeric")

  df$TIME <- as.character(df$TIME)
  expect_error(cwres_time(df),
               "is required to be numeric")

  expect_error(cont_cat(df, x="PRED//PRED", y="TIME//TIME"),
               "is required to be numeric")

})

