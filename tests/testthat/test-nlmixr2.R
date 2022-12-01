test_that("test that nlmixr2 data loads info correctly", {
  
  expect_true(inherits(nlmixr2extra::theoFitOde, "nlmixr2FitData"))
  fl <- pmplots_nlmixr2_data(nlmixr2extra::theoFitOde, "left")
  fr <- pmplots_nlmixr2_data(nlmixr2extra::theoFitOde, "right")
  fi <- pmplots_nlmixr2_data(nlmixr2extra::theoFitOde, "inner")

  expect_false(any(names(nlmixr2extra::theoFitOde)=="WT"))
  expect_true(any(names(fl)=="WT"))
  expect_true(any(names(fr)=="WT"))
  expect_true(any(names(fi)=="WT"))
})
