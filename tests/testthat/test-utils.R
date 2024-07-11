library(testthat)

context("test-utils")
data <- pmplots_data_obs()
id <- pmplots_data_id()
p <- dv_time(data)

test_that("CWRESI gets subbed for CWRES [PMP-TEST-062]", {
  data$CWRES <- NULL
  expect_is(cwres_time(data), "gg")
  expect_message(cwres_time(data),"Creating CWRES column from CWRESI")
})

test_that("log scale [PMP-TEST-063]", {
  x <- log_scale()
  expect_is(x, "list")
  expect_equal(x$transform, "log10")
  expect_equal(x$breaks, NULL)
  x <- log_scale(logbr())
  expect_equal(x$breaks, 10^seq(-10,10))
})

test_that("rot_x and rot_y [PMP-TEST-064]", {
  expect_is(p + rot_x(), "gg")
  expect_is(p + rot_y(), "gg")
})

test_that("rot_at - rotate list of plots", {
  etas <- paste0("ETA", 1:3)
  co <- c("STUDYc", "CPc", "RF")

  x0 <- eta_cat(id, x = co, y = etas)

  expect_identical(
    names(x0)[1:3],
    c("ETA1vSTUDYc", "ETA2vSTUDYc", "ETA3vSTUDYc")
  )

  x <- rot_at(x0, at = "ETA2vSTUDYc", angle = 11)
  expect_is(x, "list")
  expect_is(x[[1]], "gg")
  expect_equal(x$ETA2vSTUDYc$theme$axis.text.x$angle, 11)
  expect_null(x$ETA1vSTUDYc$theme$axis.text.x$angle)

  x <- rot_at(x0, re = "ETA2", angle = 33)
  expect_is(x, "list")
  expect_is(x[[1]], "gg")
  expect_equal(x$ETA2vSTUDYc$theme$axis.text.x$angle, 33)
  expect_equal(x$ETA2vCPc$theme$axis.text.x$angle, 33)
  expect_equal(x$ETA2vRF$theme$axis.text.x$angle, 33)
  expect_null(x$ETA1vSTUDYc$theme$axis.text.x$angle)
  expect_null(x$ETA3vCPc$theme$axis.text.x$angle)

  expect_error(rot_at(x0, at = "kyle"), "requested names not found")
  expect_warning(rot_at(x0, re = "banana"), "did not find any plots")
  expect_error(rot_at(x0[[1]]), "must be a list of gg")
  expect_error(rot_at(unname(x0)), "must be named")
})

test_that("def [PMP-TEST-065]", {
  x <- defx(breaks = c(1,2,3))
  expect_equal(x$breaks, c(1,2,3))
  expect_equal(x$position, "bottom")
  x <- defy(breaks = c(1,2,3))
  expect_equal(x$breaks, c(1,2,3))
  expect_equal(x$position, "left")
  x <- defcx()
  expect_equal(x$position, "bottom")
})

test_that("logbr [PMP-TEST-066]", {
  x <- logbr()
  expect_equal(x, 10^seq(-10,10))
  y <- logbr3()
  expect_equal(y, sort(c(x,3*x)))

})

test_that("char [PMP-TEST-067]", {
  expect_true(pmplots:::charthere("kyle", "k"))
  expect_equal(pmplots:::charcount("mississippi", "i"),4)
})

test_that("search col name [PMP-TEST-068]", {
  data$CWRES <- NULL
  a <- pmplots:::search_cwres_i("CWRES", data)
  expect_equal(a,"CWRESI")
})

test_that("args are passed to rot_x and rot_y [PMP-TEST-069]", {
  p <- dv_time(data) + rot_x(hjust = 0, vjust = 0)
  expect_is(p, "gg")
  p <- dv_time(data) + rot_y(hjust = 0, vjust = 0)
  expect_is(p, "gg")
  p <- dv_time(data) + rot_x(vert = TRUE)
  expect_is(p, "gg")
})

test_that("trans argument is mapped to transform", {
  fns <- list(
    pm_log,
    defx,
    defy
  )
  for (fn in fns) {
    lifecycle::expect_deprecated(
      res <- fn(trans = "log2")
    )

    expect_identical(res[["transform"]], "log2")
    expect_false("trans" %in% names(res))
    identical(res, fn(transform = "log2"))
  }
})

test_that("remap_trans_arg() converts trans to transform", {
  # expect_deprecated() configures an option that should ensure the warning is
  # always emitted, but for some reason only one warning is given if
  # remap_trans_arg() is called with its default user_env value.
  env <- rlang::current_env()

  expect_identical(
    lifecycle::expect_deprecated(
      remap_trans_arg(list(trans = "log2"), user_env = env)
    ),
    list(transform = "log2")
  )

  expect_identical(
    lifecycle::expect_deprecated(
      remap_trans_arg(list(trans = "log2", transform = "log10"), user_env = env)
    ),
    list(transform = "log2")
  )

  # If both are specified, `trans` is in effect, following ggplot2 precedence.
  expect_identical(
    remap_trans_arg(list(transform = "log2")),
    list(transform = "log2")
  )
})
