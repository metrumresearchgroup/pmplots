library(testthat)
library(patchwork)

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
  x <- p + rot_x(angle = 33)
  expect_is(x, "gg")
  expect_equal(x$theme$axis.text.x$angle, 33)

  y <- p + rot_y(angle = 11)
  expect_equal(y$theme$axis.text.y$angle, 11)

  x <- p + rot_x(vertical = TRUE)
  expect_equal(x$theme$axis.text.x$angle, 90)
  expect_equal(x$theme$axis.text.x$hjust, 1)

  y <- p + rot_y(vertical = TRUE)
  expect_equal(y$theme$axis.text.y$angle, 90)
  expect_equal(y$theme$axis.text.y$vjust, 1)

  x1 <- p + rot_x(vertical = TRUE, hjust = "bottom")
  expect_equal(x1$theme$axis.text.x$angle, 90)
  expect_equal(x1$theme$axis.text.x$hjust, 0)

  x2 <- p + rot_x(vertical = TRUE, hjust = "top")
  expect_equal(x2$theme$axis.text.x$angle, 90)
  expect_equal(x2$theme$axis.text.x$hjust, 1)

  y1 <- p + rot_y(vertical = TRUE, vjust = "left")
  expect_equal(y1$theme$axis.text.y$angle, 90)
  expect_equal(y1$theme$axis.text.y$vjust, 1)

  y2 <- p + rot_y(vertical = TRUE, vjust = "right")
  expect_equal(y2$theme$axis.text.y$angle, 90)
  expect_equal(y2$theme$axis.text.y$vjust, 0)
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

  x <- rot_at(x0, at = c("ETA2vSTUDYc", "ETA1vRF"), angle = 11)
  expect_is(x, "list")
  expect_equal(x$ETA2vSTUDYc$theme$axis.text.x$angle, 11)
  expect_equal(x$ETA1vRF$theme$axis.text.x$angle, 11)

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

test_that("rot_xy", {
  g1 <- dv_pred(data)
  g2 <- dv_ipred(data)
  p1 <- g1 + g2
  p2 <- g1/g2
  l <- list(a = g1, b = g2)
  lp <- list(a = p1, b = p2)

  nuke_env <- function(xs) {
    for (i in seq_along(xs)) {
      if (identical(names(xs[i]), "plot_env")) {
        xs[[i]] <- "CLOBBERED"
      } else if (is.list(xs[[i]])) {
        xs[[i]] <- nuke_env(xs[[i]])
      }
    }
    return(xs)
  }

  # Check if these are the same, ignoring plot_env
  # rotate gg
  a <- rot_xy(g1)
  b <- g1 + rot_x()
  expect_equal(nuke_env(a), nuke_env(b))

  # rotate patchwork
  a <- rot_xy(p1)
  b <- p1 & rot_x()
  expect_equal(nuke_env(a), nuke_env(b))

  # list of gg
  a <- lapply(l, function(xx) xx + rot_x())
  expect_is(a, "list")
  b <- rot_xy(l)
  expect_equal(nuke_env(a), nuke_env(b))
  expect_error(rot_xy(unname(l)), "must be named")

  # list of patchwork
  a <- lapply(lp, function(xx) xx & rot_x())
  expect_is(a, "list")
  b <- rot_xy(lp)
  expect_equal(nuke_env(a), nuke_env(b))

  # Arguments are passed through
  a <- rot_xy(g1, angle = 89)
  expect_equal(a$theme$axis.text.x$angle, 89)

  a <- rot_xy(g2, angle = 98, axis = "y")
  expect_equal(a$theme$axis.text.y$angle, 98)
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

test_that("pm_with arranges a list of plots", {
  data <- pmplots_data_id()
  x <- c("STUDYc", "CPc", "RF")
  y <- paste0("ETA", 1:3)
  p <- eta_cat(data, x, y[1])
  a <- pm_with(p, STUDYc+CPc+RF)
  expect_is(a, "patchwork")
  expect_length(a, 3)

  p <- eta_covariate_list(data, x, y)
  a <- pm_with(p$ETA1, (STUDYc + CPc)/RF)
  expect_is(a, "patchwork")
  expect_length(a, 2)
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
