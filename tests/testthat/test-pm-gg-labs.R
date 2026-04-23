library(testthat)
library(pmplots)
skip_if_not_installed("yspec")
library(dplyr)
library(yspec)

context("test-pm-gg-labs")

data <- ys_help$data()
spec <- ys_help$spec()

spec <- update_short(spec, DV = "Concentration", TIME = "Time")
spec$DV$unit <- "ng/mL"

labs <- list(
  TIME = "Time after first dose"
)

p0 <-
  ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
  ggplot2::geom_point()

test_that("label x- and y- from the spec", {
  expect_equal(ggplot2:::get_labs(p0)$x, "TIME")
  expect_equal(ggplot2::get_labs(p0)$y, "DV")

  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("labs overrides spec for x- and y-", {
  p <- p0 + pm_gg_labs(spec, labs)
  expect_equal(ggplot2::get_labs(p)$x, "Time after first dose")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("force in x- or y-", {
  p <- p0 + pm_gg_labs(spec, labs, x = I("A"), y = I("B"))
  expect_equal(ggplot2::get_labs(p)$x, "A")
  expect_equal(ggplot2::get_labs(p)$y, "B")
})

test_that("transformed x- or y- is ignored", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) +
    ggplot2::geom_point()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "log(DV)")
})

test_that("transformed x- or y- in labs", {
  labs[["log(DV)"]] <- "log-transformed DV"
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) +
    ggplot2::geom_point()
  p <- p0 + pm_gg_labs(spec, labs)
  expect_equal(ggplot2::get_labs(p)$x, "Time after first dose")
  expect_equal(ggplot2::get_labs(p)$y, "log-transformed DV")
})

test_that("factor x- or y- is passed through", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(factor(CP), DV)) +
    ggplot2::geom_boxplot()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Child-Pugh score")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("label colour", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh score")
  p <- p0 + pm_gg_labs(spec, colour = I("test colour"))
  expect_equal(ggplot2::get_labs(p)$colour, "test colour")
})

test_that("label fill", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(RF, DV, fill = factor(CP))) +
    ggplot2::geom_boxplot()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$fill, "Child-Pugh score")
  p <- p0 + pm_gg_labs(spec, fill = I("test fill"))
  expect_equal(ggplot2::get_labs(p)$fill, "test fill")
})

test_that("label linetype", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$linetype, "Child-Pugh score")
  p <- p0 + pm_gg_labs(spec, linetype = I("test linetype"))
  expect_equal(ggplot2::get_labs(p)$linetype, "test linetype")
})

test_that("col, color, and colour are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p1 <- p0 + pm_gg_labs(spec, colour = I("test colour"))
  p2 <- p0 + pm_gg_labs(spec, col = I("test colour"))
  p3 <- p0 + pm_gg_labs(spec, color = I("test colour"))
  expect_equal(ggplot2::get_labs(p1)$colour, ggplot2::get_labs(p2)$colour)
  expect_equal(ggplot2::get_labs(p1)$colour, ggplot2::get_labs(p3)$colour)
})

test_that("lty and linetype are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p1 <- p0 + pm_gg_labs(spec, linetype = I("test linetype"))
  p2 <- p0 + pm_gg_labs(spec, lty = I("test linetype"))
  expect_equal(ggplot2::get_labs(p1)$linetype, ggplot2::get_labs(p2)$linetype)
})

test_that("label shape", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, shape = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$shape, "Child-Pugh score")
  p <- p0 + pm_gg_labs(spec, shape = I("test shape"))
  expect_equal(ggplot2::get_labs(p)$shape, "test shape")
})

test_that("layer-level colour is labelled", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(ggplot2::aes(colour = factor(CP)))
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh score")
})

test_that("layer-level fill is labelled", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(RF, DV)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = factor(CP)))
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$fill, "Child-Pugh score")
})

test_that("layer-level shape is labelled", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(ggplot2::aes(shape = factor(CP)))
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$shape, "Child-Pugh score")
})

test_that("layer-level linetype is labelled", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_line(ggplot2::aes(linetype = factor(CP)))
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$linetype, "Child-Pugh score")
})

test_that("all aesthetics at layer level are labelled", {
  p0 <-
    ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(TIME, DV, colour = factor(CP)))
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh score")
})

test_that("inform when same aesthetic maps to two spec-matched variables", {
  d2 <- filter(data, ID == 2) |> mutate(TAFD = TIME)
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(data = d2, ggplot2::aes(TAFD, DV))
  expect_message(
    p0 + pm_gg_labs(spec),
    regexp = "Aesthetic 'x'.*TIME.*TAFD"
  )
})

test_that("no message when only one variable is spec-matched", {
  d2 <- filter(data, ID == 2) |> rename(CONC = DV)
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(data = d2, ggplot2::aes(TIME, CONC))
  expect_no_message(p0 + pm_gg_labs(spec))
})

test_that("quietly = TRUE suppresses conflicting-aesthetic message", {
  d2 <- filter(data, ID == 2) |> mutate(TAFD = TIME)
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(data = d2, ggplot2::aes(TAFD, DV))
  expect_no_message(p0 + pm_gg_labs(spec, quietly = TRUE))
  p <- suppressMessages(p0 + pm_gg_labs(spec))
  p2 <- p0 + pm_gg_labs(spec, quietly = TRUE)
  expect_equal(ggplot2::get_labs(p)$x, ggplot2::get_labs(p2)$x)
})

test_that("no message when user passes aesthetic directly", {
  d2 <- filter(data, ID == 2) |> mutate(TAFD = TIME)
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
    ggplot2::geom_point(data = d2, ggplot2::aes(TAFD, DV))
  expect_no_message(p0 + pm_gg_labs(spec, x = I("My time label")))
})

test_that("pm_gg_labs x_break inserts newline in x label at word boundary", {
  # spec: TIME = "Time (hour)" (11 chars) — space at 5; x_break 7 picks 5
  p <- p0 + pm_gg_labs(spec, x_break = 7)
  expect_equal(ggplot2::get_labs(p)$x, "Time\n(hour)")
})

test_that("pm_gg_labs y_break inserts newline in y label at word boundary", {
  # spec: DV = "Concentration (ng/mL)" (21 chars) — space at 14; y_break 15 picks 14
  p <- p0 + pm_gg_labs(spec, y_break = 15)
  expect_equal(ggplot2::get_labs(p)$y, "Concentration\n(ng/mL)")
})

test_that("pm_gg_labs x_break = Inf (default) does not insert newlines", {
  p <- p0 + pm_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("top-level mapping wins over layer-level", {
  d2 <- data[data$ID==1, c("TIME", "DV")]
  names(d2) <- c("TAFO", "CONC")
  labs <- list(TAFO = "Time after first something", CONC  = "Drug concentration")
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) + 
    ggplot2::geom_point(ggplot2::aes(colour = factor(CP))) +
    ggplot2::geom_point(data = d2, ggplot2::aes(TAFO, CONC), color = "black")
  p <- p0 + pm_gg_labs(spec, labs, quietly = TRUE)
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh score")
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})
