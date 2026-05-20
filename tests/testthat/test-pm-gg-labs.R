library(testthat)
library(pmplots)
skip_if_not_installed("yspec")
library(dplyr)
library(yspec)

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

test_that("pm_gg_labs var_break breaks a named column's label at word boundary", {
  # DV = "Concentration (ng/mL)" — space at 14; var_break 15 picks 14
  p <- p0 + pm_gg_labs(spec, var_break = list(DV = 15))
  expect_equal(ggplot2::get_labs(p)$y, "Concentration\n(ng/mL)")
  # TIME label is unaffected
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
})

test_that("pm_gg_labs var_break accepts a named numeric vector", {
  p <- p0 + pm_gg_labs(spec, var_break = c(DV = 15))
  expect_equal(ggplot2::get_labs(p)$y, "Concentration\n(ng/mL)")
})

test_that("pm_gg_labs var_break silently ignores keys not in spec/labs", {
  expect_no_error(p0 + pm_gg_labs(spec, var_break = list(NOTACOL = 10)))
  p <- p0 + pm_gg_labs(spec, var_break = list(NOTACOL = 10))
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

# pm_gg_break / pm_gg_break_aes ------------------------------------------

# A plot with long labels set explicitly so tests don't depend on spec content
pb <- ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Population predicted concentration (ng/mL)",
    y = "Observed concentration (ng/mL)"
  )

test_that("pm_gg_break breaks x label by variable name", {
  p <- pb + pm_gg_break(TIME = 20)
  expect_equal(ggplot2::get_labs(p)$x, "Population\npredicted concentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$y, "Observed concentration (ng/mL)")
})

test_that("pm_gg_break breaks y label by variable name", {
  p <- pb + pm_gg_break(DV = 20)
  expect_equal(ggplot2::get_labs(p)$y, "Observed\nconcentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$x, "Population predicted concentration (ng/mL)")
})

test_that("pm_gg_break breaks multiple labels in one call", {
  p <- pb + pm_gg_break(TIME = 20, DV = 20)
  expect_equal(ggplot2::get_labs(p)$x, "Population\npredicted concentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$y, "Observed\nconcentration (ng/mL)")
})

test_that("pm_gg_break with unknown variable name is silently ignored", {
  expect_no_error(pb + pm_gg_break(NOTACOL = 10))
  p <- pb + pm_gg_break(NOTACOL = 10)
  expect_false(grepl("\n", ggplot2::get_labs(p)$x, fixed = TRUE))
  expect_false(grepl("\n", ggplot2::get_labs(p)$y, fixed = TRUE))
})

test_that("pm_gg_break width wider than label leaves label unchanged", {
  p <- pb + pm_gg_break(TIME = 200)
  expect_false(grepl("\n", ggplot2::get_labs(p)$x, fixed = TRUE))
})

test_that("pm_gg_break_aes breaks x label by aesthetic name", {
  p <- pb + pm_gg_break_aes(x = 20)
  expect_equal(ggplot2::get_labs(p)$x, "Population\npredicted concentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$y, "Observed concentration (ng/mL)")
})

test_that("pm_gg_break_aes breaks y label by aesthetic name", {
  p <- pb + pm_gg_break_aes(y = 20)
  expect_equal(ggplot2::get_labs(p)$y, "Observed\nconcentration (ng/mL)")
  expect_equal(ggplot2::get_labs(p)$x, "Population predicted concentration (ng/mL)")
})

test_that("pm_gg_break variable and aes modes produce the same result", {
  p1 <- pb + pm_gg_break(TIME = 20, DV = 20)
  p2 <- pb + pm_gg_break_aes(x = 20, y = 20)
  expect_equal(ggplot2::get_labs(p1)$x, ggplot2::get_labs(p2)$x)
  expect_equal(ggplot2::get_labs(p1)$y, ggplot2::get_labs(p2)$y)
})

test_that("pm_gg_break_aes breaks colour label", {
  pc <- pb + ggplot2::aes(colour = factor(CP)) +
    ggplot2::labs(colour = "Child-Pugh score group")
  p <- pc + pm_gg_break_aes(colour = 12)
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh\nscore group")
})

test_that("pm_gg_break aborts on non-numeric value", {
  expect_error(pm_gg_break(TIME = "foo"), "named numeric")
})

test_that("pm_gg_break aborts on unnamed argument", {
  expect_error(pm_gg_break(20), "named numeric")
})

# pm_label_break ------------------------------------------

test_that("pm_label_break returns a labeller function", {
  expect_true(is.function(pm_label_break(10)))
})

test_that("pm_label_break breaks a long strip label at word boundary", {
  lb <- pm_label_break(10)
  # "A long label here": spaces at 2, 7, 13; width 10 picks last valid (7)
  result <- lb(data.frame(grp = "A long label here"))
  expect_equal(result[[1]], "A long\nlabel here")
})

test_that("pm_label_break leaves a label shorter than width unchanged", {
  lb <- pm_label_break(20)
  result <- lb(data.frame(grp = "Short"))
  expect_equal(result[[1]], "Short")
})

test_that("pm_label_break handles multiple labels in one facet variable", {
  lb <- pm_label_break(10)
  result <- lb(data.frame(grp = c("A long label here", "Short")))
  expect_equal(result[[1]], c("A long\nlabel here", "Short"))
})

test_that("pm_label_break leaves a label that already contains a newline unchanged", {
  lb <- pm_label_break(5)
  result <- lb(data.frame(grp = "Already\nbroken"))
  expect_equal(result[[1]], "Already\nbroken")
})
