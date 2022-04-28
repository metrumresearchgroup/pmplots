#' Create plots showing DV, PRED, and IPRED by individual
#'
#' `DV` is plotted with a symbol while `PRED` and `IPRED` are plotted with lines
#' and symbols. The plot is faceted by unique individual identifier (like `ID`
#' or `USUBJID`) as well as other faceting variables.
#'
#' @param data the data frame to plot.
#' @param ... additional arguments passed to [dv_pred_ipred_impl()].
#' @param id_per_plot number of unique combinations of `facets` columns to
#' include on each page.
#' @param facets a character vector of column names to use for faceting the
#' plot, passed to [ggplot2::facet_wrap()]; if passed in [col_label()] format,
#' then the column data is modified with a call to [glue::glue_data()], so that
#' the strip label is more informative; see examples.
#' @param ncol number of columns in the plot grid; passed to
#' [ggplot2::facet_wrap()].
#' @param nrow number of rows in the plot grid; passed to
#' [ggplot2::facet_wrap()].
#' @param x the time-axis column, in
#' [col_label()] format; the title portion is
#' used for the x-axis title along with `xunit`; see also the `xlab` argument.
#' @param dv the `DV` column, in [col_label()] format; the title portion is used
#' for the y-axis title; see also the `ylab` argument.
#' @param pred the name of the `PRED` column; [col_label()] format is allowed,
#' but the label portion is discarded.
#' @param ipred the name of the `IPRED` column; [col_label()] format is allowed,
#' but the label portion is discarded.
#' @param dv_color color to use for `DV` points.
#' @param ipred_color color to use for `IPRED` line.
#' @param pred_color color to use for `PRED` line.
#' @param pred_lty `PRED` linetype; passed to [ggplot2::geom_line()].
#' @param ipred_lty `IPRED` linetype; passed to [ggplot2::geom_line()].
#' @param pred_point logical; should points be plotted for `PRED`?
#' @param ipred_point logical; should points be plotted for `IPRED`?
#' @param dv_shape shape for `DV`; passed to [ggplot2::geom_point()].
#' @param dv_line logical; if `TRUE` then a line is added to the plot connecting
#' `DV` points.
#' @param lwd line width for `PRED` and `IPRED`; passed to
#' [ggplot2::geom_line()].
#' @param dv_lwd  line width for `DV`; passed to [ggplot2::geom_line()] as
#' `lwd`.
#' @param size size of shape for `DV`, `IPRED` and `PRED`; passed
#' to [ggplot2::geom_point()].
#' @param xbreaks x-axis (time) breaks; passed to
#' [ggplot2::scale_x_continuous()].
#' @param angle rotation angle for x-axis tick labels; passed to [rot_x()].
#' @param xunit used to form x-axis title only if `xlab` is not provided.
#' @param xlab x-axis title; if not `NULL`, passed to [ggplot2::xlab()].
#' @param ylab y-axis title; if not `NULL`, passed to [ggplot2::ylab()].
#' @param log_y logical; if `TRUE` then y-axis is shown in log-scale.
#' @param plot.margin for the plot; passed [ggplot2::margin()].
#' @param strip.text optionally, the result of [ggplot2::element_text()] to
#' format the strip text (e.g. change the font size or padding).
#' @param legend.position passed to [ggplot2::theme()].
#' @param scales passed to [ggplot2::facet_wrap()].
#' @param use_theme a theme to use for the plot.
#' @param axis.text.rel relative text size for axis text; use this to
#' selectively decrease font size for axis tick labels.
#' @param fun a function accepting a gg object as argument and returning
#' an updated gg object; if supplied, this function is applied to each plot
#' in the output list.
#' @param id_col deprecated; use `facets` argument instead.
#' @param font_size deprecated.
#' @param margin deprecated.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' p <- dv_pred_ipred(
#'   data,
#'   ylab = "Concentration (ng/mL)",
#'   nrow = 3, ncol = 3
#' )
#'
#' p <- dv_pred_ipred(
#'   data,
#'   facets = c("ID", "STUDYc//Study: {STUDYc}")
#' )
#'
#' @md
#' @export
dv_pred_ipred <- function(data, id_per_plot = 9, facets = "ID",
                          nrow = NULL, ncol = NULL,
                          fun = NULL, ..., id_col = deprecated()) {

  if(is_present(id_col)) {
    deprecate_warn(
      "0.3.5",
      "dv_pred_ipred(id_col = )",
      "dv_pred_ipred(id_col = )"
    )
  }

  assert_that(is.character(facets))
  assert_that(length(facets) > 0)
  facets <- col_labels(facets)
  ans <- chunk_by_cols(data, id_per_chunk = id_per_plot, cols = unname(facets))
  out <- lapply(
    ans,
    dv_pred_ipred_impl,
    ...,
    facets = facets,
    nrow = nrow,
    ncol = ncol
  )
  if(is.function(fun)) out <- lapply(out, fun)
  out
}

#' @rdname dv_pred_ipred
#' @export
dv_pred_ipred_impl <- function(data,
                               x = pm_axis_time(),
                               dv  = pm_axis_dv(),
                               pred = pm_axis_pred(),
                               ipred = pm_axis_ipred(),
                               facets = "ID",
                               id_col = deprecated(),
                               xbreaks = waiver(),
                               xunit = pm_opts$time.unit,
                               xlab  = NULL,
                               ylab  = NULL,
                               angle = NULL,
                               strip.text = element_text(),
                               font_size = deprecated(),
                               margin = deprecated(),
                               plot.margin = NULL,
                               legend.position = "top",
                               pred_lty = 2,
                               ipred_lty = 1,
                               pred_point = TRUE,
                               ipred_point = TRUE,
                               lwd = 0.5,
                               size = pm_opts$scatter.size,
                               dv_shape = 19,
                               dv_line = FALSE,
                               dv_lwd = 0.5,
                               scales = "free",
                               log_y = FALSE,
                               use_theme = pm_theme(),
                               dv_color = "black",
                               ipred_color = "red2",
                               pred_color = "blue2",
                               ncol = NULL,
                               nrow = NULL,
                               axis.text.rel = NULL,
                               fun = NULL) {

  if(is_present(margin)) {
    deprecate_warn(
      "0.3.5",
      "dv_pred_ipred_impl(margin = )",
      "dv_pred_ipred_impl(strip.text = )"
    )
  }
  if(is_present(font_size)) {
    deprecate_warn(
      "0.3.5",
      "dv_pred_ipred_impl(font_size = )",
      "dv_pred_ipred_impl(strip.text = )"
    )
  }

  show_dv <- TRUE
  show_ipred <- TRUE
  show_pred <- TRUE
  ycols <- character(0)
  clrs <- character(0)
  lnes <- integer(0)
  shapes <- integer(0)

  assert_that(is_named(facets))
  facets_glue <- facets[names(facets) != facets]

  if(!all(facets %in% names(data))) {
    stop("cannot find all facets in `data`", call. = FALSE)
  }
  if(is.null(dv)) {
    dv <- col_label("DV")
    dv <- dv[1]
    show_dv <- FALSE
  } else {
    dv <- col_label(dv)
    dv <- dv[1]
    require_numeric(data,dv)
    ycols <- c(ycols, dv)
    clrs <- c(clrs, dv_color)
    shapes <- c(shapes, dv_shape)
    lnes <- c(lnes, 0)
    if(anyNA(data[[dv]])) {
      warning("[pmplots] removed missing values in dv column", call.=FALSE)
    }
  }

  if(is.null(pred)) {
    pred <- col_label("PRED")[1]
    show_pred <- FALSE
  } else {
    pred <- col_label(pred)[1]
    require_numeric(data, pred)
    ycols <- c(ycols, pred)
    clrs <- c(clrs, pred_color)
    lnes <- c(lnes, pred_lty)
    pred_shape <- NA_integer_
    if(isTRUE(pred_point)) pred_shape <- dv_shape
    shapes <- c(shapes, pred_shape)
    if(anyNA(data[[pred]])) {
      warning("[pmplots] removed missing values in pred column",call.=FALSE)
    }
  }

  if(is.null(ipred)) {
    ipred <- col_label("IPRED")[1]
    show_ipred <- FALSE
  } else {
    ipred <- col_label(ipred)[1]
    require_numeric(data, ipred)
    ycols <- c(ycols, ipred)
    clrs <- c(clrs, ipred_color)
    lnes <- c(lnes, ipred_lty)
    ipred_shape <- NA_integer_
    if(isTRUE(ipred_point)) ipred_shape <- dv_shape
    shapes <- c(shapes, ipred_shape)
    if(anyNA(data[[ipred]])) {
      warning("[pmplots] removed missing values in ipred column",call.=FALSE)
    }
  }

  if(length(ycols)==0) {
    stop("no y columns to plot",call.=FALSE)
  }

  x <- col_label(x)
  xl <- glue_unit(x[2], xunit)
  x <- x[1]
  yl <- "value"

  require_numeric(data, x[1])

  if(is.character(ylab)) yl <- ylab
  if(is.character(xlab)) xl <- xlab

  data <- data[,unique(c(facets, x[1], ycols)), drop = FALSE]
  data <- pivot_longer(
    data,
    cols = unname(ycols),
    values_to = "value",
    names_to = "name"
  )

  if(length(facets_glue) > 0) {
    to_glue <- names(facets_glue)
    for(i in seq_along(facets_glue)) {
      data[[facets_glue[i]]] <- glue_data(data, to_glue[[i]])
    }
  }

  data$name <- factor(data$name, levels = rev(ycols), labels = rev(ycols))
  data <- arrange(data, .data$name)
  names(clrs) <- unname(ycols)
  names(lnes) <- unname(ycols)
  names(shapes) <- unname(ycols)

  p <-
    ggplot(data, aes(x = .data[[x[1]]], y = .data$value)) +
    scale_color_manual(name = "", values = clrs) +
    scale_linetype_manual(name = "", values = lnes) +
    scale_shape_manual(name = "", values = shapes) +
    geom_line(aes(lty =    .data$name, col = .data$name), lwd = lwd) +
    geom_point(aes(shape = .data$name, col = .data$name), na.rm = TRUE, size = size)

  if(dv_line) {
    dfline <- filter(data, .data$name == dv)
    p <- p + geom_line(data = dfline, col = dv_color, lwd = dv_lwd)
  }

  p <-
    p +
    facet_wrap(facets, ncol = ncol, nrow = nrow, scales = scales) +
    scale_x_continuous(breaks = xbreaks) +
    ylab(yl) + xlab(xl)

  if(log_y) p <- p + scale_y_log10()

  use_theme <-
    use_theme +
    theme(
      legend.position = legend.position,
      strip.text = strip.text
    )

  if(is.numeric(axis.text.rel)) {
    axtx <- element_text(size = ggplot2::rel(axis.text.rel))
    use_theme <- use_theme + theme(axis.text = axtx)
  }
  if(inherits(margin, "margin")) {
    use_theme <- use_theme + theme(plot.margin = plot.margin)
  }

  p <- p + use_theme

  if(is.numeric(angle)) p <- p + rot_x(angle)

  p
}

#' @param options a named list of options to pass to [dv_pred_ipred]
#' @rdname dv_pred_ipred
#' @export
do_dv_pred_ipred <- function(data, options=list()) {
  options[["data"]] <- data
  do.call(dv_pred_ipred, options)
}
