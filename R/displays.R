class_pm_display <- function(x) {
  stopifnot(is.list(x))
  class(x) <- c("pm_display", class(x))
  x
}

diagnostic_display_list <- function(df, x, y, fun_cat, fun_cont) {
  out <- vector(mode = "list", length = length(y))
  for(i in seq_along(y)) {
    out[[i]] <- lapply(seq_along(x), function(ii) {
      col <- col_label(x[[ii]])[[1]]
      require_column(df, col)
      if(inherits(unlist(df[, col]), c("character", "factor", "logical", "integer"))) {
        p <- fun_cat(df, x = x[ii], y = y[i])
      } else {
        p <- fun_cont(df, x = x[ii], y = y[i])
      }
    })
  }
  out
}

#' Create ETA versus covariate displays
#'
#' Get a single graphic of `ETA` versus continuous and / or categorical
#' covariates (`eta_covariate()`) or the component plots as a list
#' that can be arranged by the user (`eta_covariate_list()`).
#'
#' @param df a data frame to plot.
#' @param x character `col//title` for covariates to plot on x-axis;
#' see [col_label()].
#' @param y character `col//title` for ETAs to plot on y-axis; see [col_label()].
#' @param ncol passed to [pm_grid()].
#' @param byrow passed to [pm_grid()].
#' @param tag_levels passed to [patchwork::plot_annotation()].
#' @param transpose logical; if `TRUE`, output will be transposed to group
#' plots by the covariate, rather than the `ETA`; see **Examples**.
#'
#' @details
#' Pass `ncol = NULL` or another non-numeric value to bypass arranging plots
#' coming from `eta_covariate()`.
#'
#' @examples
#' data <- pmplots_data_id()
#' etas <- c("ETA1//ETA-CL", "ETA2//ETA-V")
#' cont <- c("WT//Weight (kg)", "ALB//Albumin (mg/dL)")
#' cats <- c("RF//Renal function", "CPc//Child-Pugh")
#'
#' eta_covariate(data, x = c(cont, cats), y = etas, tag_levels = "A")
#'
#' eta_covariate(data, cont, y = etas)
#' eta_covariate(data, cont, y = etas, transpose = TRUE)
#'
#' eta_covariate_list(data, x = cats, y = etas)
#' eta_covariate_list(data, x = cats, y = etas, transpose = TRUE)
#'
#' @return
#' `eta_covariate()` returns a list of plots arranged in graphics as a
#' `patchwork` object using [pm_grid()]. `eta_covariate_list()` the same
#' plots, but unarranged as a list of lists.
#'
#' When `transpose` is `FALSE` (default), plots in a single graphic are grouped
#' by the `ETA`, and the names of the list reflect that name (e.g., `ETA1`).
#' When `transpose` is `TRUE`, the graphics are grouped by column names passed
#' via `x` and the names of the list reflect those covariate data names. See
#' **Examples**.
#'
#' @seealso [npde_covariate()], [cwres_covariate()]
#' @md
#' @export
eta_covariate <- function(df, x, y, ncol = 2, tag_levels = NULL, byrow = NULL,
                          transpose = FALSE) {
  require_patchwork()
  if(missing(ncol) && length(x)==1) {
    ncol <- 1
  }
  p <- eta_covariate_list(df, x, y, transpose)
  if(is.numeric(ncol)) {
    p <- lapply(p, pm_grid, ncol = ncol, byrow = byrow)
  }
  if(!is.null(tag_levels)) {
    p <- lapply(p, function(x) x + patchwork::plot_annotation(tag_levels = tag_levels))
  }
  p
}

#' @rdname eta_covariate
#' @export
eta_covariate_list <- function(df, x, y, transpose = FALSE) {
  p  <- diagnostic_display_list(df, x, y, eta_cat, eta_cont)
  labx <- col_label_col(x)
  laby <- col_label_col(y)
  p  <- lapply(p, setNames, nm = labx)
  names(p) <- laby
  if(isTRUE(transpose)) {
    p <- list_transpose(p)
  }
  p <- lapply(p, class_pm_display)
  p
}

#' Create NPDE versus covariate displays
#'
#' Get a single graphic of `NPDE` versus continuous and / or categorical
#' covariates (`npde_covariate()`) or get a list that can be arranged by the
#' user  (`npde_covariate_list()`). See [npde_panel()] for other `NPDE`
#' diagnostic displays.
#'
#' @inheritParams eta_covariate
#'
#' @details
#' Pass `ncol = NULL` or another non-numeric value to bypass arranging plots
#' coming from `npde_covariate()`.
#'
#' @examples
#' data <- pmplots_data_id()
#' cont <- c("WT//Weight (kg)", "ALB//Albumin (mg/dL)")
#' cats <- c("RF//Renal function", "CPc//Child-Pugh")
#'
#' npde_covariate(data, x = c(cont, cats), tag_levels = "A")
#' npde_covariate_list(data, x = cats)
#'
#'
#' @return
#' `npde_covariate()` returns single graphic of scatter plot diagnostics
#' as a `patchwork` object that has been arranged using [pm_grid()] and
#' `npde_covariate_list()` returns the same component plots unarranged in a list.
#'
#' @seealso [cwres_covariate()], [eta_covariate()]
#' @md
#' @export
npde_covariate <- function(df, x, ncol = 2, tag_levels = NULL, byrow = NULL) {
  require_patchwork()
  if(missing(ncol) && length(x)==1) {
    ncol <- 1
  }
  p <- npde_covariate_list(df, x)
  if(is.numeric(ncol)) {
    p <- pm_grid(p, ncol = ncol, byrow = byrow)
  }
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname npde_covariate
#' @export
npde_covariate_list <- function(df, x) {
  p  <- diagnostic_display_list(df, x, pm_axis_npde(), npde_cat, npde_cont)
  p <- p[[1]]
  labx <- col_label_col(x)
  names(p) <- labx
  p <- class_pm_display(p)
  p
}

#' Create CWRES versus covariate displays
#'
#' Get a single graphic of `CWRES` versus continuous and / or categorical
#' covariates (`cwres_covariate()`) or get a list that can be arranged by
#' the user (`cwres_covariate_list()`). See [cwres_panel()] for other `CWRES`
#' diagnostic displays.
#'
#' @inheritParams eta_covariate
#'
#' @details
#' Pass `ncol = NULL` or another non-numeric value to bypass arranging plots
#' coming from `cwres_covariate()`.
#'
#' @examples
#' data <- pmplots_data_id()
#' cont <- c("WT//Weight (kg)", "ALB//Albumin (mg/dL)")
#' cats <- c("RF//Renal function", "CPc//Child-Pugh")
#'
#' cwres_covariate(data, x = c(cont, cats), tag_levels = "A")
#' cwres_covariate_list(data, x = cont)
#'
#' @return
#' `cwres_covariate()` returns single graphic of scatter plot diagnostics
#' as a `patchwork` object that has been arranged using [pm_grid()] and
#' `cwres_covariate_list()` returns the same component plots unarranged in a
#' list.
#'
#' @seealso [npde_covariate()], [eta_covariate()]
#' @md
#' @export
cwres_covariate <- function(df, x, ncol = 2, tag_levels = NULL, byrow = NULL) {
  require_patchwork()
  if(missing(ncol) && length(x)==1) {
    ncol <- 1
  }
  p <- cwres_covariate_list(df, x)
  if(is.numeric(ncol)) {
    p <- pm_grid(p, ncol = ncol, byrow = byrow)
  }
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname cwres_covariate
#' @export
cwres_covariate_list <- function(df, x) {
  p  <- diagnostic_display_list(df, x, pm_axis_npde(), cwres_cat, cwres_cont)
  p <- p[[1]]
  labx <- col_label_col(x)
  names(p) <- labx
  p <- class_pm_display(p)
  p
}

#' Create a display of NPDE diagnostic plots
#'
#' Get a single graphic of basic `NPDE` diagnostics (`npde_panel()`) or get the
#' component plots in a list that can be arranged by the user
#' (`npde_panel_list()`) . See [npde_covariate()] for plotting `NPDE` versus
#' covariates.
#'
#' @inheritParams eta_covariate
#'
#' @param unit_tad passed to [npde_time()] as `xunit`.
#' @param unit_time passed to [npde_tad()] as `xunit`.
#' @param xname passed to [npde_pred()].
#' @param xby_time passed to [npde_time()] as `xby`.
#' @param xby_tad passed to [npde_tad()] as `xby`.
#'
#' @examples
#' data <- pmplots_data_obs()
#' npde_panel(data, tag_levels = "A")
#'
#' l <- npde_panel_list(data)
#' names(l)
#' with(l, (q+hist) / pred, tag_levels = "a")
#'
#' @return
#' `npde_panel()` returns a single graphic as a `patchwork` object with the
#' following panels:
#'
#' - `NPDE` versus `TIME` via [npde_time()]
#' - `NPDE` versus `TAD` via [npde_tad()]
#' - `NPDE` versus `PRED` via [npde_pred()]
#' - `NPDE` histogram via [npde_hist()]
#' - `NPDE` quantile-quantile plot via [npde_q()]
#'
#' `npde_panel_list()` returns a list of the individual plots that are
#' incorporated into the `npde_panel()` output. Each element of the list
#' is named for the plot in that position: `time`, `tad`, `pred`, `hist`
#' `q`. See **Examples** for how you can work with that list.
#'
#' @seealso [cwres_panel()]
#'
#' @md
#' @export
npde_panel <- function(df, xname = "value",
                       unit_time = "hours", unit_tad = "hours",
                       xby_time  = NULL, xby_tad = NULL,
                       tag_levels = NULL) {
  require_patchwork()
  l <- npde_panel_list(df, xname, unit_time, unit_tad, xby_time, xby_tad)
  time <- l$time
  if(!is.null(l$tad)) {
    time <- time + l$tad
  }
  p <- time/l$pred/(l$hist+l$q)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname npde_panel
#' @export
npde_panel_list <- function(df, xname = "value",
                            unit_time = "hours", unit_tad = "hours",
                            xby_time  = NULL, xby_tad = NULL) {
  time <- npde_time(df, xunit = unit_time, xby = xby_time)
  tad <- NULL
  if("TAD" %in% names(df)) {
    tad <- npde_tad(df, xunit = unit_tad, xby = xby_tad)
  }
  pred <- npde_pred(df, xname = xname)
  hist <- npde_hist(df)
  q <- npde_q(df)
  p <- list(time = time, tad = tad, hist = hist, q = q, pred = pred)
  p <- class_pm_display(p)
  p
}

#' Create a display of CWRES diagnostic plots
#'
#' Get a single graphic of basic `CWRES` diagnostics (`cwres_panel()`) or get
#' the component plots in a list that can be arranged by the user
#' (`cwres_panel_list()`) . See [cwres_covariate()] for plotting `CWRES`
#' versus covariates.
#'
#' @inheritParams npde_panel
#'
#' @examples
#' data <- pmplots_data_obs()
#' cwres_panel(data, tag_levels = "A")
#'
#' l <- cwres_panel_list(data)
#' with(l, (q+hist) / pred)
#'
#' @return
#' `cwres_panel()` returns a single graphic with the following panels:
#'
#' - `CWRES` versus `TIME` via [cwres_time()]
#' - `CWRES` versus `TAD` via [cwres_tad()]
#' - `CWRES` versus `PRED` via [cwres_pred()]
#' - `CWRES` histogram via [cwres_hist()]
#' - `CWRES` quantile-quantile plot via [cwres_q()]
#'
#' `cwres_panel_list()` returns a list of the individual plots that are
#' incorporated into the `cwres_panel()` output. Each element of the list
#' is named for the plot in that position: `time`, `tad`, `pred`, `hist`
#' `q`. See **Examples** for how you can work with that list.
#'
#' @seealso [npde_panel()], [npde_covariate()]
#' @md
#' @export
cwres_panel <- function(df, xname = "value",
                        unit_time = "hours", unit_tad = "hours",
                        xby_time  = NULL, xby_tad = NULL, tag_levels = NULL) {
  require_patchwork()
  l <- cwres_panel_list(df, xname, unit_time, unit_tad, xby_time, xby_tad)
  time <- l$time
  if(!is.null(l$tad)) {
    time <- time + l$tad
  }
  p <- time/l$pred/(l$hist+l$q)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname cwres_panel
#' @export
cwres_panel_list <- function(df, xname = "value",
                             unit_time = "hours", unit_tad = "hours",
                             xby_time = NULL, xby_tad = NULL) {
  time <- cwres_time(df, xunit = unit_time, xby = xby_time)
  tad <- NULL
  if("TAD" %in% names(df)) {
    tad <- cwres_tad(df, xunit = unit_tad, xby = xby_tad)
  }
  pred <- cwres_pred(df, xname = xname)
  hist <- cwres_hist(df)
  q <- cwres_q(df)
  p <- list(time = time, tad = tad, hist = hist, q = q, pred = pred)
  p <- class_pm_display(p)
  p
}

#' Create a display of residual histograms and quantile-quantile plots
#'
#' Get a single graphic showing `NPDE` or `CWRES` histogram and
#' quantile-quantile plots.
#'
#' @inheritParams npde_panel
#' @inheritParams eta_covariate
#'
#' @examples
#' data <- pmplots_data_obs()
#' npde_hist_q(data, tag_levels = "a")
#' npde_hist_q(data, tag_levels = "a", ncol = 2)
#' cwres_hist_q(data)
#'
#' @details
#' The default value for `ncol` (1) means the two plots will be arranged in a
#' single column, with the histogram on the top and quantile-quantile plot
#' on the bottom. Using `ncol=2` will return an graphic with the plots
#' side by side.
#'
#' @return
#' A single graphic is returned, with a `NPDE`  or `CWRES` histogram and
#' quantile-quantile plot arranged in an object
#' using [patchwork::plot_annotation()].
#'
#' @name res_hist_q
#' @export
npde_hist_q <- function(df, ncol = 1, tag_levels = NULL) {
  require_patchwork()
  hist <- npde_hist(df)
  q <- npde_q(df)
  p <- pm_grid(list(hist, q), ncol = ncol)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname res_hist_q
#' @export
cwres_hist_q <- function(df, ncol = 1, tag_levels = NULL) {
  require_patchwork()
  hist <- cwres_hist(df)
  q <- cwres_q(df)
  p <- pm_grid(list(hist, q), ncol = ncol)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' Create a display of residuals versus time and predicted values
#'
#' Get a single graphic of `NPDE` or `CWRES` diagnostics versus `TIME`,
#' `TAD` and `PRED`. Output can be in either long or compact format.
#'
#' @inheritParams npde_panel
#' @param compact use `compact = TRUE` to get a more compact display; see
#' **Examples**.
#'
#' @return
#' A single graphic with three panels (`NPDE` or `CWRES` versus `TIME`, `TAD`
#' and `PRED`) as a `patchwork` object. The default behavior is to create a
#' graphic with three panels in three rows, filling a portrait page.
#' Use `compact = TRUE` for a single graphic in two rows, with the `TIME`
#' plot on the top and the `TAD` and `PRED` plots on the bottom.
#'
#' @examples
#' data <- pmplots_data_obs()
#' npde_scatter(data)
#' npde_scatter(data, compact = TRUE)
#'
#' @seealso [npde_panel()], [npde_panel_list()], [cwres_panel()],
#' [cwres_panel_list()]
#'
#' @export
npde_scatter <- function(df, xname = "value",
                         unit_time = "hours", unit_tad = "hours",
                         xby_time  = NULL, xby_tad = NULL,
                         tag_levels = NULL, compact = FALSE) {
  require_patchwork()
  time <- npde_time(df, xby = xby_time, xunit = unit_time)
  tad <- npde_tad(df, xby = xby_tad, xunit = unit_tad)
  pred <- npde_pred(df, xname = xname)
  if(isTRUE(compact)) {
    p <- time / (tad + pred)
  } else {
    p <- time / tad / pred
  }
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' @rdname npde_scatter
#' @export
cwres_scatter <- function(df, xname = "value",
                          unit_time = "hours", unit_tad = "hours",
                          xby_time  = NULL, xby_tad = NULL,
                          tag_levels = NULL, compact = FALSE) {
  require_patchwork()
  time <- cwres_time(df, xby = xby_time, xunit = unit_time)
  tad <- cwres_tad(df, xby = xby_tad, xunit = unit_tad)
  pred <- cwres_pred(df, xname = xname)
  if(isTRUE(compact)) {
    p <- time / (tad + pred)
  } else {
    p <- time / tad / pred
  }
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}

#' Create a display of continuous versus categorical covariates
#'
#' Get a single graphic of continuous covariate boxplots split by
#' levels of different categorical covariates (`cont_cat_panel()`).
#' Alternatively, get the component plots to be arranged by the user
#' (`cont_cat_panel_list()`)
#'
#' @inheritParams eta_covariate
#'
#' @param x character `col//title` for the categorical covariates to
#' plot on x-axis; see [col_label()].
#' @param y character `col//title` for the continuous covariates to
#' plot on y-axis; see [col_label()].
#' @param transpose logical; if `TRUE`, output will be transposed to
#' group plots by the categorical covariates rather than the continuous
#' covariates.
#' @param ... additional arguments passed to [cont_cat()].
#'
#' @details
#' Pass `ncol = NULL` or another non-numeric value to bypass arranging plots
#' coming from `cont_cat_panel()`.
#'
#' @examples
#' data <- pmplots_data_id()
#' cont <- c("WT//Weight (kg)", "ALB//Albumin (mg/dL)", "AGE//Age (years)")
#' cats <- c("RF//Renal function", "CPc//Child-Pugh")
#'
#' cont_cat_panel(data, x = cats, y = cont, tag_levels = "A")
#'
#' cont_cat_panel(data, cats, cont)
#' cont_cat_panel(data, cats, cont, transpose = TRUE)
#'
#' l <- cont_cat_panel_list(data, cats, cont, transpose = TRUE)
#' names(l)
#' with(l$RF, WT/(ALB + AGE), tag_levels = "A")
#'
#' @return
#' `cont_cat_panel()` returns a list of plots arranged in graphics as a
#' `patchwork` object using [pm_grid()]. `cont_cat_panel_list()` returns the
#' same plots, but unarranged as a named list of lists.
#'
#' When `transpose` is `FALSE` (default), plots in a single graphic are grouped
#' by the continuous covariates (passed as `y`), and the names of the list
#' reflect those names (e.g., `WT`). When `transpose` is `TRUE`, the graphics
#' are grouped by the categorical covariates (passed as `x`) and the names of
#' the list reflect those names (e.g. `RF`). See **Examples**.
#'
#' @seealso [eta_covariate()], [eta_covariate_list()]
#' @md
#' @export
cont_cat_panel <- function(df, x, y, ncol = 2, tag_levels = NULL,
                            byrow = FALSE, transpose = FALSE, ...) {
  require_patchwork()
  p <- cont_cat_panel_list(df, x, y, transpose, ...)
  if(is.numeric(ncol)) {
    p <- lapply(p, pm_grid, ncol = ncol, byrow = byrow)
  }
  if(!is.null(tag_levels)) {
    p <- lapply(p, function(x) x + patchwork::plot_annotation(tag_levels = tag_levels))
  }
  p
}

#' @rdname cont_cat_panel
#' @export
cont_cat_panel_list <- function(df, x, y, transpose = FALSE, ...) {
  p <- list_plot_y(df, x, y, .fun = cont_cat, ...)
  labx <- col_label_col(x)
  laby <- col_label_col(y)
  names(p) <- laby
  p <- lapply(p, setNames, labx)
  if(isTRUE(transpose)) {
    p <- list_transpose(p)
  }
  p <- lapply(p, class_pm_display)
  p
}

#' with method for pm_display objects
#'
#' @param data a `pm_display` object.
#' @param expr a `patchwork` formula for arranging plots in `data`.
#' @param tag_levels passed to [patchwork::plot_annotation()].
#' @param ... not used.
#' @export
with.pm_display <- function(data, expr, tag_levels = NULL, ...) {
  require_patchwork()
  expr <- enexpr(expr)
  p <- eval(expr, envir = data)
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  p
}
