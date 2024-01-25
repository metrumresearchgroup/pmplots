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

#' Plot ETAs versus covariates
#'
#' @param df a data frame to plot.
#' @param x character `col//title` for covariates to plot on x-axis;
#' see [col_label].
#' @param y character `col//title` for ETAs to plot on y-axis; see [col_label].
#' @param ncol passed to [pm_grid()].
#' @param byrow passed through [pm_grid()].
#' @param tag_levels passed to [patchwork::plot_annotation()].
#'
#' @examples
#' data <- pmplots_data_id()
#' etas <- c("ETA1//ETA-CL", "ETA2//ETA-V")
#' cont <- c("WT//Weight (kg)", "ALB//Albumin (mg/dL)")
#' cat <- c("RF//Renal function", "CPc//Child-Pugh")
#' eta_covariate(data, x = c(cont, cat), y = etas, tag_levels = "A")
#'
#'
#' @seealso [npde_covariate()], [cwres_covariate()]
#' @md
#' @export
eta_covariate <- function(df, x, y, ncol = 2, tag_levels = NULL, byrow = NULL) {
  require_patchwork()
  p <- eta_covariate_list(df, x, y)
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
eta_covariate_list <- function(df, x, y) {
  p  <- diagnostic_display_list(df, x, y, eta_cat, eta_cont)
  labx <- lapply(x, col_label)
  labx <- sapply(labx, "[[", 1)
  laby <- lapply(y, col_label)
  laby <- sapply(laby, "[[", 1)
  p  <- lapply(p, setNames, nm = labx)
  names(p) <- laby
  p <- lapply(p, class_pm_display)
  p
}

#' Plot NPDE versus covariates
#'
#' @inheritParams eta_covariate
#'
#' @seealso [cwres_covariate()], [eta_covariate()]
#' @md
#' @export
npde_covariate <- function(df, x, ncol = 2, tag_levels = NULL, byrow = NULL) {
  require_patchwork()
  p <- npde_covariate_list(df, x)
  if(is.numeric(ncol)) {
    p <- pm_grid(p, ncol = ncol, byrow = byrow)
  }
  if(!is.null(tag_levels)) {
    p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  }
  p
}

#' @rdname npde_covariate
#' @export
npde_covariate_list <- function(df, x) {
  p  <- diagnostic_display_list(df, x, pm_axis_npde(), npde_cat, npde_cont)
  p <- p[[1]]
  labx <- lapply(x, col_label)
  labx <- sapply(labx, "[[", 1)
  names(p) <- labx
  p <- lapply(p, class_pm_display)
  p
}

#' Plot CWRES versus covariates
#'
#' @inheritParams eta_covariate
#'
#' @seealso [npde_covariate()], [eta_covariate()]
#' @md
#' @export
cwres_covariate <- function(df, x, ncol = 2, tag_levels = NULL) {
  require_patchwork()
  p <- cwres_covariate_list(df, x)
  if(is.numeric(ncol)) {
    p <- pm_grid(p, ncol = ncol)
  }
  if(!is.null(tag_levels)) {
    p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  }
  p
}

#' @rdname cwres_covariate
#' @export
cwres_covariate_list <- function(df, x) {
  p  <- diagnostic_display_list(df, x, pm_axis_npde(), cwres_cat, cwres_cont)
  p <- p[[1]]
  labx <- lapply(x, col_label)
  labx <- sapply(labx, "[[", 1)
  names(p) <- labx
  p
}

#' Create a panel of NPDE diagnostic plots
#'
#' @inheritParams eta_covariate
#'
#' @param unit_tad passed through [npde_time()] as `xunit`.
#' @param unit_time passed through [npde_tad()] as `xunit`.
#' @param xname passed through [npde_pred()].
#' @param xby_time passed to [npde_time()] as `xby`.
#' @param xby_tad passed to [npde_tad()] as `xby`.
#'
#' @seealso [cwres_panel()]
#' @md
#' @export
npde_panel <- function(df, ncol = 2, xname = "value",
                       unit_time = "hours", unit_tad = "hours",
                       xby_time  = NULL, xby_tad = NULL,
                       tag_levels = NULL) {
  require_patchwork()
  l <- npde_panel_list(df, xname, unit_time, unit_tad, xby_time, xby_tad)
  time <- l$time
  if(!is.null(l$tad)) {
    time <- time + l$tad
  }
  c <- l$pred
  d <- l$hist
  e <- l$q
  p <- time/l$pred/(l$hist+l$q)
  if(!is.null(tag_levels)) {
    p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  }
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

#' Plot a panel of CWRES diagnostic plots
#'
#' @inheritParams npde_panel
#'
#' @seealso [npde_panel()]
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
  if(!is.null(tag_levels)) {
    p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  }
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

#' with method for pm_display objects
#'
#' @param tag_levels passed to [patchwork::plot_annotation()].
#' @export
with.pm_display <- function(data, expr, tag_levels = NULL, ...) {
  expr <- enexpr(expr)
  p <- eval(expr, envir = data)
  if(!is.null(tag_levels)) {
    p <- p + patchwork::plot_annotation(tag_levels = tag_levels)
  }
  p
}
