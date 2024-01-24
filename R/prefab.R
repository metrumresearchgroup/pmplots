

diagnostic_display_plots <- function(data, x, y, ncol, fun_cat, fun_cont, tag) {
  out <- vector(mode = "list", length = length(y))
  tag <- isTRUE(tag)
  for(i in seq_along(y)) {
    panel <- lapply(seq_along(x), function(ii) {
      col <- col_label(x[[ii]])[[1]]
      require_column(data, col)
      if(inherits(unlist(data[, col]), c("character", "factor", "logical", "integer"))) {
        p <- fun_cat(data, x = x[ii], y = y[i])
      } else {
        p <- fun_cont(data, x = x[ii], y = y[i])
      }
    })
    out[[i]] <- pm_grid(panel, ncol = ncol)
    if(isTRUE(tag)) {
      out[[i]] <- out[[i]] + patchwork::plot_annotation(tag_levels = 'A')
    }
    out
  }
  out
}

#' Plot ETAs versus covariates
#'
#' @md
#' @export
eta_covariate <- function(data, x, etas, ncol = 2, tag = FALSE) {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Must have the patchwork package installed to run `eta_covariate()`",
      call.=FALSE
    )
  }
  diagnostic_display(data, x, etas, ncol, eta_cat, eta_cont, tag = tag)
}

#' Plot NPDE versus covariates
#'
#' @md
#' @export
npde_covariate <- function(data, x, ncol = 2, tag = FALSE) {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Must have the patchwork package installed to run `npde_covariate()`",
      call.=FALSE
    )
  }
  diagnostic_display(data, x, y = pm_axis_npde(), ncol = ncol,
                     fun_cat = npde_cat, fun_cont = npde_cont, tag = tag)[[1]]
}

#' Plot CWRES versus covariates
#'
#' @md
#' @export
cwres_covariate <- function(data, x, ncol = 2, tag = FALSE) {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Must have the patchwork package installed to run `cwres_covariate()`",
      call.=FALSE
    )
  }
  diagnostic_display(data, x, y = pm_axis_cwres(), ncol = ncol,
                     fun_cat = cwres_cat, fun_cont = cwres_cont, tag = tag)[[1]]
}

#' Create a panel of NPDE diagnostic plots
#'
#' @md
#' @export
npde_panel <- function(data, ncol = 2, time_unit = "days",xname = "value", tag = FALSE) {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Must have the patchwork package installed to run `npde_panel()`",
      call.=FALSE
    )
  }
  l <- npde_panel_list(data, time_unit, xname)
  a <- l$time
  if(!is.null(l$tad)) {
    a <- a + l$tad
  }
  c <- l$pred
  d <- l$hist
  e <- l$q
  p <- a/c/(d+e)
  if(isTRUE(tag)) p <- p + patchwork::plot_annotation(tag_levels = 'A')
  p
}

#' @rdname npde_panel
#' @export
npde_panel_list <- function(data, time_unit = "days", xname = "value") {
    a <- npde_time(data, xunit = time_unit)
    b <- NULL
    if("TAD" %in% names(data)) {
      b <- npde_tad(data, xunit = time_unit)
    }
    c <- npde_pred(data, xname = xname)
    d <- npde_hist(data)
    e <- npde_q(data)
    list(time = a, tad = b, hist = d, q = e, pred = c)
}

#' Plot a panel of CWRES diagnostic plots
#'
#' @md
#' @export
cwres_panel <- function(data, time_unit = "days", xname = "value", tag = FALSE) {
  if(!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Must have the patchwork package installed to run `cwres_panel()`",
      call.=FALSE
    )
  }
  l <- cwres_panel_list(data, time_unit, xname)
  a <- l$time
  if(!is.null(l$tad)) {
    a <- a + l$tad
  }
  c <- l$pred
  d <- l$hist
  e <- l$q
  p <- a/c/(d+e)
  if(isTRUE(tag)) p <- p + patchwork::plot_annotation(tag_levels = 'A')
  p
}

#' @rdname cwres_panel
#' @export
cwres_panel_list <- function(data, time_unit = "days", xname = "value") {
  a <- cwres_time(data, xunit = time_unit)
  b <- NULL
  if("TAD" %in% names(data)) {
    b <- cwres_tad(data, xunit = time_unit)
  }
  c <- cwres_pred(data, xname = xname)
  d <- cwres_hist(data)
  e <- cwres_q(data)
  list(time = a, tad = b, hist = d, q = e, pred = c)
}
