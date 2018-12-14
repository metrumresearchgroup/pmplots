
##' Faceted plots
##'
##' For these plots, data sets made long with respect to several
##' y-axis variables and then plotted and faceted with `facet_wrap`.
##'
##'
##' @param df data frame to plot
##' @param x x-axis data in [col_label] format
##' @param columns y-axis data in [col_label] format; expecting
##' a vector of `col_label` values
##' @param ... passed to `fun`
##' @param fun the plotting function
##' @param title a title to use for the axis with faceting groups
##' @param scales passed to `facet_wrap`
##' @param ncol passed to `facet_wrap`
##' @param use_labels if `TRUE`, the label part of `col_label` will
##' be used in the strip; the column name is used otherwise
##'
##' @details
##'
##' `wrap_cont_cont` is a general function used by the others to
##' create faceted plots of two continuous variables.  `wrap_cont_time`
##' plots several continuous variables versus time.  `wrap_res_time`
##' plots several different residuals (or NPDE) versus time. `wrap_eta_cont`
##' plots etas versus a continuous covariate. `wrap_hist` creates a faceted
##' histogram plot.
##'
##' @md
##' @rdname wrap_plots
##' @name wrap_plots
##' @export
wrap_cont_cont <- function(df, x, y, ..., fun=cont_cont,
                           title = NULL, scales = "free_y",
                           ncol = NULL, use_labels = FALSE) {
  multi_x <- FALSE

  if(length(x) > 1) {
    multi_x <- TRUE
    y <- y[1]
    to_melt <- col_labels(x)
    df <- gather(df, variable, value, to_melt, factor_key=TRUE)
    if(use_labels) {
      df <- mutate(df, variable = factor(variable, labels = names(to_melt)))
    }

    if(!is.null(title)) {
      x <- paste0("value", "//", title)
    } else {
      x <- "value"
    }
  }

  if(length(y) > 1) {
    if(multi_x) stop("Either columns or x must be length 1.", call.=FALSE)
    x <- x[1]
    to_melt <- col_labels(y)
    df <- gather(df, variable, value, to_melt, factor_key=TRUE)
    if(use_labels) {
      df <- mutate(df, variable = factor(variable, labels = names(to_melt)))
    }
    if(!is.null(title)) {
      y <- paste0("value", "//", title)
    } else {
      y <- "value"
    }
  }

  fun(df, x = x, y = y, ...) + facet_wrap(~variable, scales = scales, ncol = ncol)
}

##' @rdname wrap_plots
##' @export
wrap_cont_time <- function(df, ..., x = pm_axis_time()) {
  wrap_cont_cont(df, ..., x = pm_axis_time(), fun = y_time)
}

##' @rdname wrap_plots
##' @export
wrap_res_time <- function(df, ..., x = pm_axis_time()) {
  wrap_cont_cont(df, ..., x = x, fun = res_time)
}

##' @rdname wrap_plots
##' @export
wrap_eta_cont <- function(df, x, y, scales="fixed", ...) {
  wrap_cont_cont(df, x, y=y, fun = eta_cont, scales = scales, ...)
}

##' @rdname wrap_plots
##' @export
wrap_hist <- function(df, x, title =NULL, scales = "free_x", ncol=NULL, use_labels=FALSE, ...) {
  x <- col_labels(x)
  df <- gather(df, variable, value, x, factor_key=TRUE)
  if(use_labels) {
    df <- mutate(df, variable = factor(variable, labels=names(x)))
  }
  if(is.null(title)) {
    x <- "value"
  } else {
    x <- paste0("value//", title)
  }
  cont_hist(df, x = x, ...) + facet_wrap(~variable, scales = scales, ncol=ncol)
}

##' @rdname wrap_plots
##' @export
wrap_dv_preds <- function(df, ..., title = "Predicted {yname}", xname = "", scales="fixed") {
  x <- c(pm_axis_pred(), pm_axis_ipred())
  for(i in seq_along(x)) x[i] <- glue(x[i])
  wrap_cont_cont(
    df, x = x, y = pm_axis_dv(), ...,
    fun = dv_pred, title = title, scales = scales
  )
}



