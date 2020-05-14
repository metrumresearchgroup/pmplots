
#' Faceted plots
#'
#' For these plots, data sets made long with respect to several
#' y-axis variables and then plotted and faceted with
#' [ggplot2::facet_wrap].
#'
#'
#' @param df data frame to plot
#' @param x x-axis data in [col_label] format; if `y` has length greater than 1,
#' then `x` must be length equal to 1
#' @param y y-axis data in [col_label] format; if `x` has length greater than 1,
#' then `y` must be length equal to 1
#' @param ... passed to `fun`
#' @param fun the plotting function
#' @param title a title to use for the axis with faceting groups
#' @param scales passed to `facet_wrap`
#' @param ncol passed to `facet_wrap`
#' @param use_labels if `TRUE`, the label part of `col_label` will
#' be used in the strip; the column name is used otherwise
#' @param label_fun labeller function; passed to [ggplot2::facet_wrap]; the
#' default is based on [parse_label] and allows latex markup in the label
#' @param xname placeholder
#'
#' @details
#'
#' `wrap_cont_cont` is a general function used by the others to
#' create faceted plots of two continuous variables.  `wrap_cont_time`
#' plots several continuous variables versus time.  `wrap_res_time`
#' plots several different residuals (or NPDE) versus time. `wrap_eta_cont`
#' plots etas versus a continuous covariate. `wrap_hist` creates a faceted
#' histogram plot.
#'
#' For all plots, either `x` or `y` may contain multiple columns, but an error
#' will be generated if both `x` and `y` list multiple columns.
#'
#' @md
#' @rdname wrap_plots
#' @name wrap_plots
#' @export
wrap_cont_cont <- function(df, x, y, ..., fun = pm_scatter,
                           title = NULL, scales = "free_y",
                           ncol = NULL, use_labels = FALSE,
                           label_fun = label_parse_label) {
  multi_x <- length(x) > 1
  multi_y <- length(y) > 1

  if(multi_x && multi_y) {
    stop("either x or y may have length > 1, not both.",call.=FALSE)
  }


  if(multi_x) {
    y <- y[1]
    to_melt <- col_labels(x)
    df <- pivot_longer(df, cols = to_melt, names_to = "variable", values_to = "value")
    df <- mutate(df, variable = fct_inorder(.data[["variable"]]))
    if(use_labels) {
      df <- mutate(df, variable = factor(.data[["variable"]], labels = names(to_melt)))
    }

    if(!is.null(title)) {
      x <- paste0("value//", title)
    } else {
      x <- "value"
    }
  }

  if(multi_y) {
    x <- x[1]
    to_melt <- col_labels(y)
    df <- pivot_longer(df, cols = to_melt,names_to =  "variable", values_to = "value")
    df <- mutate(df, variable = fct_inorder(.data[["variable"]]))
    if(use_labels) {
      df <- mutate(df, variable = factor(.data[["variable"]], labels = names(to_melt)))
    }
    if(!is.null(title)) {
      y <- paste0("value//", title)
    } else {
      y <- "value"
    }
  }

  fun(df, x = x, y = y, ...) +
    facet_wrap(~variable, scales = scales, ncol = ncol, labeller=label_fun)
}

#' @rdname wrap_plots
#' @export
wrap_cont_time <- function(df, ..., x = pm_axis_time()) {
  wrap_cont_cont(df, ..., x = pm_axis_time(), fun = y_time)
}

#' @rdname wrap_plots
#' @export
wrap_res_time <- function(df, ..., x = pm_axis_time()) {
  wrap_cont_cont(df, ..., x = x, fun = res_time)
}

#' @rdname wrap_plots
#' @export
wrap_eta_cont <- function(df, x, y, scales="fixed", ...) {
  wrap_cont_cont(df, x, y=y, fun = eta_cont, scales = scales, ...)
}

#' @rdname wrap_plots
#' @export
wrap_hist <- function(df, x, title =NULL, scales = "free_x", ncol=NULL,
                      use_labels=FALSE, label_fun = label_parse_label, ...) {
  x <- col_labels(x)
  df <- gather(df, "variable", "value", x, factor_key=TRUE)
  if(use_labels) {
    df <- mutate(df, variable = factor(.data[["variable"]], labels=names(x)))
  }
  if(is.null(title)) {
    x <- "value"
  } else {
    x <- paste0("value//", title)
  }
  cont_hist(df, x = x, ...) +
    facet_wrap(~variable, scales = scales, ncol=ncol, labeller=label_fun)
}

#' @rdname wrap_plots
#' @export
wrap_dv_preds <- function(df, ..., title = "Predicted {yname}", xname="", scales="fixed") {
  x <- c(pm_axis_pred(), pm_axis_ipred())
  for(i in seq_along(x)) x[i] <- glue(x[i])
  wrap_cont_cont(
    df, x = x, y = pm_axis_dv(), ...,
    fun = dv_pred, title = title, scales = scales
  )
}



