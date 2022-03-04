
#' Faceted plots
#'
#' For these plots, data sets made long with respect to several
#' y-axis variables and then plotted and faceted with
#' [ggplot2::facet_wrap()].
#'
#' `wrap_cont_cont` is a general function used by
#' the others to create faceted plots of two continuous variables.
#' `wrap_cont_time` plots several continuous variables versus time.
#' `wrap_res_time` plots several different residuals (or NPDE) versus time.
#' `wrap_eta_cont` plots etas versus a continuous covariate. `wrap_hist`
#' creates a faceted histogram plot. `wrap_cont_cat` plots continuous versus
#' categorical data as a boxplot.
#'
#' @param df Data frame to plot.
#' @param x x-axis data in [col_label()] format; if `y` has length greater than 1,
#' then `x` must be length equal to 1.
#' @param y y-axis data in [col_label()] format; if `x` has length greater than 1,
#' then `y` must be length equal to 1.
#' @param ... Passed to `fun`.
#' @param fun The plotting function.
#' @param title A title to use for the axis with faceting groups.
#' @param scales Passed to `facet_wrap`.
#' @param ncol Passed to `facet_wrap`.
#' @param use_labels If `TRUE`, the label part of `col_label` will
#' be used in the strip; the column name is used otherwise.
#' @param labeller A labeller function; passed to [ggplot2::facet_wrap()]; the
#' default is based on [parse_label()] and allows latex markup in the label.
#' @param label_fun Deprecated; use `labeller` instead.
#' @param xname Placeholder.
#'
#' @details
#'
#' The following functions are called (as `fun`) to make each wrapped plot
#'
#' - `wrap_cont_cont` calls [pm_scatter()]
#' - `wrap_cont_time` calls [y_time()]
#' - `wrap_res_time` calls [res_time()]
#' - `wrap_eta_cont` calls [eta_cont()]
#' - `wrap_hist` calls [cont_hist()]
#' - `wrap_cont_cat` calls [pm_box()];
#'
#' For all plots, both `x` and `y` should name numeric data columns with the
#' exception of `wrap_cont_cat` which expects `x` to name a categorical
#' data column (the data are sent to [pm_box()]).
#'
#' When [pm_box()] is called by `wrap_cont_cat`, the `shown` argument will be
#' forced to the value `FALSE`.
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
                           label_fun = deprecated(),
                           labeller = label_tex) {

  if(is_present(label_fun)) {
    deprecate_warn(
      "0.3.4",
      "wrap_cont_cont(label_fun = )",
      "wrap_cont_cont(labeller = )"
    )
    labeller <- label_fun
  }

  multi_x <- length(x) > 1
  multi_y <- length(y) > 1

  if(multi_x && multi_y) {
    stop("Either x or y may have length > 1, not both.", call. = FALSE)
  }

  if(multi_x) {
    y <- y[1]
    to_melt <- col_labels(x)
    df <- pivot_longer(
      df,
      cols = all_of(unname(to_melt)),
      names_to = "variable",
      values_to = "value"
    )
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
    df <- pivot_longer(
      df,
      cols = all_of(unname(to_melt)),
      names_to =  "variable",
      values_to = "value"
    )
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
    facet_wrap(~variable, scales = scales, ncol = ncol, labeller = labeller)
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
wrap_eta_cont <- function(df, x, y, scales = "fixed", ...) {
  wrap_cont_cont(df, x, y = y, fun = eta_cont, scales = scales, ...)
}

#' @rdname wrap_plots
#' @export
wrap_hist <- function(df, x, title = NULL, scales = "free_x", ncol = NULL,
                      use_labels = FALSE, labeller = label_tex,
                      label_fun = deprecated(), ...) {

  if(is_present(label_fun)) {
    deprecate_warn(
      "0.3.4",
      "wrap_hist(label_fun = )",
      "wrap_hist(labeller = )"
    )
    labeller <- label_fun
  }

  x <- col_labels(x)

  df <- pivot_longer(
    df,
    cols = all_of(unname(x)),
    names_to =  "variable",
    values_to = "value"
  )
  df <- mutate(df, variable = fct_inorder(.data[["variable"]]))

  if(use_labels) {
    df <- mutate(df, variable = factor(.data[["variable"]], labels=names(x)))
  }
  if(is.null(title)) {
    x <- "value"
  } else {
    x <- paste0("value//", title)
  }
  cont_hist(df, x = x, ...) +
    facet_wrap(~variable, scales = scales, ncol=ncol, labeller = labeller)
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

#' @rdname wrap_plots
#' @name wrap_plots
#' @export
wrap_cont_cat <- function(df, x, y, ...,
                          title = NULL, scales = "free_y",
                          ncol = NULL, use_labels = FALSE,
                          labeller = label_tex, label_fun = deprecated()) {

  if(is_present(label_fun)) {
    deprecate_warn(
      "0.3.4",
      "wrap_cont_cat(label_fun = )",
      "wrap_cont_cat(labeller = )"
    )
    labeller <- label_fun
  }

  multi_x <- length(x) > 1

  if(multi_x) {
    ans <- lapply(
      x,
      FUN = wrap_cont_cat,
      df = df,
      y = y, ...,
      title = title,
      scales = scales,
      ncol = ncol,
      use_labels = TRUE,
      label_fun = labeller
    )
    return(ans)
  }

  x <- x[1]
  to_melt <- col_labels(y)
  df <- pivot_longer(
    df,
    cols = unname(to_melt),
    names_to =  "variable",
    values_to = "value"
  )
  df <- mutate(df, variable = fct_inorder(.data[["variable"]]))
  if(use_labels) {
    df <- mutate(df, variable = factor(.data[["variable"]], labels = names(to_melt)))
  }
  if(!is.null(title)) {
    y <- paste0("value//", title)
  } else {
    y <- "value"
  }

  pm_box(df, x = x, y = y, ..., shown = FALSE) +
    facet_wrap(~variable, scales = scales, ncol = ncol, labeller = labeller)
}
