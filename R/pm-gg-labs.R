validate_label_list <- function(x, name) {
  assert_that(is.list(x), msg = paste0("`", name, "` must be a list."))
  assert_that(is_named(x), msg = paste0("`", name, "` must be a named list."))
  ok <- vapply(x, function(v) is.character(v) && length(v) == 1L, logical(1))
  if(any(!ok)) {
    bad <- paste(names(x)[!ok], collapse = ", ")
    abort(paste0("Every element of `", name, "` must be a character string of length 1. Problem: ", bad))
  }
  invisible(x)
}

gg_get_labs_2 <- function(p) {
  defaults <- ggplot2::labs()  # named list of NULLs for all standard slots
  modifyList(defaults, p$labels)
}

resolve_axis_label <- function(label, envir, default) {
  if(inherits(label, "AsIs")) return(as.character(label))
  res <- envir[[label]] %||% default
  if(is.null(res)) return(label)
  return(res)
}

#' Label pmplot aesthetics from a yspec object or named list
#'
#' This function generates `x` and `y` axis titles based on the data columns
#' used to create the plot, looking up in a named list or `yspec` object. This
#' is only for pmplot outputs; consider [yspec::ys_gg_labs()] for labeling
#' aesthetics in an arbitrary `gg` object.
#'
#' @param spec a named list of label data; names correspond to columns
#' in the data used to make the plot; may also be a `yspec` object, which
#' will be converted to a named list through [ys_get_short_unit()].
#' @param labs another named list of label data to override names found in
#' `spec`.
#' @param x label for the x aesthetic; if `NULL`, resolved via the mapped
#' column name. Pass a column name as a plain string to look it up in `spec`
#' or `labs`; wrap in [I()] to use the string as a literal label.
#' @param y label for the y aesthetic; see `x`.
#' @param short_max passed to [ys_get_short_unit()] when `spec` is a `yspec`
#' object.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A `pm_gg_labs` object that can be added to a pmplots gg object
#' with `+`.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(PRED = "Population predicted CX1123 (ng/mL)",
#'              DV = "Observed CX1123 (ng/mL)")
#'
#' p <- dv_pred(data) + pm_gg_labs(spec)
#'
#' p
#'
#' @md
#' @export
pm_gg_labs <- function(spec = list(), labs = list(),
                       x = NULL, y = NULL,
                       short_max = Inf,
                       ...) {
  envir <- list()
  if(inherits(spec, "yspec")) {
    if(!requireNamespace("yspec")) {
      abort("the yspec package must be installed to use a yspec object for labels.")
    }
    spec <- yspec::ys_get_short_unit(spec, short_max = short_max)
  }
  if(length(spec)) {
    validate_label_list(spec, "spec")
    envir <- spec
  }
  if(length(labs)) {
    validate_label_list(labs, "labs")
    envir <- c(labs, envir)
  }
  envir <- envir[!duplicated(names(envir))]
  structure(
    list(
      envir = envir,
      x = x,
      y = y,
      extra = list(...)
    ),
    class = "pm_gg_labs"
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.pm_gg_labs <- function(object, p, object_name) {
  assert_that(isTRUE(p$pmp.pmplot), msg = "pm_gg_labs() can only be used with plots created by pmplots.")
  existing <- gg_get_labs_2(p)
  args <- list()
  args$x <- resolve_axis_label(object$x %||% p$pmp.x, object$envir, existing$x)
  args$y <- resolve_axis_label(object$y %||% p$pmp.y, object$envir, existing$y)
  p + do.call(ggplot2::labs, c(args, object$extra))
}

#' Relabel a pmplots plot using a yspec object or named list
#'
#' This function relabels `x` and `y` axis titles based on the data columns
#' used to create the plot, looking up in a named list or `yspec` object. This
#' is only for pmplot outputs; consider [yspec::ys_gg_labs()] for labeling
#' aesthetics in an arbitrary `gg` object.
#'
#' @inheritParams pm_gg_labs
#' @param x a gg object created through a `pmplots` function.
#' @param ... additional arguments passed to [pm_gg_labs()].
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' p <- dv_pred(data)
#' p
#'
#' spec <- list(DV = "CX1123 concentration (ng/mL)")
#'
#' p <- pm_relabel(p, spec)
#' p
#'
#' @seealso [pm_gg_labs()]
#' @export
pm_relabel <- function(x, ...) UseMethod("pm_relabel")

#' @rdname pm_relabel
#' @export
pm_relabel.gg <- function(x, spec, labs = list(), ...) {
  assert_that(isTRUE(x$pmp.pmplot), msg = "pm_relabel() can only be used with plots created by pmplots.")
  x + pm_gg_labs(spec, labs, ...)
}

#' @rdname pm_relabel
#' @export
pm_relabel.list <- function(x, spec, labs = list(), ...) {
  lapply(x, pm_relabel, spec = spec, labs = labs, ...)
}

#' Add axis label data to a data frame
#'
#' This function adds candidate axis titles as an attribute on columns in
#' `data`. This attribute is intended to be specifically used for pmplot axis
#' labels, not to be confused with the label added by [yspec::ys_add_labels()].
#'
#' @return The data frame `df` with `pmp.axis.label` attributes set on the
#' labeled columns. If no columns in `spec` or `labs` match column names in
#' `df`, a warning is issued and `df` is returned unchanged.
#'
#' @seealso [pm_label_rm()]
#' @inheritParams pm_gg_labs
#' @param df a data frame to label.
#' @export
pm_label_columns <- function(df, spec, labs = list(), short_max = Inf) {
  assert_that(inherits(df,"data.frame"))
  envir <- list()
  if(inherits(spec, "yspec")) {
    if(!requireNamespace("yspec")) {
      abort("the yspec package must be installed to use a yspec object for labels.")
    }
    spec <- yspec::ys_get_short_unit(spec, short_max = short_max)
  }
  if(length(spec)) {
    validate_label_list(spec, "spec")
    envir <- spec
  }
  if(length(labs)) {
    validate_label_list(labs, "labs")
    envir <- c(labs, envir)
  }
  envir <- envir[!duplicated(names(envir))]
  envir <- envir[names(envir) %in% names(df)]
  if(!length(envir)) {
    warn("No columns were labeled.")
    return(df)
  }
  for(col in names(envir)) {
    attr(df[[col]], "pmp.axis.label") <- envir[[col]]
  }
  df
}

#' Remove pmplot-specific axis label information
#'
#' @param df a data frame to de-label.
#'
#' @return The data frame `df` with all `pmp.axis.label` attributes removed.
#'
#' @seealso [pm_label_columns()]
#' @export
pm_label_rm <- function(df) {
  df[] <- lapply(df, function(col) {
    attr(col, "pmp.axis.label") <- NULL
    col
  })
  df
}

pm_save_xy <- function(p, data, x = NULL, y = NULL) {
  p$pmp.pmplot <- TRUE
  p$pmp.x <- x
  p$pmp.y <- y
  if(is.character(x)) {
    p$pmp.data.axis.x <- attr(data[[x]], "pmp.axis.label")
  } else {
    p$pmp.data.axis.x <- NULL
  }
  if(is.character(y)) {
    p$pmp.data.axis.y <- attr(data[[y]], "pmp.axis.label")
  } else {
    p$pmp.data.axis.y <- NULL
  }
  p
}

pm_get_data_x <- function(p) {
  assert_that(isTRUE(p$pmp.pmplot), msg = "plot is not recognized as pmplot.")
  p$pmp.data.axis.x
}

pm_get_data_y <- function(p) {
  assert_that(isTRUE(p$pmp.pmplot), msg = "plot is not recognized as pmplot.")
  p$pmp.data.axis.y
}
