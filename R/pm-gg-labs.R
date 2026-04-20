
resolve_axis_label <- function(label, envir, default) {
  if(inherits(label, "AsIs")) return(label)
  res <- envir[[label]] %||% default
  if(is.null(res)) return(label)
  return(res)
}

#'
#' @md
#' @export
pm_lab_xy <- function(p, envir) {
  x <- p$pmp.x
  y <- p$pmp.y
  p + ggplot2::labs(
    x = resolve_axis_label(x, envir, x),
    y = resolve_axis_label(y, envir, y)
  )
}


#' Automatically label ggplot aesthetics from a yspec object or other source
#'
#' @param labs a named list of label data; names correspond to columns
#' in the data used to make the plot; overrides `spec`.
#' @param x label for the x aesthetic. If `NULL`, resolved via the mapped
#' column name. Pass a column name as a plain string to look it up in `spec` or
#' `labs`; wrap in [I()] to use the string as a literal label.
#' @param y label for the y aesthetic; see `x`.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#'
#' @md
#' @export
pm_gg_labs <- function(labs = list(),
                       x = NULL, y = NULL,
                       warn = TRUE,
                       short_max = Inf,
                       ...) {
  envir <- list()
  if(inherits(labs, "yspec")) {
    if(!requireNamespace("yspec")) {
      abort("the yspec package must be installed to use a yspec object for labels.")
    }
    labs <- yspec::ys_get_short_unit(labs, short_max = short_max)
  }
  if(length(labs)) {
    assert_that(is.list(labs))
    assert_that(is_named(labs))
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

  x <- p$pmp.x
  y <- p$pmp.y

  args <- list()
  args$x <- resolve_axis_label(x, object$envir, x)
  args$y <- resolve_axis_label(y, object$envir, y)
  p + do.call(ggplot2::labs, c(args, object$extra))
}

#'
#' @export
pm_relabel <- function(x, ...) UseMethod("pm_relabel")

#' @export
pm_relabel.gg <- function(x, labs, ...) {
  x + pm_gg_labs(labs)
}

#' @export
pm_relabel.list <- function(x, labs, ...) {
  lapply(x, pm_relabel, labs = labs, ...)
}

#' @export
pm_add_labels <- function(data, spec) {
  assert_that(inherits(data,"data.frame"))
  if(inherits(spec, "yspec")) {
    stopifnot(requireNamespace("yspec"))
    spec <- ys_get_short_unit(spec)
  }
  col_labels <- spec
  col_labels <- col_labels[names(col_labels) %in% names(data)]
  if(!length(col_labels)) {
    warn("No columns were labeled.")
    return(data)
  }
  for(col in names(col_labels)) {
    attr(data[[col]], "pmp.axis.label") <- col_labels[[col]]
  }
  data
}

#' @export
pm_rm_labels <- function(data) {
  data[] <- lapply(data, function(col) {
    attr(col, "pmp.axis.label") <- NULL
    col
  })
  data
}

pm_save_xy <- function(p, data, x = NULL, y = NULL) {
  p$pmp.x <- x
  p$pmp.y <- y
  if(is.character(x)) {
    p$pmp.data.x <- attr(data[[x]], "pmp.axis.label")
  } else {
    p$pmp.data.x <- NULL
  }
  if(is.character(y)) {
    p$pmp.data.y <- attr(data[[y]], "pmp.axis.label")
  } else {
    p$pmp.data.y <- NULL
  }
  p
}
