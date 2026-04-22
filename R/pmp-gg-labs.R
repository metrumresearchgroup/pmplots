validate_label_list <- function(x, name) {
  assert_that(is.list(x), msg = paste0("`", name, "` must be a list."))
  assert_that(is_named(x), msg = paste0("`", name, "` must be a named list."))
  ok <- lengths(x)==1 & vapply(x, \(v) is.character(v), logical(1))
  if(any(!ok)) {
    bad <- paste(names(x)[!ok], collapse = ", ")
    abort(paste0("Every element of `", name, "` must be a character string of length 1. Problem: ", bad))
  }
  invisible(x)
}

gg_get_labs_2 <- function(p) {
  defaults <- ggplot2::labs()
  modifyList(defaults, p$labels)
}

resolve_axis_label <- function(label, envir, default) {
  if(is.null(label)) return(default)
  if(inherits(label, "AsIs")) return(as.character(label))
  res <- envir[[label]] %||% default
  if(is.null(res)) return(label)
  return(res)
}

#' Label pmplot aesthetics from a yspec object or named list
#'
#' This function generates `x` and `y` axis titles based on the data columns
#' used to create the plot, looking up in a named list or `yspec` object. This
#' is only for pmplot outputs; consider [pm_gg_labs()] for labeling aesthetics
#' in an arbitrary `gg` object.
#'
#' @param spec a named list of label data; names correspond to columns
#' in the data used to make the plot; may also be a `yspec` object, which
#' will be converted to a named list through [yspec::ys_get_short_unit()].
#' @param labs another named list of label data to override names found in
#' `spec`.
#' @param x label for the x aesthetic; if `NULL`, resolved via the mapped
#' column name. Pass a column name as a plain string to look it up in `spec`
#' or `labs`; wrap in [I()] to use the string as a literal label.
#' @param y label for the y aesthetic; see `x`.
#' @param short_max passed to [yspec::ys_get_short_unit()] when `spec` is a
#' `yspec` object.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A `pmp_gg_labs` object that can be added to a pmplots gg object
#' with `+`.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(PRED = "Population predicted CX1123 (ng/mL)",
#'              DV = "Observed CX1123 (ng/mL)")
#'
#' p <- dv_pred(data) + pmp_gg_labs(spec)
#'
#' p
#'
#' @seealso [pmp_relabel()], [pmp_relabel_wrap()], [pmp_relabel_pairs()]
#' @md
#' @export
pmp_gg_labs <- function(spec = list(), labs = list(),
                       x = NULL, y = NULL,
                       short_max = Inf,
                       ...) {
  envir <- list()
  if(inherits(spec, "yspec")) {
    require_yspec()
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
    class = "pmp_gg_labs"
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.pmp_gg_labs <- function(object, p, object_name) {
  assert_that(
    isTRUE(p$pmp.pmplot),
    msg = "pmp_gg_labs() can only be used with plots created by pmplots."
  )
  existing <- gg_get_labs_2(p)
  args <- list()
  x <- object$x %||% p$pmp.x
  y <- object$y %||% p$pmp.y
  args$x <- resolve_axis_label(x, object$envir, existing$x)
  args$y <- resolve_axis_label(y, object$envir, existing$y)
  p + do.call(ggplot2::labs, c(args, object$extra))
}

#' Relabel a pmplots plot using a yspec object or named list
#'
#' This function relabels `x` and `y` axis titles based on the data columns
#' used to create the plot, looking up in a named list or `yspec` object. This
#' is only for pmplot outputs; consider [yspec::ys_gg_labs()] for labeling
#' aesthetics in an arbitrary `gg` object.
#'
#' @inheritParams pmp_gg_labs
#' @param obj a gg object created through a `pmplots` function or a 
#' list of such objects.
#' @param ... additional arguments passed to [pmp_gg_labs()].
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' p <- dv_pred(data)
#' p
#'
#' spec <- list(DV = "CX1123 concentration (ng/mL)")
#'
#' p <- pmp_relabel(p, spec)
#' p
#'
#' @seealso [pmp_gg_labs()], [pmp_relabel_wrap()], [pmp_relabel_pairs()]
#' @export
pmp_relabel <- function(obj, ...) UseMethod("pmp_relabel")

#' @rdname pmp_relabel
#' @export
pmp_relabel.gg <- function(obj, spec = list(), labs = list(), ...) {
  assert_that(
    isTRUE(obj$pmp.pmplot),
    msg = "pmp_relabel() can only be used with plots created by pmplots."
  )
  obj + pmp_gg_labs(spec, labs, ...)
}

#' @rdname pmp_relabel
#' @export
pmp_relabel.list <- function(obj, spec = list(), labs = list(), ...) {
  lapply(obj, pmp_relabel, spec = spec, labs = labs, ...)
}

#' Relabel facet strips in a wrapped pmplot
#'
#' This function relabels the facet strip labels in a plot created by a
#' `wrap_*` function (e.g., [wrap_eta_cont()], [wrap_cont_cont()]) by looking
#' up the facet variable names in a named list or `yspec` object. Variable
#' names are discovered automatically from the plot data. Names absent from
#' `spec` and `labs` are left unchanged.
#'
#' @inheritParams pmp_gg_labs
#' @param p a ggplot object created by a `wrap_*` pmplots function.
#'
#' @return The plot `p` with updated facet strip labels.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(WT = "Weight (kg)", ALB = "Albumin (mg/dL)")
#'
#' p <- wrap_eta_cont(data, x = c("WT", "ALB"), y = "ETA1//ETA1", scales = "free_x")
#'
#' pmp_relabel_wrap(p, spec)
#'
#' @seealso [pmp_relabel()], [pmp_relabel_pairs()], [pmp_gg_labs()]
#' @export
pmp_relabel_wrap <- function(p, spec, labs = list(), short_max = Inf) {
  assert_that(
    isTRUE(p$pmp.pmplots.wrap),
    msg = "pmp_relabel_wrap() can only be used with wrapped pmplots (e.g., from wrap_eta_cont())."
  )
  envir <- list()
  if(inherits(spec, "yspec")) {
    require_yspec()
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

  var_names <- p$pmp.pmplots.wrap.varnames

  label_map <- setNames(
    vapply(var_names, function(v) envir[[v]] %||% v, character(1)),
    var_names
  )

  labelfun <- function(v) lapply(v, function(x) parse_label(label_map[[x]]))

  p + facet_wrap(
    reformulate(p$pmp.pmplots.wrap.facets),
    labeller = as_labeller(labelfun),
    scales   = p$pmp.pmplots.wrap.scales,
    ncol     = p$pmp.pmplots.wrap.ncol
  )
}

#' Relabel facets in a pmplots pairs plot
#'
#' This function relabels the axis strip labels in a pairs plot created by
#' [eta_pairs()] or [pairs_plot()] by looking up the variable names in a named
#' list or `yspec` object. Variable names are discovered automatically from the
#' plot object. Names absent from `spec` and `labs` are left unchanged.
#'
#' @inheritParams pmp_gg_labs
#' @param p a pairs plot object created by [eta_pairs()] or [pairs_plot()].
#' @param unit_break if `TRUE` (the default), a newline is inserted between the
#'   label text and a trailing parenthetical unit (e.g., `"Weight (kg)"` becomes
#'   `"Weight\n(kg)"`).
#' @param ... currently not used.
#'
#' @return The pairs plot `p` with updated facet labels.
#'
#' @examples
#' id <- pmplots_data_id()
#'
#' etas <- c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA")
#'
#' spec <- list(ETA1 = "ETA on CL (L/h)", ETA2 = "ETA on Vc (L)")
#'
#' p <- eta_pairs(id, etas)
#'
#' pmp_relabel_pairs(p, spec)
#'
#' @seealso [pmp_relabel()], [pmp_relabel_wrap()], [pmp_gg_labs()]
#' @md
#' @export
pmp_relabel_pairs <- function(p, spec, labs = list(), short_max = Inf, unit_break = TRUE, ...) {
  assert_that(
    isTRUE(p$pmp.pmplot.pairs),
    msg = "pmp_relabel_pairs() can only be used with pm pairs plots."
  )
  envir <- list()
  if(inherits(spec, "yspec")) {
    require_yspec()
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

  var_names <- p$pmp.pmplot.pairs.cols

  label_map <- setNames(
    vapply(var_names, function(v) envir[[v]] %||% v, character(1)),
    var_names
  )

  if(isTRUE(unit_break)) {
    label_map <- newline_at_unit(label_map)
  }

  p$yAxisLabels <- unname(label_map)

  p
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
#' @inheritParams pmp_gg_labs
#' @param df a data frame to label.
#' @export
pm_label_columns <- function(df, spec, labs = list(), short_max = Inf) {
  assert_that(inherits(df,"data.frame"))
  envir <- list()
  if(inherits(spec, "yspec")) {
    require_yspec()
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
