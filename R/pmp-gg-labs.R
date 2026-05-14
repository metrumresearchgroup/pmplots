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
#' will be converted to a named list through `yspec::ys_get_short_unit()`.
#' @param labs another named list of label data to override names found in
#' `spec`.
#' @param x label for the x aesthetic; if `NULL`, resolved via the mapped
#' column name. Pass a column name as a plain string to look it up in `spec`
#' or `labs`; wrap in [I()] to use the string as a literal label.
#' @param y label for the y aesthetic; see `x`.
#' @param short_max passed to [yspec::ys_get_short_unit()] when `spec` is a
#' `yspec` object.
#' @param x_break character width at which to insert a single line break in the
#'   x axis label; defaults to `Inf` (no break); when the resolved label exceeds
#'   this width, a single newline is inserted at the last word boundary at or
#'   before the limit.
#' @param y_break character width at which to insert a single line break in the
#'   y axis label; see `x_break`.
#' @param var_break a named list or named numeric vector; names refer to
#'   variables in `spec` or `labs`, and each value is passed as the `width`
#'   argument to [str_break()] to insert a newline in that variable's label.
#'   Applied variable-by-variable before axis labels are resolved; keys absent
#'   from `spec`/`labs` are silently ignored.
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
                       x_break = Inf,
                       y_break = Inf,
                       var_break = list(),
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
  if(length(var_break)) {
    assert_that(is_named(var_break))
    assert_that(is.list(var_break) || is.numeric(var_break))
    var_break <- var_break[names(var_break) %in% names(envir)]
    for(v in names(var_break)) {
      envir[[v]] <- str_break(envir[[v]], width = var_break[[v]])
    }
  }
  structure(
    list(
      envir = envir,
      x = x,
      y = y,
      x_break = x_break,
      y_break = y_break,
      extra = list(...)
    ),
    class = "pmp_gg_labs"
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.pmp_gg_labs <- function(object, p, object_name) {
  assert_that(
    isTRUE(p$pmp.pmplot) || is_pmp_patch(p),
    msg = "pmp_gg_labs() can only be used with plots created by pmplots."
  )
  existing <- gg_get_labs_2(p)
  args <- list()
  x <- object$x %||% p$pmp.x
  y <- object$y %||% p$pmp.y
  args$x <- resolve_axis_label(x, object$envir, existing$x)
  args$y <- resolve_axis_label(y, object$envir, existing$y)
  args$x <- str_break(args$x, width = object$x_break)
  args$y <- str_break(args$y, width = object$y_break)
  p + do.call(ggplot2::labs, c(args, object$extra))
}

#' Relabel a pmplots plot using a yspec object or named list
#'
#' This function relabels `x` and `y` axis titles based on the data columns
#' used to create the plot, looking up in a named list or `yspec` object. This
#' is only for pmplot outputs; consider [pm_relabel()] for labeling
#' aesthetics in an arbitrary `gg` object.
#'
#' Methods are provided for `gg` objects (single pmplots plots), `patchwork`
#' objects (multi-panel layouts produced by functions such as [eta_covariate()]
#' or [npde_panel()]), and plain `list` objects containing pmplots plots.
#' The patchwork method applies the relabeling to every panel in the layout
#' using the `&` operator.
#'
#' @inheritParams pmp_gg_labs
#' @param obj a gg object created through a `pmplots` function, a `patchwork`
#' object (e.g., from [eta_covariate()] or [npde_panel()]), or a list of such
#' objects.
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
pmp_relabel.patchwork <- function(obj, spec = list(), labs = list(), ...) {
  obj & pmp_gg_labs(spec, labs, ...)
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
#' @param f_break character width at which to insert a single line break in
#'   facet strip labels; defaults to `Inf` (no break); when the resolved label
#'   exceeds this width, a single newline is inserted at the last word boundary
#'   at or before the limit.
#' @param unit_break if `TRUE`, a newline is inserted between the label text and
#'   a trailing parenthetical unit (e.g., `"Weight (kg)"` becomes
#'   `"Weight\n(kg)"`); defaults to `FALSE`.
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
pmp_relabel_wrap <- function(p, spec, labs = list(), short_max = Inf, f_break = Inf, unit_break = FALSE) {
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

  if(is.finite(f_break)) {
    label_map <- setNames(
      str_break(label_map, width = f_break),
      names(label_map)
    )
  }

  if(isTRUE(unit_break)) {
    label_map <- newline_at_unit(label_map)
  }

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
#' @param f_break character width at which to insert a single line break in
#'   facet strip labels; defaults to `Inf` (no break); when the resolved label
#'   exceeds this width, a single newline is inserted at the last word boundary
#'   at or before the limit.
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
pmp_relabel_pairs <- function(p, spec, labs = list(), short_max = Inf,
                              f_break = Inf, unit_break = TRUE, ...) {
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

  if(is.finite(f_break)) {
    label_map <- setNames(
      str_break(label_map, width = f_break),
      names(label_map)
    )
  }

  if(isTRUE(unit_break)) {
    label_map <- newline_at_unit(label_map)
  }

  p$yAxisLabels <- unname(label_map)
  p$xAxisLabels <- unname(label_map)

  p
}

#' Relabel plots in a list using pmp_relabel
#'
#' Pass in a named list of gg objects and apply [pmp_relabel()] or
#' [pm_relabel()], as appropriate.
#'
#' @param x a named list of gg objects.
#' @param at a character vector of list names to relabel.
#' @param re a regular expression for selecting names to be used for `at`.
#' @param spec a named list of label data; names correspond to columns in the
#'   data used to make the plots; may also be a `yspec` object.
#' @param labs another named list of label data to override names found in
#'   `spec`.
#' @param ... additional arguments passed to [pmp_relabel()] or [pm_relabel()].
#'
#' @details
#' Note that all plots in the list need to be named. When `re` is provided it
#' takes precedence over `at`.
#'
#' @seealso [pmp_relabel()], [rot_at()].
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(DV = "Observed (ng/mL)", PRED = "Population predicted (ng/mL)")
#'
#' x <- list(p1 = dv_pred(data), p2 = dv_ipred(data))
#'
#' x <- relabel_at(x, at = "p1", spec = spec)
#' x$p1
#'
#' x <- relabel_at(x, re = "p", spec = spec)
#' x$p2
#'
#' @md
#' @export
relabel_at <- function(x, at = names(x), spec = list(), labs = list(),
                       re = NULL, ...) {
  if(!is.list(x) || is_ggplot(x) || inherits(x, "patchwork")) {
    abort("`x` must be a list of gg objects.")
  }
  if(!is_named(x)) abort("`x` must be named.")
  if(is.character(re)) {
    where <- grep(re, names(x), perl = TRUE)
  } else {
    if(!is.character(at)) abort("`at` must be character.")
    bad <- setdiff(at, names(x))
    if(length(bad)) {
      names(bad) <- rep("x", length(bad))
      abort("requested names not found in `x`.", body = bad)
    }
    where <- which(names(x) %in% at)
  }
  if(!length(where)) {
    warn("did not find any plots to relabel.")
    return(x)
  }
  for(w in where) {
    pmp <- isTRUE(x[[w]]$pmp.pmplot)
    if(!pmp) {
      pmp <- is_pmp_patch(x[[w]])
    }
    if(pmp) {
      x[[w]] <- pmp_relabel(x[[w]], spec = spec, labs = labs, ...)
    } else {
      x[[w]] <- pm_relabel(x[[w]], spec = spec, labs = labs, ...)
    }
  }
  x
}

#' Add axis label data to a data frame
#'
#' This function adds candidate axis titles as an attribute on columns in
#' `data`. This attribute is intended to be specifically used for pmplot axis
#' labels, not to be confused with the label added by `yspec::ys_add_labels()`.
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
