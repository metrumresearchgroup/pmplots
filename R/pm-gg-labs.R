#' @include pmp-gg-labs.R
NULL

#' Label ggplot aesthetics from a yspec object or other source
#'
#' This function generates labels for ggplot aesthetics based on the data
#' columns used to create the plot, looking up in a named list or `yspec`
#' object. See [pmp_gg_labs()] to label `x-` and `y-` axes in plots generated
#' by pmplots functions.
#'
#' @param spec a named list of label data; names correspond to columns
#' in the data used to make the plot; may also be a `yspec` object, which
#' will be converted to a named list through [yspec::ys_get_short_unit()].
#' @param labs another named list of label data to override names found in
#' `spec`.
#' @param x label for the x aesthetic; if `NULL`, resolved via the mapped
#' column name. Pass a column name as a plain string to look it up in `spec` or
#' `labs`; wrap in [I()] to use the string as a literal label.
#' @param y label for the y aesthetic; see `x`.
#' @param fill label for the fill aesthetic; see `x`.
#' @param colour,color,col label for the colour aesthetic; see `x`.
#' @param linetype,lty label for the linetype aesthetic; see `x`.
#' @param shape label for the shape aesthetic; see `x`.
#' @param quietly if `FALSE`, inform when the same aesthetic is mapped to
#' multiple variables that each have a spec entry but resolve to different
#' labels.
#' @param short_max passed to [yspec::ys_get_short_unit()].
#' @param x_break character width at which to insert a single line break in the
#' x axis label; defaults to `Inf` (no break). When the resolved label exceeds
#' this width, a single newline is inserted at the last word boundary at or
#' before the limit.
#' @param y_break character width at which to insert a single line break in the
#' y axis label; see `x_break`.
#' @param col_break a named list or named numeric vector; names refer to columns
#' in `spec` or `labs`, and each value is passed as the `width` argument to
#' [str_break()] to insert a newline in that column's label. Applied
#' column-by-column before axis labels are resolved; keys absent from
#' `spec`/`labs` are silently ignored.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#'
#' @details
#' In case multiple aesthetics are found, the aesthetics in the top-most
#' layer will be used. The user will be informed in case multiple
#' aesthetics are involved that resolve to different names. This
#' situation should be rare; use the `quietly` argument to suppress
#' notification to the console.
#'
#' @examples
#'
#' if(requireNamespace("yspec")) {
#' library(ggplot2)
#'
#' library(yspec)
#'
#' spec <- ys_help$spec()
#'
#' spec <- update_short(spec, TIME = "Time")
#'
#' data <- ys_help$data()
#'
#' p <- ggplot(data, aes(TIME, DV)) + geom_point()
#'
#' p + pm_gg_labs(spec)
#'
#' }
#'
#' @md
#' @export
pm_gg_labs <- function(spec = list(),
                       labs = list(),
                       x = NULL, y = NULL,
                       fill = NULL,
                       colour = NULL, color = NULL, col = NULL,
                       linetype = NULL, lty = NULL,
                       shape = NULL,
                       quietly = FALSE,
                       short_max = Inf,
                       x_break = Inf,
                       y_break = Inf,
                       col_break = list(),
                       ...) {
  colour <- colour %||% color %||% col
  linetype <- linetype %||% lty
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
  if(length(col_break)) {
    assert_that(is_named(col_break))
    assert_that(is.list(col_break) || is.numeric(col_break))
    col_break <- col_break[names(col_break) %in% names(envir)]
    for(col in names(col_break)) {
      envir[[col]] <- str_break(envir[[col]], width = col_break[[col]])
    }
  }
  structure(
    list(
      envir = envir,
      x = x,
      y = y,
      x_break = x_break,
      y_break = y_break,
      fill = fill,
      colour = colour,
      linetype = linetype,
      shape = shape,
      quietly = quietly,
      extra = list(...)
    ),
    class = "pm_gg_labs"
  )
}

aes_name <- function(q) {
  if(is.null(q)) return(NULL)
  rlang::as_label(q)
}

strip_factor_call <- function(var) {
  fct <- grepl("factor", var, fixed  = TRUE)
  if(!fct) return(var)
  vars <- all.vars(str2lang(var), functions = TRUE)
  vars <- vars[vars != "factor"]
  if(length(vars)==1) {
    vars
  } else {
    var
  }
}

resolve_label <- function(var, envir) {
  if(is.null(var)) return(NULL)
  var <- strip_factor_call(var)
  if(!is.null(envir) && !is.null(envir[[var]])) envir[[var]] else var
}

resolve_aes_label <- function(aes, all_mappings, object) {
  val <- object[[aes]]
  if(is.character(val)) {
    if(inherits(val, "AsIs")) return(as.character(val))
    return(object$envir[[val]])
  }
  qs <- all_mappings[names(all_mappings) == aes]
  if(length(qs) == 0) return(NULL)
  vars <- vapply(qs, aes_name, character(1))
  labels <- vapply(vars, resolve_label, character(1), envir = object$envir)
  if(isTRUE(object$quietly)) return(labels[[1]])
  vars_stripped <- vapply(vars, strip_factor_call, character(1))
  in_envir <- vapply(vars_stripped, \(v) !is.null(object$envir[[v]]), logical(1))
  if(sum(in_envir) > 1 && length(unique(labels[in_envir])) > 1) {
    inform(
      paste0(
        "Aesthetic '", aes, "' is mapped to multiple variables (",
        paste(vars, collapse = ", "),
        ") that resolve to different labels; label for '", vars[1], "' will be used."
      )
    )
  }
  labels[[1]]
}

#' Relabel a plot using a yspec object or named list
#'
#' Applies [pm_gg_labs()] to a ggplot object, `patchwork` object, or a list of
#' ggplot objects, updating axis and aesthetic labels by looking up column names
#' in a named list or `yspec` object.
#'
#' @inheritParams pm_gg_labs
#' @param x a gg object, a `patchwork` object (e.g., from [eta_covariate()] or
#'   [npde_panel()]), or a list of ggplot objects.
#' @param ... additional arguments passed to [pm_gg_labs()].
#'
#' @details
#' Methods are provided for `gg` objects (single plots), `patchwork` objects
#' (multi-panel layouts produced by functions such as [eta_covariate()] or
#' [npde_panel()]), and plain `list` objects containing ggplot objects. The
#' patchwork method applies the relabeling to every panel in the layout using
#' the `&` operator.
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(
#'   DV = "CX1123 concentration (ng/mL)",
#'   PRED = "Population prediction (ng/mL)"
#' )
#'
#' p <- dv_pred(data)
#'
#' pm_relabel(p, spec)
#'
#' @seealso [pm_gg_labs()], [pmp_gg_labs()]
#' @export
pm_relabel <- function(x, ...) UseMethod("pm_relabel")

#' @rdname pm_relabel
#' @export
pm_relabel.gg <- function(x, spec, labs = list(), ...) {
  x + pm_gg_labs(spec, labs, ...)
}

#' @rdname pm_relabel
#' @export
pm_relabel.patchwork <- function(x, spec, labs = list(), ...) {
  x & pm_gg_labs(spec, labs, ...)
}

#' @rdname pm_relabel
#' @export
pm_relabel.list <- function(x, spec, labs = list(), ...) {
  lapply(x, pm_relabel, spec = spec, labs = labs, ...)
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.pm_gg_labs <- function(object, p, object_name) {

  layer_mappings <- do.call(c, lapply(unname(p$layers), \(l) l$mapping))
  all_mappings <- c(p$mapping, layer_mappings)

  args <- list()
  args$x <- resolve_aes_label("x", all_mappings, object)
  args$y <- resolve_aes_label("y", all_mappings, object)
  args$x <- str_break(args$x, width = object$x_break)
  args$y <- str_break(args$y, width = object$y_break)
  for(aes in c("fill", "colour", "linetype", "shape")) {
    label <- resolve_aes_label(aes, all_mappings, object)
    if(!is.null(label)) args[[aes]] <- label
  }

  p + do.call(ggplot2::labs, c(args, object$extra))
}
