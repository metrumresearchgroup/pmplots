#' Label ggplot aesthetics from a yspec object or other source
#'
#' @param spec a `yspec` object; label data is generated through a call to
#' [yspec::ys_get_short_unit()].
#' @param labs a named list of label data; names correspond to columns
#' in the data used to make the plot; overrides `spec`.
#' @param x label for the x aesthetic. If `NULL`, resolved via the mapped
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
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#' 
#' @examples
#' library(ggplot2)
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
                       ...) {
  colour <- colour %||% color %||% col
  linetype <- linetype %||% lty
  if(inherits(spec, "yspec")) {
    spec <- yspec::ys_get_short_unit(spec, short_max = short_max)
  }
  envir <- list()
  if(length(spec)) {
    assert_that(is.list(spec))
    assert_that(is_named(spec))
    envir <- spec
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
  as_label(q)
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
#' Applies [pm_gg_labs()] to a ggplot object or a list of ggplot objects,
#' updating axis and aesthetic labels by looking up column names in a named
#' list or `yspec` object.
#'
#' @inheritParams pm_gg_labs
#' @param x a ggplot object or a list of ggplot objects.
#' @param ... additional arguments passed to [pm_gg_labs()].
#'
#' @examples
#' data <- pmplots_data_obs()
#'
#' spec <- list(DV = "CX1123 concentration (ng/mL)", PRED = "Population prediction (ng/mL)")
#'
#' p <- dv_pred(data)
#' pm_relabel(p, spec)
#'
#' @seealso [pm_gg_labs()]
#' @export
pm_relabel <- function(x, ...) UseMethod("pm_relabel")

#' @rdname pm_relabel
#' @export
pm_relabel.gg <- function(x, spec, labs = list(), ...) {
  x + pm_gg_labs(spec, labs, ...)
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
  for(aes in c("fill", "colour", "linetype", "shape")) {
    label <- resolve_aes_label(aes, all_mappings, object)
    if(!is.null(label)) args[[aes]] <- label
  }

  p + do.call(ggplot2::labs, c(args, object$extra))
}
