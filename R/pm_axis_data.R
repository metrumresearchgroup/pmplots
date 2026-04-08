
#' Get defaults for plot axes
#'
#' @param what the axis identifier.
#'
#' @examples
#' pm_axis("cwres")
#'
#' @export
pm_axis <- function(what) {
  if(!what %in% names(pm_axis_data$col)) {
    stop(glue("cannot find axis data for `{what}`."))
  }
  mk_col_title(what)
}
#' @export
#' @rdname pm_axis
pm_axis_data <- list(
  col = list(
    cwres = "CWRES",
    cwresi = "CWRESI",
    res = "RES",
    wres = "WRES",
    time = "TIME",
    tad = "TAD",
    tafd = "TAFD",
    pred = "PRED",
    ipred = "IPRED",
    dv = "DV",
    npde = "NPDE"
  ),
  title  = list(
    cwres = "CWRES",
    cwresi = "CWRES with interaction",
    res = "Residual",
    wres = "Weighted residual",
    time = "Time",
    tad  = "Time after dose",
    tafd = "Time after first dose",
    pred = "Population predicted",
    ipred = "Individual predicted",
    dv = "Observed {yname}",
    npde = "NPDE"
  ),
  short = list(
    cwres = "CWRES",
    cwresi = "CWRESI",
    res = "RES",
    wres = "WRES",
    time = "Time",
    tad = "TAD",
    tafd = "TAFD",
    pred = "PRED",
    ipred = "IPRED",
    dv = "Observed",
    npde = "NPDE"
  ), 
  title_opt = list(
    time = "time.label", 
    tafd = "tafd.label", 
    tad = "tad.label"
  ),
  short_opt = list(
    time = "time.label.short", 
    tafd = "tafd.label.short", 
    tad = "tad.label.short"
  )
)

mk_col_title <- function(what, sep = "//") {
  title <- ifelse(isTRUE(opts$axis.title.short), "short", "title")
  paste0(
    pm_axis_data[["col"]][[what]],
    sep,
    pm_axis_data[[title]][[what]]
  )
}

mk_col_title_opt <- function(what, sep = "//") {
  title <- ifelse(isTRUE(opts$axis.title.short), "short_opt", "title_opt")
  paste0(
    pm_axis_data[["col"]][[what]], 
    sep, 
    opts$get(pm_axis_data[[title]][[what]])
  )
}

#' Functions to generate axis data
#'
#' @details
#' These functions call  either `pmplots:::mk_col_title()` or 
#' `pmplots:::mk_col_title_opt()`. When time units (`xunit`) are 
#' passed, they will be wrapped in parens.
#'
#' @param xunit an optional time unit to be glued into the title.
#' @param xname an optional name to be glued into the title.
#' @param yname an optional name to be glued into the title.
#'
#' @examples
#' pm_axis_time()
#' pm_axis_time("h")
#' pm_axis_pred("concentration")
#'
#' @seealso [pm_axis()]
#' @rdname pm_axis_functions
#' @name pm_axis_functions
#' @md
#' @export
pm_axis_tad <- function(xunit = NULL) {
  title <- mk_col_title_opt("tad")
  glue_unit(title, xunit)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_time <- function(xunit = NULL) {
  title <- mk_col_title_opt("time")
  glue_unit(title, xunit)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_tafd <- function(xunit = NULL) {
  title <- mk_col_title_opt("tafd")
  glue_unit(title, xunit)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_res <- function() mk_col_title("res")
#' @rdname pm_axis_functions
#' @export
pm_axis_wres <- function() mk_col_title("wres")
#' @rdname pm_axis_functions
#' @export
pm_axis_cwres <- function() mk_col_title("cwres")
#' @rdname pm_axis_functions
#' @export
pm_axis_cwresi <- function() mk_col_title("cwresi")
#' @rdname pm_axis_functions
#' @export
pm_axis_npde <- function() mk_col_title("npde")
#' @rdname pm_axis_functions
#' @export
pm_axis_pred <- function(xname = NULL) {
  glue_xname(mk_col_title("pred"), xname)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_ipred <- function(xname = NULL) {
  glue_xname(mk_col_title("ipred"), xname)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_dv <- function(yname = NULL) {
  glue_yname(mk_col_title("dv"), yname)
}
