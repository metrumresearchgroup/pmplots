
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
    npde = "NPDE",
    npd = "NPD"
  ),
  title  = list(
    cwres = "CWRES",
    cwresi = "CWRES with interaction",
    res = "Residual",
    wres = "Weighted residual",
    time = "Time {xunit}",
    tad  = "Time after dose {xunit}",
    tafd = "Time after first dose {xunit}",
    pred = "Population predicted {xname}",
    ipred = "Individual predicted {xname}",
    dv = "Observed {yname}",
    npde = "NPDE",
    npd = "NPD"
  ),
  short = list(
    cwres = "CWRES",
    cwresi = "CWRESI",
    res = "RES",
    wres = "WRES",
    time = "Time {xunit}",
    tad = "TAD",
    tafd = "TAFD",
    pred = "PRED",
    ipred = "IPRED",
    dv = "Observed",
    npde = "NPDE",
    npd = "NPD"
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

#' Functions to generate axis data
#'
#' @details
#' These functions call `pmplots:::mk_col_title()`. When time units
#' (`xunit`) are passed, they will be wrapped in parens.
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
  glue_unit(mk_col_title("tad"), xunit)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_time <- function(xunit = NULL) {
  glue_unit(mk_col_title("time"), xunit)
}
#' @rdname pm_axis_functions
#' @export
pm_axis_tafd <- function(xunit = NULL) {
  glue_unit(mk_col_title("tafd"), xunit)
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
pm_axis_npd <- function() mk_col_title("npd")
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
