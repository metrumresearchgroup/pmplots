
##' Get defaults for plot axes
##'
##' @param what the axis identifier
##'
##' @examples
##' pm_axis("cwres")
##'
##' @export
pm_axis <- function(what) {
  assertthat::assert_that(
    exists(what,pm_axis_data$col)
  )
  mk_col_title(what)
}
##' @export
##' @rdname pm_axis
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
    cwres = "Conditional weighted residual",
    cwresi = "Conditional weighted residual",
    res = "Residual",
    wres = "Weighted residual",
    time = "Time {xunit}",
    tad  = "Time after dose {xunit}",
    tafd = "Time after first dose {xunit}",
    pred = "Population predicted {xname}",
    ipred = "Individual predicted {xname}",
    dv = "Observed {yname}",
    npde = "NPDE"
  )
)

mk_col_title <- function(what,sep="//") {
  paste0(
    pm_axis_data[["col"]][[what]],
    sep,
    pm_axis_data[["title"]][[what]]
  )
}

##' Functions to generate axis data
##'
##' @details
##' These functions call \code{pmplots:::mk_col_title}
##'
##' @examples
##'pm_axis_time()
##'
##' @seealso \code{\link{pm_axis}}
##' @rdname pm_axis_functions
##' @name pm_axis_functions
##' @export
pm_axis_tad <- function() mk_col_title("tad")
##' @rdname pm_axis_functions
##' @export
pm_axis_time <- function() mk_col_title("time")
##' @rdname pm_axis_functions
##' @export
pm_axis_tafd <- function() mk_col_title("tafd")
##' @rdname pm_axis_functions
##' @export
pm_axis_res <- function() mk_col_title("res")
##' @rdname pm_axis_functions
##' @export
pm_axis_wres <- function() mk_col_title("wres")
##' @rdname pm_axis_functions
##' @export
pm_axis_cwres <- function() mk_col_title("cwres")
##' @rdname pm_axis_functions
##' @export
pm_axis_cwresi <- function() mk_col_title("cwresi")
##' @rdname pm_axis_functions
##' @export
pm_axis_npde <- function() mk_col_title("npde")
##' @rdname pm_axis_functions
##' @export
pm_axis_pred <- function() mk_col_title("pred")
##' @rdname pm_axis_functions
##' @export
pm_axis_ipred <- function() mk_col_title("ipred")
##' @rdname pm_axis_functions
##' @export
pm_axis_dv <- function() mk_col_title("dv")

