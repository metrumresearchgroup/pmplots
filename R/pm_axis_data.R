
##' Get defaults for plot axes
##'
##' @param what the axis identifier
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
    dv = "DV {yname}",
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

pm_axis_tad <- function() mk_col_title("tad")
pm_axis_time <- function() mk_col_title("time")
pm_axis_tafd <- function() mk_col_title("tafd")
pm_axis_res <- function() mk_col_title("res")
pm_axis_wres <- function() mk_col_title("wres")
pm_axis_cwres <- function() mk_col_title("cwres")
pm_axis_cwresi <- function() mk_col_title("cwresi")
pm_axis_npde <- function() mk_col_title("npde")
pm_axis_pred <- function() mk_col_title("pred")

