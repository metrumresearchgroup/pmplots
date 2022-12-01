
##' Example data sets
##'
##' @examples
##'
##' head(pmplots_data_obs())
##'
##' head(pmplots_data_id())
##'
##' @export
pmplots_data <- function() {
  loc <- system.file(package="pmplots")
  ans <- readRDS(file=file.path(loc,"exdata", "pmplots_data.RDS"))
  ans$CWRES <- ans$CWRESI
  ans
}
##' @export
##' @rdname pmplots_data
pmplots_data_id <- function() {
  loc <- system.file(package="pmplots")
  readRDS(file=file.path(loc,"exdata", "pmplots_data_id.RDS"))
}
##' @export
##' @rdname pmplots_data
pmplots_data_obs <- function() {
  loc <- system.file(package="pmplots")
  ans <- readRDS(file=file.path(loc,"exdata", "pmplots_data_obs.RDS"))
  ans$CWRES <- ans$CWRESI
  dplyr::filter(ans, !is.na(IPRED))
}

##' Get the correct dataset for a nlmixr2 fit
##'
##' @param data This is the dataset to check
##' 
##' @param type gives the type of merge between the original dataset
##'   and the fit dataset.  The left dataset represents the original
##'   dataset while the right dataset represents the fitted dataset.
##' 
##' @return If this is a nlmixr2 fit object, return the expanded full
##'   dataset.  If it isn't a nlmixr2 fit simply return the dataset
##' 
##' @author Matthew Fidler
##'
##'
##' @export
##' 
##' @examples
##' if (requireNamespace("nlmixr2extra", quietly = TRUE)) {
##' 
##'  # This gets the merged dataset from a nlmixr2 fit
##'  # If we grab the pre-run nlmixr2 fit from nlmixr2extra we can see the fit doesn't have WT in it
##'  print(nlmixr2extra::theoFitOde)
##'
##'  # But if we simply expand the dataset, the WT is added from the original dataset
##'  pmplots_nlmixr2_data(nlmixr2extra::theoFitOde)
##'
##' }
pmplots_nlmixr2_data <- function(data, type=c("left", "inner", "right")) {
  if (inherits(data, "nlmixr2FitData")) { 
    type <- match.arg(type)
    return(switch(type,
                  left=data$dataMergeLeft,
                  inner=data$dataMergeInner,
                  right=data$dataMergeRight))
  }
  data
}
