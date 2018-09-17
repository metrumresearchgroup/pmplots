
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
  readRDS(file=file.path(loc,"exdata", "pmplots_data.RDS"))
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
  dplyr::filter(readRDS(file=file.path(loc,"exdata", "pmplots_data_obs.RDS")),
                !is.na(IPRED))
}
