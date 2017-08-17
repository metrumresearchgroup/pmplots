
##' superset2 data sets
##'
##' @export
superset2 <- function() {
  loc <- system.file(package="pmplots")
  readRDS(file=file.path(loc,"exdata", "superset2.RDS"))
}
##' @export
##' @rdname superset2
superset2id <- function() {
  loc <- system.file(package="pmplots")
  readRDS(file=file.path(loc,"exdata", "superset2id.RDS"))
}
##' @export
##' @rdname superset2
superset2obs <- function() {
  loc <- system.file(package="pmplots")
  readRDS(file=file.path(loc,"exdata", "superset2obs.RDS"))
}
