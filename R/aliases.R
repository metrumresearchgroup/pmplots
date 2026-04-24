
.aliases <- new.env(parent = emptyenv())

#' @export
set_aliases <- function(TIME = NULL, TAD = NULL) {
  if(is.character(TIME)) {
    .aliases[["TIME"]] <- TIME
  }
  if(is.character(TAD)) {
    .aliases[["TAD"]] <- TAD
  }
}

sub_aliases <- function(x) {
  if(x %in% names(.aliases)) {
    return(.aliases[[x]])
  }
  x
}
