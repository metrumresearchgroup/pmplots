
.aliases <- new.env(parent = emptyenv())

#' @export
set_aliases <- function(...) {
  args <- list(...)
  if(!is_named(args)) {
    abort("all arguments must be named")
  }
  vars <- names(args)
  for(i in seq_along(vars)) {
    .aliases[[vars[i]]] <- args[[i]]
  }
}

sub_aliases <- function(x) {
  if(x %in% names(.aliases)) {
    return(.aliases[[x]])
  }
  x
}
