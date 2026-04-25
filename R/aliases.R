
.aliases <- new.env(parent = emptyenv())

#' Manage column name aliases
#'
#' @description
#' * `pm_aliases()` prints the currently active aliases.
#' * `pm_set_aliases()` registers one or more aliases mapping a data column name
#'    to a canonical pmplots column name.
#' * `pm_clear_aliases()` removes all registered aliases.
#' * `pm_show_canonical()` returns the canonical column names that can be aliased.
#'
#' @export
pm_aliases <- function() {
  vars <- names(.aliases) 
  if(!length(vars)) {
    inform("[pmplots] no aliases were found.")
    return(invisible(NULL))
  }
  al <- as.list(.aliases)
  pada <- max(unlist(sapply(al, nchar)))
  padb <- max(nchar(names(al)))
  for(i in seq_along(al)) {
    b <- names(al)[i]
    a <- al[[b]]
    a <- formatC(a, width = pada, flag = "-")
    b <- formatC(b, width = padb, flag = "-")
    msg <- paste0("data ", a, " --> ", b, " in pmplots")
    names(msg) <- "*"
    inform(msg)
  }
}
#' @rdname pm_aliases
#' @param ... `alias_name = canonical_name` pairs, where the right-hand side
#' can be quoted or unquoted.
#' @export
pm_set_aliases <- function(...) {
  args <- enexprs(...)
  if(!is_named(args)) {
    abort("all arguments must be named")
  }
  aliases <- names(args)
  canon <- vapply(args, function(x) {
    if(is.symbol(x)) return(as.character(x))
    if(is.character(x)) return(x)
    abort("arguments must be unquoted or quoted column names.")
  }, "TIME", USE.NAMES = FALSE)
  if(!all(canon %in% canon_cols)) {
    stop("only certain columns can be aliased; see `pm_show_canonical()`. ")
  }
  for(i in seq_along(aliases)) {
    .aliases[[canon[i]]] <- aliases[[i]]
  }
}
#' @rdname pm_aliases
#' @export
pm_clear_aliases <- function() {
  rm(list = ls(.aliases), envir = .aliases)
}

#' @rdname pm_aliases
#' @export
pm_show_canonical <- function() {
  canon_cols
}

substitute_alias <- function(x) {
  if(!is.character(x) && length(x)==1) {
    abort("`x` must be character with length 1.")
  }
  if(x %in% names(.aliases)) {
    return(.aliases[[x]])
  }
  x
}

