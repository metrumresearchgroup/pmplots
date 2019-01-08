# nocov start

##' pmplots browser
##'
##' @param quiet if `TRUE` then a summary of available plots
##' will be printed
##' @param x output from `pm_browser`
##' @param plot_name the name of a plot to show
##'
##' @export
pm_browser <- function(quiet=FALSE) {
  assert_that(requireNamespace("yaml"))
  loc <- system.file("examples", "examples.yml", package="pmplots")
  data <- pmplots_data_obs() %>% mutate(CWRES = .data[["CWRESI"]])
  id <- pmplots_data_id()
  etas <- c("ETA1", "ETA2", "ETA3")
  x0 <- yaml::yaml.load_file(loc)
  out <- map_df(x0$plots, as_data_frame)
  names(out)[1] <- "y"
  out <- suppressMessages(
    suppressWarnings(
      out %>%
        mutate(gg = lapply(.data[["call"]], parse_eval)) %>%
        mutate(gg = map2(.data[["gg"]], .data[["call"]], .f=add_title)) %>%
        mutate(time = .data[["x"]] %in% c("time", "tad", "tafd")) %>%
        mutate(res = .data[["y"]] %in% c("res", "wres", "cwres", "cwresi")) %>%
        mutate(wrap = grepl("wrap", .data[["call"]])) %>%
        mutate(n = seq(n())) %>%
        mutate(name = names(x0$plots))
    )
  )
  if(!quiet) {
    message("available plots in the browser tibble:")
    lst <- paste0(out[["name"]], collapse = " ")
    lst <- strwrap(lst, width = 40)
    lst <- paste0(" ", lst, "\n")
    message(lst)
  }
  return(invisible(out))
}

add_title <- function(plot,call) {
  plot + ggtitle(call)
}

wrap_r_chunk <- function(text) {
  paste0("```{r}\n", text, "```")
}

##' @rdname pm_browser
##' @export
pm_browser_show <- function(x, plot_name=NULL) {
  if(is.null(plot_name)) {
    plot_name <- x[["name"]]
  }
  x <- filter(x, .data[["name"]]==plot_name)
  if(nrow(x)==0) {
    stop("The plot ", plot_name, " doesn't exist.", call.=FALSE)
  }
  z <- lapply(plot_name, function(.name) {
    x <- filter(x,.data[["name"]]==.name)
    cat("y:", x[["y"]], "\n")
    cat("x:", x[["x"]], "\n")
    cat("call:", x[["call"]], "\n")
    suppressMessages(print(x[["gg"]][[1]]))
  })
  return(invisible(x))
}

# nocov end
