# nocov start

##' pmplots browser object
##'
##' @param quiet if `TRUE` then a summary of available plots
##' will be printed
##' @param title if `TRUE`, plotting code will be added as the plot title
##' @param x output from `pm_browser`
##' @param plot_name the name of a plot to show
##' @seealso \code{\link{pm_gadget}}
##' @export
pm_browser <- function(quiet=FALSE,title=TRUE) {
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
        mutate(gg = map2(.data[["gg"]], .data[["call"]], .f=add_title,title=title)) %>%
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

add_title <- function(plot,call,title) {
  if(!title) return(plot)
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

##' pmplots browser gadget
##' @seealso \code{\link{pm_browser}}
##' @export
pm_gadget <- function() {

  assert_that(requireNamespace("shiny"))
  assert_that(requireNamespace("miniUI"))

  x <- pm_browser(quiet = TRUE,title=FALSE)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Plot Browser", left=NULL),
    miniUI::miniContentPanel(
      shiny::selectInput("plot_name", "Choose a plot",
                  choices = x[["name"]]),
      shiny::plotOutput("plot", height = "67%"),
      shiny::verbatimTextOutput("code"),
      padding=24
    )
  )

  server <- function(input, output, session) {
    foo <- shiny::reactive({
      filter(x, .data[["name"]]==input$plot_name)
    })

    # Render the plot
    output$plot <- shiny::renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      foo() %>% pull(.data[["gg"]])
    }, res = 120)
    output$code <- shiny::renderText({
      this <- foo()
      cll <- this[["call"]]
      x <- NULL
      y <- NULL
      if(grepl("(id", cll, fixed=TRUE)) {
        x <- "id <- pmplots_data_id()\n"
      }
      if(grepl("(data", cll, fixed=TRUE)) {
        y <- "data <- pmplots_data_obs()\n"
      }
      return(paste0(c(y,x,cll), collapse = "\n"))
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })
  }
  shiny::runGadget(ui, server)
}


# nocov end
