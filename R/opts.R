
#' Set global plot options
#'
#' @param smooth.lwd line width for smoother
#' @param smooth.col line color for smoother
#' @param smooth.lty line type for smoother
#' @param smooth.method method to use for creating smoother
#' @param scatter.size point size for scatter plot
#' @param scatter.col character name of color for scatter plot
#' @param density.lwd line width for density plot on histogram
#' @param density.col line color for density plot on histogram
#' @param density.lty line type for density plot on histogram
#' @param hline.lwd line width for horizontal reference line; see [layer_h]
#' and [gh]
#' @param hline.col line color for horizontal reference line; see [layer_h]
#' and [gh]
#' @param hline.lty line type for horizontal reference line; see [layer_h]
#' and [gh]
#' @param abline.lwd line width for diagonal reference line; see [layer_a]
#' and [ga]
#' @param abline.col line color for diagonal refrence line; see [layer_a]
#' and [ga]
#' @param abline.lty line type for diagonal reference line; see [layer_a]
#' and [ga]
#' @param histogram.fill fill color for histograms
#' @param histogram.alpha alpha value for histograms
#' @param histogram.col line color for histograms
#' @param boxplot.fill fill color for histograms
#' @param boxplot.alpha alpha value for boxplots
#' @param boxplot.hline.lwd line width for horizantal reference line
#' @param boxplot.hline.lty line type for horizontal reference line
#' @param boxplot.hline.col line color for horizontal reference line
#' @param boxplot.outlier.shape shape for outliers in boxplots
#' @param qq.col point color for qq plots
#' @param qq.alpha alpha value for qq plots
#' @param qq.size point size for qq plots
#' @param pairs.cor.size font size for correlation text in upper panels of pairs
#' plots
#' @param pairs.cor.prefix prefix for correlation text in upper panels of pairs
#' plots
#' @param pairs.cor.col color for correlation text in upper panels of pairs plots
#' @param pairs.cor.fontface font face for correlation text in upper panels of
#' pairs plots
#' @param pairs.cor.digits number of digits to show when reporting correlation
#' in upper panels of pairs plots
#' @param pairs.cor.shown if `TRUE`, then report the number of non-missing
#' observations used to calculate correltion in upper panels of pairs plots
#' @param axis.title.short shorten standard axis titles
#' @param time.unit default time unit
#'
#' @details
#' [pm] and [pm_opts] both refer to the same environment.
#'
#' Global options can
#' be set in the environment with `pm$set(name = value)`.  There is also
#' a `.list` argument to `pm$set` that allows you to pass in a named list of
#' options to set (e.g. `pm$set(.list = list(smooth.col="red4"))`).
#'
#' Values can be extracted with `pm$get("name")`.
#'
#' Because it is an environment, the `$` operator can also be used to get and
#' set values (see examples).
#'
#' Other methods in the environment include `pm$as.list()`, `pm$mget()`,
#' `pm$reset()`.  `pm$self` refers to the environment itself. A list of
#' default settings can be obtained with `pm$defaults`.  Methods for the
#' `pm_opts` object include: [print.pm_opts], [as.list.pm_opts],
#' [`$<-.pm_opts`].
#'
#'
#'
#' @examples
#'
#' pm$set(smooth.lwd = 2)
#'
#' pm$smooth.lwd
#'
#' pm$smooth.lwd <- 1.3
#'
#' pm$smooth.lwd
#'
#' pm$get("smooth.lwd")
#'
#' pm$reset()
#'
#' x <- pm$as.list()
#'
#' \dontrun{
#'  defs <- pm$defaults
#'  defs$smooth.col <- "firebrick"
#'  pm$set(.list = defs)
#' }
#'
#' @md
#' @name pm_opts
pm_options <- function(smooth.lwd = 1.35,
                       smooth.col = .ggblue,
                       smooth.lty = 2,
                       smooth.method = "loess",
                       scatter.size = 1.5,
                       scatter.col = "black",
                       density.lwd = 1.35,
                       density.col = .ggblue,
                       density.lty = 2,
                       hline.lwd = 1.35,
                       hline.col = "darkgrey",
                       hline.lty = 1,
                       abline.lwd = 1.35,
                       abline.col = "darkgrey",
                       abline.lty = 1,
                       histogram.fill = "black",
                       histogram.alpha = 0.6,
                       histogram.col = "white",
                       boxplot.fill = "white",
                       boxplot.alpha = 1,
                       boxplot.hline.lwd = 1,
                       boxplot.hline.lty = 2,
                       boxplot.hline.col = "black",
                       boxplot.outlier.shape = 19,
                       qq.col = .ggblue,
                       qq.alpha = 1,
                       qq.size = 1.35,
                       pairs.cor.size = 3,
                       pairs.cor.prefix = "corr\n",
                       pairs.cor.col = .ggblue,
                       pairs.cor.fontface = "bold",
                       pairs.cor.digits = 2,
                       pairs.cor.shown = TRUE,
                       axis.title.short = FALSE,
                       time.unit = "hr") {
  set <- function(..., .list = NULL) {
    if(is.list(.list)) {
      x <- .list
    } else {
      x <- list(...)
    }
    if(length(x)==0) invisible(NULL)
    for(k in names(x)) assign(k,x[[k]],envir=self)
    return(invisible(NULL))
  }
  get <- function(x) {
    self[[x]]
  }
  reset <- function() {
    for(k in names(defaults)) assign(k,defaults[[k]],envir=self)
    return(invisible(NULL))
  }
  mget <- function(x) base::mget(x,envir=self)
  as.list <- function() {
    ans <- base::as.list.environment(self)
    ans$defaults <- NULL
    ans$self <- NULL
    ans$set <- NULL
    ans$get <- NULL
    ans$reset <- NULL
    ans$mget <- NULL
    ans$as.list <- NULL
    ans
  }
  self <- environment()
  defaults <- as.list()
  structure(self, class = c("pm_opts", "environment"))
}

#' Assign a pmplots option
#'
#' @param x the `pm_opts` object
#' @param i the name of the option to set
#' @param value the value to set
#'
#' @examples
#'
#' pm$scatter.size <- 4
#'
#' @export
`$<-.pm_opts` <- function(x,i,value) {
  if(!(i %in% names(x[["defaults"]]))) {
    warning(glue("'{i}' is not a valid option to set"),call.=FALSE)
    return(invisible(x))
  }
  assign(i,value,envir=x)
  return(invisible(x))
}

#' Extract multiple pmplots options
#'
#' @param x the `pm_opts` object
#' @param i a character option name to extract
#' @param ... other option names to extract
#' @param exact not used
#'
#' @examples
#' pm["scatter.size", "time.unit"]
#'
#' @export
`[.pm_opts` <- function(x,i,...,exact=TRUE) {
  i <- as.character(i)
  dots <- as.character(list(...))
  x$mget(c(i,dots))
}

#' Coerce pm_opts object to a list
#'
#' @param x the `pm_opts` object
#' @param ... not used
#'
#' @examples
#' x <- as.list(pm)
#'
#' @export
as.list.pm_opts <- function(x,...) {
  x$as.list()
}

#' Print the pm_opts object
#'
#' @param x the `pm_opts` object
#' @param ... not used
#' @examples
#' pm
#'
#' @export
print.pm_opts <- function(x,...) {
  cat(str(x))
}

#' @rdname pm_opts
#' @export
pm_opts <- pm_options()
#' @rdname pm_opts
#' @export
pm <- pm_opts
opts <- pm_opts

