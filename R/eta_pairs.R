
pairs_lower <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(col=opts$scatter.col,size=opts$scatter.size) +
    geom_smooth(
      method = opts$smooth.method,
      color = opts$smooth.col,
      lty = opts$smooth.lty,
      se = FALSE,
      lwd = opts$smooth.lwd
    )
}

pairs_upper <- function(data, mapping, ...) {
  x <- rlang::quo_name(mapping$x)[1]
  y <- rlang::quo_name(mapping$y)[1]
  corr <- cor(data[,x],data[,y],use = "complete.obs")
  label <- force_digits(corr,digits=opts$pairs.cor.digits)
  label <- paste0(opts$pairs.cor.prefix, label)
  if(isTRUE(opts$pairs.cor.shown)) {
    n <- sum((!is.na(data[,x])) & (!is.na(data[,y])))
    label <- paste0(label, "\n", paste0("(n=",n,")"))
  }

  GGally::ggally_text(
    label = label,
    size = opts$pairs.cor.size,
    col = opts$pairs.cor.col,
    fontface = opts$pairs.cor.fontface
  ) + theme(
      panel.grid = ggplot2::element_blank()
    )
}

##' Pairs plots using ggpairs
##'
##' This funciton is a wrapper to [GGally::ggpairs] with customized
##' functions for upper and lower off-diagonal panels. See details for help
##' on customizing some aspects of the off-diagonal plots.
##'
##' @param x plotting data.frame
##' @param y character `col//label` for pairs data; see [col_label]
##' @param bins passed to [ggplot2::geom_histogram]
##' @param alpha passed to [ggplot2::geom_histogram]
##' @param fill passed to [ggplot2::geom_histogram]
##' @param col passed to [ggplot2::geom_histogram]
##' @param label_fun labeler function that gets passed to [GGally::ggpairs];
##' the default is based on [parse_label] and thus allows latex
##' expressions in the label (see examples)
##' @param upper_fun function to use for `upper` argument
##' @param lower_fun function to use for `lower` argument
##' @param ... passed to [GGally::ggpairs]
##'
##' @details This function requires the `GGally` package to be installed.
##'
##' When the length of `etas` is one, arguments are passed to [eta_hist] and
##' that result is returned.
##'
##' Use the options object to modify aspects of the smoother line:
##' `smooth.col`, `smooth.lwd`, `smooth.lty`, `smooth.method`.  Also, use the
##' options object to control aspects of the points: `scatter.col`,
##' `scatter.size`.
##'
##' @return The result from a `ggpairs` call (a single plot).
##'
##' @examples
##'
##' id <- pmplots_data_id()
##'
##' etas <- c("ETA1//ETA-CL", "ETA2//ETA-VC", "ETA3//ETA-KA")
##'
##' eta_pairs(id,etas)
##'
##' df <- data.frame(x = rnorm(1000), y = rnorm(1000))
##'
##' pairs_plot(df, c("x", "y"))
##'
##' df2 <- dplyr::tibble(x = rnorm(100), y = x^2)
##'
##' pairs_plot(df2, c("x//x", "y//x$^2$"))
##'
##' @md
##' @export
pairs_plot <- function(x, y, bins = 15,
                       alpha = opts$histogram.alpha,
                       fill = opts$histogram.fill,
                       col = opts$histogram.col,
                       label_fun = label_parse_label,
                       upper_fun = NULL, lower_fun = NULL, ...) {

  if(!requireNamespace("GGally")) {
    stop("this function requires that the GGally package be installed",
         call. = FALSE)
  }

  if(is.null(upper_fun)) {
    upper_fun <- pairs_upper
  }

  if(is.null(lower_fun)) {
    lower_fun <- pairs_lower
  }

  if(length(y)==1) {
    ans <- eta_hist(
      x, y, bins = bins, alpha = alpha, fill = fill,
      col = col, ...
    )
    return(ans)
  }

  diag_fun <- GGally::wrap("barDiag", bins = bins,
                           alpha = alpha, fill=fill, col=col)
  x <- as.data.frame(x)
  etal <- lapply(y, col_label)
  cols <- sapply(etal, "[[", 1L)
  labs <- sapply(etal, "[[", 2L)
  cols <- unique(cols)
  labs <- unique(labs)

  for(col in cols) {
    require_numeric(x,col)
  }

  GGally::ggpairs(
    x, aes(...),
    columns=cols,
    columnLabels=labs,
    labeller = label_fun,
    upper = list(continuous = upper_fun),
    diag = list(continuous = diag_fun),
    lower = list(continuous = lower_fun)
  ) + pm_theme()
}

#' @param etas character `col//label` for pairs data; see [col_label]
#' @rdname pairs_plot
#' @export
eta_pairs <- function(x,etas,...) {
  pairs_plot(x = x, y = etas, ...)
}
