
pairs_lower <- function(data, mapping, smooth_color = .ggblue, smooth_lty = 2, ...) {

  mapping_list <- rlang::as_list(mapping)

  if(is.character(mapping_list$smooth_colour)){
    smooth_color <- mapping_list$smooth_colour
  }
  if(is.numeric(mapping_list$smooth_lty)) {
    smooth_lty <- mapping_list$smooth_lty
  }

  ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = "loess", color = smooth_color, lty = smooth_lty,
                se = FALSE, lwd = 1.3)

}

pairs_upper <- function(data, mapping, ...) {
  x <- rlang::quo_name(mapping$x)[1]
  y <- rlang::quo_name(mapping$y)[1]
  label <- as.character(signif(cor(data[,x],data[,y],use = "complete.obs"), digits=3))

  label <- paste0("Corr: ", label)
  GGally::ggally_text(label = label) +
    theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank())
}

##' Pairs plots using ggpairs
##'
##' This funciton is a wrapper to [GGally::ggpairs] with customized
##' functions for upper and lower off-diagonal panels.
##'
##' @param x plotting data.frame
##' @param etas character `col//label` for pairs data; see [col_label]
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
pairs_plot <- function(x, etas, bins = 15, alpha = 0.6, fill = "black",
                       col="grey", label_fun = label_parse_label,
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

  if(length(etas)==1) {
    ans <- eta_hist(
      x, etas, bins = bins, alpha = alpha, fill = fill,
      col = col, ...
    )
    return(ans)
  }

  diag <- GGally::wrap("barDiag", bins = bins,
                       alpha = alpha, fill=fill, col=col)
  x <- as.data.frame(x)
  etal <- lapply(etas, col_label)
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
    upper = list(continuous = pairs_upper),
    diag = list(continuous = diag),
    lower = list(continuous = pairs_lower)
  ) + pm_theme()
}

##' @rdname pairs_plot
##' @export
eta_pairs <- function(...) {
  pairs_plot(...)
}
