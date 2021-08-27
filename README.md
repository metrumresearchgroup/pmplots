
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmplots <img align="right" src = "man/figures/metrum_pmplots_git_logo.png" width="135px">

<!-- badges: start -->
<!-- badges: end -->

The goal of pmplots is to create exploratory and diagnostic plots
commonly used in pharmacometrics.

## Installation

You can install the released version of pmplots from
[MPN](https://mpn.metworx.com/docs/) with:

``` r
mpn <- "https://mpn.metworx.com/snapshots/stable/2021-06-20"
install.packages("pmplots", repos = mpn)
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metrumresearchgroup/pmplots")
```

## Example

``` r
library(pmplots)
#> Loading required package: ggplot2
data <- pmplots_data_obs()
```

The default pmplots behavior is to expect names following NONMEM
convention

``` r
dv_pred(data)
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Function and workflow overview

A functions and features (code + output) listing is available at
[pmplots\_complete.md](https://github.com/metrumresearchgroup/pmplots/blob/master/inst/examples/pmplots_complete.md).

# Suggested packages

Consider installing the
[cowplot](https://CRAN.R-project.org/package=cowplot) package to help
arranging plots on a page. Also, consider installing
[latex2exp](https://CRAN.R-project.org/package=latex2exp) to allow you
to use latex in axis titles.
