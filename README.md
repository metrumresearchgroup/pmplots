
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

## Examples

``` r
library(pmplots)
library(dplyr)

data <- pmplots_data_obs()
```

The default pmplots behavior is to expect names following NONMEM
convention, for exammple `DV` refers to observed values, `PRED` refers
to population predicted values. With that convention, we can easily make
observed versus predicted plots

``` r
dv_pred(data, yname = "MyDrug (ng/ml)")
```

![](man/figures/README-dv_pred-1.png)<!-- -->

## NPDE plots

``` r
npde_time(data)
```

![](man/figures/README-npde_time-1.png)<!-- -->

``` r
npde_pred(data)
```

![](man/figures/README-npde_pred-1.png)<!-- -->

## QQ plots

``` r
cwres_q(data)
```

![](man/figures/README-cwres_q-1.png)<!-- -->

## Exploratory plots

``` r
id <- pmplots_data_id()
cont_cat(id, x = "STUDYc", y = c("WT", "SCR", "AAG")) %>% pm_grid()
```

![](man/figures/README-cont_cat-1.png)<!-- -->

``` r
pairs_plot(id, y = c("WT//Weight (kg)", "SCR//Creat (mg/dL)", "BMI//BMI (kg/m2)"))
```

![](man/figures/README-pairs-1.png)<!-- -->

## Some faceted versions

``` r
wrap_cont_time(data, y = c("RES","WRES", "CWRES", "NPDE"), ncol = 2)
```

![](man/figures/README-facet_example-1.png)<!-- -->

## Customization

This can all be customized.

# Function and workflow overview

A functions and features (code + output) listing is available at
[pmplots\_complete.md](https://github.com/metrumresearchgroup/pmplots/blob/master/inst/examples/pmplots_complete.md).

# Suggested packages

Consider installing the
[cowplot](https://CRAN.R-project.org/package=cowplot) package to help
arranging plots on a page. Also, consider installing
[latex2exp](https://CRAN.R-project.org/package=latex2exp) to allow you
to use latex in axis titles.
