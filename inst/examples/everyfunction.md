Available functions
================

-   [Example data in the package](#example-data-in-the-package)
-   [`col//title` specification](#coltitle-specification)
-   [Observed vs predicted](#observed-vs-predicted)
    -   [Observed versus population predicted (`dv_pred`)](#observed-versus-population-predicted-dv_pred)
        -   [Observed versus population predicted - log/log](#observed-versus-population-predicted---loglog)
    -   [Observed versus individual predicted (`dv_ipred`)](#observed-versus-individual-predicted-dv_ipred)
        -   [Observed versus individual predicted - log/log](#observed-versus-individual-predicted---loglog)
-   [Residual plots](#residual-plots)
    -   [Residuals](#residuals)
        -   [Residuals versus time (`res_time`)](#residuals-versus-time-res_time)
        -   [Residuals versus time after dose (`res_tad`)](#residuals-versus-time-after-dose-res_tad)
        -   [Residuals versus population predicted (`res_pred`)](#residuals-versus-population-predicted-res_pred)
        -   [RES versus continuous covariate (`res_cont`)](#res-versus-continuous-covariate-res_cont)
        -   [RES by categorical covariate (`res_cat`)](#res-by-categorical-covariate-res_cat)
    -   [Weighted residuals](#weighted-residuals)
        -   [Weighted residuals versus time (`wres_time`)](#weighted-residuals-versus-time-wres_time)
        -   [Weighted residuals versus time after dose (`wres_tad`)](#weighted-residuals-versus-time-after-dose-wres_tad)
        -   [Weighted esiduals versus population predicted (`wres_pred`)](#weighted-esiduals-versus-population-predicted-wres_pred)
        -   [WRES versus continuous covariate (`wres_cont`)](#wres-versus-continuous-covariate-wres_cont)
        -   [WRES by categorical covariate (`wres_cat`)](#wres-by-categorical-covariate-wres_cat)
        -   [WRES QQ plot (`wres_q`)](#wres-qq-plot-wres_q)
    -   [Conditional weighted residuals (CWRES)](#conditional-weighted-residuals-cwres)
        -   [CWRES versus time (`cwres_time`)](#cwres-versus-time-cwres_time)
        -   [CWRES versus time after dose (`cwres_tad`)](#cwres-versus-time-after-dose-cwres_tad)
        -   [CWRES versus continuous covariate (`cwres_cont`)](#cwres-versus-continuous-covariate-cwres_cont)
        -   [CWRES by categorical covariate (`cwres_cat`)](#cwres-by-categorical-covariate-cwres_cat)
        -   [CWRES versus population predicted (`cwres_pred`)](#cwres-versus-population-predicted-cwres_pred)
        -   [CWRES QQ plot (`cwres_q`)](#cwres-qq-plot-cwres_q)
-   [ETA plots](#eta-plots)
    -   [ETA versus continuous covariates (`eta_cont`)](#eta-versus-continuous-covariates-eta_cont)
    -   [ETA by categorical covariates (`eta_cat`)](#eta-by-categorical-covariates-eta_cat)
    -   [ETA histograms (`eta_hist`)](#eta-histograms-eta_hist)
-   [DV versus time (`dv_time`)](#dv-versus-time-dv_time)
    -   [Basic plot](#basic-plot)
    -   [Faceted](#faceted)
    -   [Colored](#colored)
    -   [log-Scale](#log-scale)
-   [Data summary](#data-summary)
    -   [Continuous variable by categorical variable (`cont_cat`)](#continuous-variable-by-categorical-variable-cont_cat)
    -   [Split and plot (`split_plot`)](#split-and-plot-split_plot)
-   [Plot output (`mrggsave`)](#plot-output-mrggsave)
    -   [Saving single plots](#saving-single-plots)
    -   [Save multiple plots to one file](#save-multiple-plots-to-one-file)
    -   [Arrange multiple plots on a single page](#arrange-multiple-plots-on-a-single-page)

``` r
library(pmplots)
library(dplyr)
```

Example data in the package
===========================

``` r
df <- superset2() %>% filter(EVID==0)

id <- distinct(df, ID, .keep_all=TRUE)

dayx <- defx(breaks = seq(0,168,24))
.yname <- "NoDoz (ng/mL)"
```

`col//title` specification
==========================

This is a way to specify the column name for source data along with the axis label col\_label("CL//Clearance (L)")

Observed vs predicted
=====================

Observed versus population predicted (`dv_pred`)
------------------------------------------------

``` r
dv_pred(df, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-4-1.png)

### Observed versus population predicted - log/log

``` r
dv_pred(df,loglog=TRUE, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-5-1.png)

Observed versus individual predicted (`dv_ipred`)
-------------------------------------------------

``` r
dv_ipred(df, what=.what)
```

![](img/everyfunction--unnamed-chunk-6-1.png)

### Observed versus individual predicted - log/log

``` r
dv_ipred(df, loglog=TRUE, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-7-1.png)

Residual plots
==============

Residuals
---------

### Residuals versus time (`res_time`)

``` r
res_time(df)
```

![](img/everyfunction--unnamed-chunk-8-1.png)

### Residuals versus time after dose (`res_tad`)

``` r
res_tad(df)
```

![](img/everyfunction--unnamed-chunk-9-1.png)

### Residuals versus population predicted (`res_pred`)

``` r
res_pred(df)
```

![](img/everyfunction--unnamed-chunk-10-1.png)

### RES versus continuous covariate (`res_cont`)

``` r
res_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-11-1.png)

### RES by categorical covariate (`res_cat`)

``` r
res_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-12-1.png)

Weighted residuals
------------------

### Weighted residuals versus time (`wres_time`)

``` r
wres_time(df) 
```

![](img/everyfunction--unnamed-chunk-13-1.png)

### Weighted residuals versus time after dose (`wres_tad`)

``` r
wres_tad(df)
```

![](img/everyfunction--unnamed-chunk-14-1.png)

### Weighted esiduals versus population predicted (`wres_pred`)

``` r
wres_pred(df)
```

![](img/everyfunction--unnamed-chunk-15-1.png)

### WRES versus continuous covariate (`wres_cont`)

``` r
wres_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-16-1.png)

### WRES by categorical covariate (`wres_cat`)

``` r
wres_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-17-1.png)

### WRES QQ plot (`wres_q`)

``` r
wres_q(df)
```

![](img/everyfunction--unnamed-chunk-18-1.png)

Conditional weighted residuals (CWRES)
--------------------------------------

### CWRES versus time (`cwres_time`)

``` r
cwres_time(df)
```

![](img/everyfunction--unnamed-chunk-19-1.png)

### CWRES versus time after dose (`cwres_tad`)

``` r
cwres_tad(df)
```

![](img/everyfunction--unnamed-chunk-20-1.png)

### CWRES versus continuous covariate (`cwres_cont`)

``` r
cwres_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-21-1.png)

### CWRES by categorical covariate (`cwres_cat`)

``` r
cwres_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-22-1.png)

``` r
cwres_cat(df, x="STUDYc//Study type", shown=FALSE)
```

![](img/everyfunction--unnamed-chunk-23-1.png)

### CWRES versus population predicted (`cwres_pred`)

``` r
cwres_pred(df)
```

![](img/everyfunction--unnamed-chunk-24-1.png)

### CWRES QQ plot (`cwres_q`)

``` r
cwres_q(df)
```

![](img/everyfunction--unnamed-chunk-25-1.png)

ETA plots
=========

ETA versus continuous covariates (`eta_cont`)
---------------------------------------------

``` r
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_cont(df, x="WT//Weight (kg)",y=etas)
```

``` r
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")
```

![](img/everyfunction--unnamed-chunk-27-1.png)

ETA by categorical covariates (`eta_cat`)
-----------------------------------------

``` r
p <- eta_cat(df, x="STUDYc//Study type", y=etas)
```

``` r
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")
```

![](img/everyfunction--unnamed-chunk-29-1.png)

ETA histograms (`eta_hist`)
---------------------------

``` r
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_hist(df,etas, bins=10)
```

``` r
mrggdraw(p, ncol=2, arrange=TRUE, script="everyfunction.R")
```

![](img/everyfunction--unnamed-chunk-31-1.png)

DV versus time (`dv_time`)
==========================

Basic plot
----------

``` r
dv_time(df, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-32-1.png)

Faceted
-------

``` r
dv_time(df, yname="NoDoze (ng/mL)") +
  facet_wrap(~DOSE, scales="free_x")
```

![](img/everyfunction--unnamed-chunk-33-1.png)

Colored
-------

``` r
dv_time(df, yname="NoDoze (ng/mL)", col="STUDYc") +
  facet_wrap(~DOSE, scales="free")
```

![](img/everyfunction--unnamed-chunk-34-1.png)

log-Scale
---------

``` r
dv_time(df, yname="NoDoze (ng/mL)", log=TRUE) +
  facet_wrap(~STUDYc)
```

![](img/everyfunction--unnamed-chunk-35-1.png)

Data summary
============

Continuous variable by categorical variable (`cont_cat`)
--------------------------------------------------------

``` r
cont_cat(df, x="STUDYc//Study name", y="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-36-1.png)

Split and plot (`split_plot`)
-----------------------------

``` r
p <- split_plot(df, sp="STUDYc", fun=dv_ipred)
```

``` r
mrggdraw(p, arrange=TRUE, script="everyfunction.R")
```

![](img/everyfunction--unnamed-chunk-38-1.png)

Plot output (`mrggsave`)
========================

Saving single plots
-------------------

``` r
p1 <- dv_pred(df)
```

``` r
mrggsave(p1, script="everyfunction.R", stem="figure1")
```

Save multiple plots to one file
-------------------------------

``` r
p2 <- dv_ipred(df)
p3 <- cwres_pred(df)
p4 <- cwres_time(df)
```

``` r
mrggsave(list(p1,p2,p3,p4), script="everyfunction.R", stem="figure1")
```

Arrange multiple plots on a single page
---------------------------------------

``` r
mrggdraw(list(p1,p2,p3,p4), arrange=TRUE, script="everyfunction.R")
```

![](img/everyfunction--unnamed-chunk-43-1.png)
