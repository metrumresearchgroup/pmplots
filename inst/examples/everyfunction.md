Available functions
================

-   [Example data in the package](#example-data-in-the-package)
-   [`col//title` specification](#coltitle-specification)
    -   [Fill in `CWRES` if it doesn't exist](#fill-in-cwres-if-it-doesnt-exist)
-   [Observed vs predicted](#observed-vs-predicted)
    -   [Observed versus population predicted (`dv_pred`)](#observed-versus-population-predicted-dv_pred)
        -   [Observed versus population predicted - log/log](#observed-versus-population-predicted---loglog)
    -   [Observed versus individual predicted (`dv_ipred`)](#observed-versus-individual-predicted-dv_ipred)
        -   [Observed versus individual predicted - log/log](#observed-versus-individual-predicted---loglog)
        -   [Observed versus both PRED and IPRED](#observed-versus-both-pred-and-ipred)
-   [Residual plots](#residual-plots)
    -   [Residuals](#residuals)
        -   [Residuals versus time (`res_time`)](#residuals-versus-time-res_time)
        -   [Residuals versus time after first dose (`res_tafd`)](#residuals-versus-time-after-first-dose-res_tafd)
        -   [Residuals versus time after dose (`res_tad`)](#residuals-versus-time-after-dose-res_tad)
        -   [Residuals versus population predicted (`res_pred`)](#residuals-versus-population-predicted-res_pred)
        -   [RES versus continuous covariate (`res_cont`)](#res-versus-continuous-covariate-res_cont)
        -   [RES by categorical covariate (`res_cat`)](#res-by-categorical-covariate-res_cat)
        -   [Residual histogram (`res_hist`)](#residual-histogram-res_hist)
    -   [Weighted residuals](#weighted-residuals)
        -   [Weighted residuals versus time (`wres_time`)](#weighted-residuals-versus-time-wres_time)
        -   [Weighted residuals versus time after first dose (`wres_tafd`)](#weighted-residuals-versus-time-after-first-dose-wres_tafd)
        -   [Weighted residuals versus time after dose (`wres_tad`)](#weighted-residuals-versus-time-after-dose-wres_tad)
        -   [Weighted esiduals versus population predicted (`wres_pred`)](#weighted-esiduals-versus-population-predicted-wres_pred)
        -   [WRES versus continuous covariate (`wres_cont`)](#wres-versus-continuous-covariate-wres_cont)
        -   [WRES by categorical covariate (`wres_cat`)](#wres-by-categorical-covariate-wres_cat)
        -   [Weighted residual histogram (`wres_hist`)](#weighted-residual-histogram-wres_hist)
        -   [WRES QQ plot (`wres_q`)](#wres-qq-plot-wres_q)
    -   [Conditional weighted residuals (CWRES)](#conditional-weighted-residuals-cwres)
        -   [CWRES versus time (`cwres_time`)](#cwres-versus-time-cwres_time)
        -   [Conditional weighted residuals versus time after first dose (`cwres_tafd`)](#conditional-weighted-residuals-versus-time-after-first-dose-cwres_tafd)
        -   [CWRES versus time after dose (`cwres_tad`)](#cwres-versus-time-after-dose-cwres_tad)
        -   [CWRES versus continuous covariate (`cwres_cont`)](#cwres-versus-continuous-covariate-cwres_cont)
        -   [CWRES by categorical covariate (`cwres_cat`)](#cwres-by-categorical-covariate-cwres_cat)
        -   [Conditional weighted residual histogram (`cwres_hist`)](#conditional-weighted-residual-histogram-cwres_hist)
        -   [CWRES versus population predicted (`cwres_pred`)](#cwres-versus-population-predicted-cwres_pred)
        -   [CWRES QQ plot (`cwres_q`)](#cwres-qq-plot-cwres_q)
-   [NPDE plots](#npde-plots)
    -   [NPDE versus TIME (`npde_time`, `npde_tad`, `npde_tafd`)](#npde-versus-time-npde_time-npde_tad-npde_tafd)
    -   [NPDE versus TAD (`npde_tad`)](#npde-versus-tad-npde_tad)
    -   [NPDE versus TAFD (`npde_tafd`)](#npde-versus-tafd-npde_tafd)
    -   [NPDE versus PRED (`npde_pred`)](#npde-versus-pred-npde_pred)
    -   [NPDE versus continuous variable (`npde_cont`)](#npde-versus-continuous-variable-npde_cont)
    -   [NPDE versus categorical variable (`npde_cat`)](#npde-versus-categorical-variable-npde_cat)
    -   [QQ-plot with NPDE (`npde_q`)](#qq-plot-with-npde-npde_q)
    -   [NPDE histogram (`npde_hist`)](#npde-histogram-npde_hist)
-   [ETA plots](#eta-plots)
    -   [ETA versus continuous covariates (`eta_cont`)](#eta-versus-continuous-covariates-eta_cont)
        -   [Grouped by eta](#grouped-by-eta)
        -   [Grouped by covariate](#grouped-by-covariate)
    -   [ETA by categorical covariates (`eta_cat`)](#eta-by-categorical-covariates-eta_cat)
    -   [ETA histograms (`eta_hist`)](#eta-histograms-eta_hist)
    -   [ETA pairs plot (`eta_pairs`)](#eta-pairs-plot-eta_pairs)
-   [DV versus time (`dv_time`)](#dv-versus-time-dv_time)
    -   [Basic plot](#basic-plot)
    -   [Faceted](#faceted)
    -   [log-Scale](#log-scale)
    -   [Wrapped plots](#wrapped-plots)
        -   [histogram](#histogram)
        -   [eta](#eta)
        -   [DV/PRED, DV/IPRED](#dvpred-dvipred)
        -   [Use labels in the strip](#use-labels-in-the-strip)
-   [Data summary](#data-summary)
    -   [Continuous variable by categorical variable (`cont_cat`)](#continuous-variable-by-categorical-variable-cont_cat)
    -   [General histogram (`cont_hist`)](#general-histogram-cont_hist)
    -   [Split and plot (`split_plot`)](#split-and-plot-split_plot)
-   [Some customization](#some-customization)
    -   [Greek letters in axis title](#greek-letters-in-axis-title)
    -   [Modify x-axis](#modify-x-axis)
    -   [Modify y-axis](#modify-y-axis)
    -   [Add layers](#add-layers)
        -   [smooth](#smooth)
        -   [abline](#abline)
    -   [Drop extra layers](#drop-extra-layers)
        -   [Drop all extra layers](#drop-all-extra-layers)
    -   [Custom breaks](#custom-breaks)
    -   [Extra reference lines to \[C\]WRES plots](#extra-reference-lines-to-cwres-plots)
-   [Replicate look and feel](#replicate-look-and-feel)
    -   [Theme](#theme)
        -   [Plain](#plain)
    -   [Smooth](#smooth-1)
    -   [Abline](#abline-1)
    -   [Horizontal reference line](#horizontal-reference-line)
    -   [Rotate x and y axis labels](#rotate-x-and-y-axis-labels)
-   [Standard axis titles](#standard-axis-titles)
-   [Log breaks](#log-breaks)

``` r
library(pmplots)
library(dplyr)
library(mrggsave)
library(purrr)
```

Example data in the package
===========================

``` r
df <- pmplots_data_obs() %>% mutate(CWRES = CWRESI)

id <- pmplots_data_id()

dayx <- defx(breaks = seq(0,168,24))

.yname <- "MRG1557 (ng/mL)"

etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")

covs <- c("WT//Weight (kg)", "ALB//Albumin (g/dL)", "SCR//Creatinine (mg/dL)")
```

Override the `df` and `id` objects in the above chunk

``` r
## Nothing here
```

`col//title` specification
==========================

This is a way to specify the column name for source data along with the axis label

``` r
col_label("CL//Clearance (L)")
```

    . [1] "CL"            "Clearance (L)"

When only the column is given, then the column name will be used for the column title:

``` r
col_label("WT")
```

    . [1] "WT" "WT"

Fill in `CWRES` if it doesn't exist
-----------------------------------

``` r
dat <- mutate(df, CWRES = NULL)

cwresi_time(df)
```

![](img/everyfunction--unnamed-chunk-6-1.png)

``` r
cwres_time(dat)
```

![](img/everyfunction--unnamed-chunk-6-2.png)

Observed vs predicted
=====================

Observed versus population predicted (`dv_pred`)
------------------------------------------------

``` r
dv_pred(df, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-7-1.png)

### Observed versus population predicted - log/log

``` r
dv_pred(df, loglog=TRUE, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-8-1.png)

Observed versus individual predicted (`dv_ipred`)
-------------------------------------------------

``` r
dv_ipred(df, yname=.yname)
```

![](img/everyfunction--unnamed-chunk-9-1.png)

### Observed versus individual predicted - log/log

``` r
dv_ipred(df, loglog=TRUE, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-10-1.png)

### Observed versus both PRED and IPRED

``` r
dv_preds(df) %>% pm_grid(ncol=2)
```

![](img/everyfunction--unnamed-chunk-11-1.png)

Residual plots
==============

Residuals
---------

### Residuals versus time (`res_time`)

``` r
res_time(df)
```

![](img/everyfunction--unnamed-chunk-12-1.png)

### Residuals versus time after first dose (`res_tafd`)

``` r
res_tafd(df)
```

![](img/everyfunction--unnamed-chunk-13-1.png)

### Residuals versus time after dose (`res_tad`)

``` r
res_tad(df)
```

![](img/everyfunction--unnamed-chunk-14-1.png)

### Residuals versus population predicted (`res_pred`)

``` r
res_pred(df)
```

![](img/everyfunction--unnamed-chunk-15-1.png)

### RES versus continuous covariate (`res_cont`)

``` r
res_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-16-1.png)

This function is also vectorized in x.

``` r
c("WT", "CRCL", "AST") %>% map(.f = partial(res_cont,df)) %>% pm_grid()
```

![](img/everyfunction--unnamed-chunk-17-1.png)

### RES by categorical covariate (`res_cat`)

``` r
dplyr::count(df, STUDYc)
```

    . # A tibble: 4 x 2
    .   STUDYc      n
    .   <fct>   <int>
    . 1 SAD       424
    . 2 MAD      1199
    . 3 Renal     960
    . 4 Hepatic   559

``` r
res_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-18-1.png)

### Residual histogram (`res_hist`)

``` r
res_hist(df)
```

![](img/everyfunction--unnamed-chunk-19-1.png)

Weighted residuals
------------------

### Weighted residuals versus time (`wres_time`)

``` r
wres_time(df) 
```

![](img/everyfunction--unnamed-chunk-20-1.png)

### Weighted residuals versus time after first dose (`wres_tafd`)

``` r
wres_tafd(df)
```

![](img/everyfunction--unnamed-chunk-21-1.png)

### Weighted residuals versus time after dose (`wres_tad`)

``` r
wres_tad(df)
```

![](img/everyfunction--unnamed-chunk-22-1.png)

### Weighted esiduals versus population predicted (`wres_pred`)

``` r
wres_pred(df)
```

![](img/everyfunction--unnamed-chunk-23-1.png)

### WRES versus continuous covariate (`wres_cont`)

This function is also vectorized in x.

``` r
wres_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-24-1.png)

### WRES by categorical covariate (`wres_cat`)

``` r
wres_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-25-1.png)

### Weighted residual histogram (`wres_hist`)

``` r
wres_hist(df)
```

![](img/everyfunction--unnamed-chunk-26-1.png)

### WRES QQ plot (`wres_q`)

``` r
wres_q(df)
```

![](img/everyfunction--unnamed-chunk-27-1.png)

Conditional weighted residuals (CWRES)
--------------------------------------

### CWRES versus time (`cwres_time`)

``` r
cwres_time(df)
```

![](img/everyfunction--unnamed-chunk-28-1.png)

### Conditional weighted residuals versus time after first dose (`cwres_tafd`)

``` r
cwres_tafd(df)
```

![](img/everyfunction--unnamed-chunk-29-1.png)

### CWRES versus time after dose (`cwres_tad`)

``` r
cwres_tad(df)
```

![](img/everyfunction--unnamed-chunk-30-1.png)

### CWRES versus continuous covariate (`cwres_cont`)

``` r
cwres_cont(df, x="WT//Weight (kg)")
```

![](img/everyfunction--unnamed-chunk-31-1.png)

Vectorized version

``` r
cwres_cont(df, covs) %>%  pm_grid(ncol=2)
```

![](img/everyfunction--unnamed-chunk-32-1.png)

### CWRES by categorical covariate (`cwres_cat`)

``` r
cwres_cat(df, x="STUDYc//Study type")
```

![](img/everyfunction--unnamed-chunk-33-1.png)

``` r
cwres_cat(df, x="STUDYc//Study type", shown=FALSE)
```

![](img/everyfunction--unnamed-chunk-34-1.png)

Vectorized version

``` r
cwres_cat(df, x = c("STUDYc//Study", "RF//Renal Function"))
```

    . [[1]]

![](img/everyfunction--unnamed-chunk-35-1.png)

    . 
    . [[2]]

![](img/everyfunction--unnamed-chunk-35-2.png)

### Conditional weighted residual histogram (`cwres_hist`)

``` r
cwres_hist(df)
```

![](img/everyfunction--unnamed-chunk-36-1.png)

### CWRES versus population predicted (`cwres_pred`)

``` r
cwres_pred(df)
```

![](img/everyfunction--unnamed-chunk-37-1.png)

### CWRES QQ plot (`cwres_q`)

``` r
cwres_q(df)
```

![](img/everyfunction--unnamed-chunk-38-1.png)

NPDE plots
==========

NPDE versus TIME (`npde_time`, `npde_tad`, `npde_tafd`)
-------------------------------------------------------

``` r
npde_time(df)
```

![](img/everyfunction--unnamed-chunk-39-1.png)

NPDE versus TAD (`npde_tad`)
----------------------------

``` r
npde_tad(df)
```

![](img/everyfunction--unnamed-chunk-40-1.png)

NPDE versus TAFD (`npde_tafd`)
------------------------------

``` r
npde_tafd(df)
```

![](img/everyfunction--unnamed-chunk-41-1.png)

NPDE versus PRED (`npde_pred`)
------------------------------

``` r
npde_pred(df)
```

![](img/everyfunction--unnamed-chunk-42-1.png)

NPDE versus continuous variable (`npde_cont`)
---------------------------------------------

``` r
npde_cont(df, "WT")
```

![](img/everyfunction--unnamed-chunk-43-1.png)

NPDE versus categorical variable (`npde_cat`)
---------------------------------------------

``` r
npde_cat(df, "STUDYc")
```

![](img/everyfunction--unnamed-chunk-44-1.png)

QQ-plot with NPDE (`npde_q`)
----------------------------

``` r
npde_q(df)
```

![](img/everyfunction--unnamed-chunk-45-1.png)

NPDE histogram (`npde_hist`)
----------------------------

``` r
npde_hist(df)
```

![](img/everyfunction--unnamed-chunk-46-1.png)

ETA plots
=========

``` r
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
covs <- c("WT//Weight (kg)", "ALB//Albumin (g/dL)", "SCR//Creatinine (mg/dL)")
```

ETA versus continuous covariates (`eta_cont`)
---------------------------------------------

### Grouped by eta

``` r
eta_cont(id, x=covs,y=etas[2]) %>% pm_grid()
```

![](img/everyfunction--unnamed-chunk-48-1.png)

### Grouped by covariate

``` r
eta_cont(id, x=covs[1], y=etas) %>% pm_grid(ncol=2)
```

![](img/everyfunction--unnamed-chunk-49-1.png)

ETA by categorical covariates (`eta_cat`)
-----------------------------------------

``` r
p <- eta_cat(id, x="STUDYc//Study type", y=etas)
```

``` r
pm_grid(p)
```

![](img/everyfunction--unnamed-chunk-51-1.png)

ETA histograms (`eta_hist`)
---------------------------

``` r
etas <- c("ETA1//ETA-CL", "ETA2//ETA-V2", "ETA3//ETA-KA")
p <- eta_hist(id,etas, bins=10)
```

``` r
pm_grid(p)
```

![](img/everyfunction--unnamed-chunk-53-1.png)

ETA pairs plot (`eta_pairs`)
----------------------------

``` r
p <- eta_pairs(id,etas)
```

``` r
print(p)
```

![](img/everyfunction--unnamed-chunk-55-1.png)

DV versus time (`dv_time`)
==========================

Basic plot
----------

``` r
dv_time(df, yname = .yname)
```

![](img/everyfunction--unnamed-chunk-56-1.png)

Faceted
-------

``` r
dv_time(df, yname="MRG1557 (ng/mL)") + facet_wrap(~DOSE, scales="free_x")
```

![](img/everyfunction--unnamed-chunk-57-1.png)

**NOTE** this will not work as you expect; the labels are wrong.

``` r
cwres_cat(df, x = "STUDYc") + facet_wrap(~CPc)
```

![](img/everyfunction--unnamed-chunk-58-1.png)

The only way to get this right is

``` r
cwres_cat(df, x = "STUDYc", shown=FALSE) + facet_wrap(~CPc)
```

![](img/everyfunction--unnamed-chunk-59-1.png)

log-Scale
---------

``` r
dv_time(df, yname="MRG1557 (ng/mL)", log=TRUE) + facet_wrap(~STUDYc)
```

![](img/everyfunction--unnamed-chunk-60-1.png)

Wrapped plots
-------------

### histogram

``` r
wrap_hist(df, x = c("WT", "ALB", "SCR"), scales = "free", bins=10, ncol=2)
```

![](img/everyfunction--unnamed-chunk-61-1.png)

### eta

``` r
wrap_eta_cont(df, y = "ETA1", x = c("WT", "ALB"), scales="free_x")
```

![](img/everyfunction--unnamed-chunk-62-1.png)

### DV/PRED, DV/IPRED

``` r
wrap_dv_preds(df, ncol=1)
```

![](img/everyfunction--unnamed-chunk-63-1.png)

### Use labels in the strip

``` r
wrap_eta_cont(
  df, 
  y = "ETA1", 
  x = c("WT//Weight (kg)", "ALB//Albumin (g/dL)"),
  scales="free_x", 
  use_labels=TRUE
)
```

![](img/everyfunction--unnamed-chunk-64-1.png) \#\# Residuals

``` r
wrap_res_time(df, y = c("RES", "CWRES", "NPDE"), ncol=2)
```

![](img/everyfunction--unnamed-chunk-65-1.png)

Data summary
============

Continuous variable by categorical variable (`cont_cat`)
--------------------------------------------------------

``` r
cont_cat(id, x="STUDYc", y="WT")
```

![](img/everyfunction--unnamed-chunk-66-1.png)

General histogram (`cont_hist`)
-------------------------------

``` r
cont_hist(id, x = "WT", bins = 20)
```

![](img/everyfunction--unnamed-chunk-67-1.png)

Split and plot (`split_plot`)
-----------------------------

``` r
p <- split_plot(df, sp="STUDYc", fun=dv_ipred)
```

``` r
pm_grid(p)
```

![](img/everyfunction--unnamed-chunk-69-1.png)

Some customization
==================

Greek letters in axis title
---------------------------

``` r
dv_pred(df, x = "PRED//Concentration ($\\mu$g)")
```

![](img/everyfunction--unnamed-chunk-70-1.png)

Modify x-axis
-------------

``` r
a <- list(trans="log", breaks = logbr3())

dv_time(df, xs=a)
```

![](img/everyfunction--unnamed-chunk-71-1.png)

Modify y-axis
-------------

``` r
dv_time(df, ys=a, yname="Y-axis name")
```

![](img/everyfunction--unnamed-chunk-72-1.png)

Add layers
----------

``` r
p <- ggplot(df, aes(PRED,DV))  + geom_point() + pm_theme()
```

### smooth

``` r
layer_s(p)
```

![](img/everyfunction--unnamed-chunk-74-1.png)

### abline

``` r
layer_a(p)
```

![](img/everyfunction--unnamed-chunk-75-1.png)

``` r
layer_h(cwres_time(df,add_layers=FALSE))
```

![](img/everyfunction--unnamed-chunk-76-1.png)

Drop extra layers
-----------------

``` r
dv_pred(df, smooth=NULL)
```

![](img/everyfunction--unnamed-chunk-77-1.png)

``` r
dv_pred(df, abline=NULL)
```

![](img/everyfunction--unnamed-chunk-78-1.png)

``` r
cwres_time(df, hline = NULL)
```

![](img/everyfunction--unnamed-chunk-79-1.png)

``` r
dv_pred(df, abline=NULL, smooth = NULL)
```

![](img/everyfunction--unnamed-chunk-80-1.png)

### Drop all extra layers

``` r
dv_pred(df, add_layers=FALSE)
```

![](img/everyfunction--unnamed-chunk-81-1.png)

Custom breaks
-------------

Default breaks:

``` r
dv_time(df)
```

![](img/everyfunction--unnamed-chunk-82-1.png)

Break every 3 days

``` r
dv_time(df, xby=72)
```

![](img/everyfunction--unnamed-chunk-83-1.png)

Custom breaks and limits

``` r
a <- list(br = seq(0,240,48), limits=c(0,240))
dv_time(df, xs=a)
```

![](img/everyfunction--unnamed-chunk-84-1.png)

Extra reference lines to \[C\]WRES plots
----------------------------------------

``` r
wres_time(df) + geom_3s()
```

![](img/everyfunction--unnamed-chunk-85-1.png)

Replicate look and feel
=======================

``` r
p <- ggplot(df, aes(IPRED,DV)) + geom_point()

p
```

![](img/everyfunction--unnamed-chunk-86-1.png)

Theme
-----

``` r
p + pm_theme()
```

![](img/everyfunction--unnamed-chunk-87-1.png)

### Plain

``` r
p + theme_plain()
```

![](img/everyfunction--unnamed-chunk-88-1.png)

Smooth
------

``` r
p + pm_smooth()
```

![](img/everyfunction--unnamed-chunk-89-1.png)

Abline
------

``` r
p + pm_abline()
```

![](img/everyfunction--unnamed-chunk-90-1.png)

Horizontal reference line
-------------------------

``` r
ggplot(df, aes(TIME,CWRES)) + geom_point() + pm_hline()
```

![](img/everyfunction--unnamed-chunk-91-1.png)

Rotate x and y axis labels
--------------------------

``` r
dv_pred(df) + rot_x(angle = 90) + rot_y()
```

![](img/everyfunction--unnamed-chunk-92-1.png)

Standard axis titles
====================

``` r
pm_axis_time()
```

    . [1] "TIME//Time {xunit}"

``` r
pm_axis_tad()
```

    . [1] "TAD//Time after dose {xunit}"

``` r
pm_axis_tafd()
```

    . [1] "TAFD//Time after first dose {xunit}"

``` r
pm_axis_res()
```

    . [1] "RES//Residual"

``` r
pm_axis_wres()
```

    . [1] "WRES//Weighted residual"

``` r
pm_axis_cwres()
```

    . [1] "CWRES//Conditional weighted residual"

``` r
pm_axis_npde()
```

    . [1] "NPDE//NPDE"

``` r
pm_axis_dv()
```

    . [1] "DV//Observed {yname}"

``` r
pm_axis_pred()
```

    . [1] "PRED//Population predicted {xname}"

``` r
pm_axis_ipred()
```

    . [1] "IPRED//Individual predicted {xname}"

Log breaks
==========

``` r
logbr3()
```

    .  [1] 1e-10 3e-10 1e-09 3e-09 1e-08 3e-08 1e-07 3e-07 1e-06 3e-06 1e-05
    . [12] 3e-05 1e-04 3e-04 1e-03 3e-03 1e-02 3e-02 1e-01 3e-01 1e+00 3e+00
    . [23] 1e+01 3e+01 1e+02 3e+02 1e+03 3e+03 1e+04 3e+04 1e+05 3e+05 1e+06
    . [34] 3e+06 1e+07 3e+07 1e+08 3e+08 1e+09 3e+09 1e+10 3e+10

``` r
logbr()
```

    .  [1] 1e-10 1e-09 1e-08 1e-07 1e-06 1e-05 1e-04 1e-03 1e-02 1e-01 1e+00
    . [12] 1e+01 1e+02 1e+03 1e+04 1e+05 1e+06 1e+07 1e+08 1e+09 1e+10
