
Introduction
============

`pmplots` is an `R` package to generate exploratory and diagnostic plots commonly of interest in pharamcometrics. Each function in `pmplots` is named according to the specific plot it generates via calls to functions in the `ggplot2` `R` package.

This document lists the functional requirements for the `pmplots` package.

Requirements for pharamcometric plotting package `pmplots`
==========================================================

<table>
<thead>
<tr>
<th style="text-align:left;">
Section header
</th>
<th style="text-align:right;">
RID
</th>
<th style="text-align:left;">
Requirement
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 6cm; ">
Default column names
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Column `RES` refers to residual; rendered `res` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Column `WRES` weighted residual; rendered `wres` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Column `CWRES` refers to conditional weighted residual; rendered `cwres` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Column `TIME` refers to model time; rendered `time` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Column `TAFD` refers to time after first dose; rendered `tafd` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Column `TAD` refers to time after dose; rendered `tad` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Column `DV` refers to observed data; rendered `dv` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Column `PRED` refers to population level predictions; rendered `pred` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Column `IPRED` refers to individual level predictions; rendered `ipred` in function names
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
Plots generated
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Functions `dv_time`, `dv_tafd`, and `dv_tad` plot `DV` versus the appropriate time measure; both lines and points are plotted
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Functions `dv_pred` and `dv_ipred` plot `DV` versus the appropriate predicted value; a line of identity is added as well as a loess smothing line; both the x- and y-axis maybe be transformed to log scale with the `loglog` argument; if `loglog` is used, only positive values are retained for the plot
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Functions `res_time`, `res_tafd`, and `res_tad` plots residual versus the appropriate time measure; a reference line is added at `res=0` as well as a loess smoothing line
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
Functions `wres_time`, `wres_tafd`, and `wres_tad` plots weighted residual versus the appropriate time measure; a reference line is added at `wres=0` as well as a loess smoothing line
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Functions `cwres_time`, `cwres_tafd`, and `cwres_tad` plots conditional weighted residual versus the appropriate time measure; a reference line is added at `cwres=0` as well as a loess smoothing line
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Functions `res_pred`, `wres_pred` and `cwres_pred` plot the appropriate residual versus population model predictions (`PRED`); a horizontal reference line is added at `c/w/res=0` as well as a loess smoothing line
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
Functions `res_cont`, `wres_cont`, and `cwres_cont` plot the appropriate residual versus a continuous covariate in the data set; a horizontal reference line is added at `c/w/res=0` as well as a loess smoothing line
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Functions `res_cat`, `wres_cat`, and `cwres_cat` makes a boxplot of the appropriate residual versus a categorical data set column
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Function `wres_q` and `cwres_q` generates quantile-quantile plots of the appropriate residual value; a refereince identity line is added
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Function `eta_hist` generates histograms of model ETAs and returns a list `gg/ggplot` objects
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Function `eta_cont` generates a scatterplot of model ETAs versus a continuous variable in the data set; a horizontal reference line at `ETAn=0` and loess smoothing line are also added to the plot
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
Function `eta_cat` generates boxplot summaries of model ETAs by a categorical variable in the data set
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Function `eta_pairs` generates pairs plots using the `ggpairs` function from the `GGally` package
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Function `splitplot` splits the input data set according to a discrete data item and generates a plot according to a user-named function, returning a list of `gg/ggplot` objects
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
Continuous scatter
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
x-axis options availabe in `x_scale_continuous` can be modified by the xs argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
y-axis options available in `y_scale_continuous` can be modified by the ys argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
When loess smoothing lines are generated, `geom_smooth` with `ggplot2` default behavior is used; the smooth may be modified through the `smooth` argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
A title may be added through the `title` argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
Boxplot summaries
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
x-axis options availabe in `x_scale_discrete` can be modified by the xs argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
y-axis options available in `y_scale_continuous` can be modified by the ys argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Boxplot summaries are generated using `geom_boxplot` with `ggplot2` default configuration
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
A title may be added thought the `title` argument
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
Input data
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
Data are input as data.frame or tibble
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
For continuous scatter plots, numeric data are required or an error is generated; data are considered discrete if they are `numeric` or `integer`
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
For boxplot summaries, discrete data are required for x-axis for boxplot summaries; data are considered discrete of they are `character`, `factor`, or `logical`
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
R packages
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Imports: `dplyr` `(&gt;= 0.7.2)`, `rlang` `(&gt;= 0.1.2)`
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Depends: `ggplot2` `(&gt;= 2.2.1)`
</td>
</tr>
<tr>
<td style="text-align:left;width: 6cm; ">
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Suggests: `testthat`
</td>
</tr>
</tbody>
</table>
