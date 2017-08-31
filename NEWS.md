
# pmplots 0.0.1.9000

## Data column names

These are default column names.  All 
default columns are numeric.  
All of the defaults can easily be
changed with function arguments.

- `CWRES` conditional weighted residual
- `WRES` weighted residual
- `RES` residual
- `TIME` time after first dose
- `TAD` time after dose
- `PRED` population predicted value
- `IPRED` individual predicted value
- `DV` observed data

## Other defaults

- `hr` default time unit

## Default style

- Data point color: `black`
- Loess line: `dashed` and `blue`
- Line of identity: `black`
- `residual` is always singular not plural (e.g. "Weighted residual" rather
than "Weighted residuals")
- Legend is located at the top of the plot
- x scales are modified

## Plot specifictions

### Any plot

- any continuous x-scale can be modified by
passing a list as `xs` containing arguments to
`scale_x_continuous`
- any continuous y-scale can be modified by
passing a list as `ys` containing arguments to
`scale_y_continuous`
- any discrete x-scale can be modified by
passing a list as `xs` containing arguments
to `scale_x_discrete`
- The input data set should only include 
rows that contributed actual observations that 
influenced the estimates. In general, dosing
records and BQL records should be discarded.
However, the contents may vary across 
different applications.  

### DV/PRED

- includes `dv_pred` and `dv_ipred`
- x- and y-values must be numeric and must exist
in the data frame, or an error is generated
- x- and y-scales have the same limits
- a line of identity is drawn (can be omitted)
- a loess line (blue, dashed) is drawn (can be omitted)
- switch to make both scales log-transformed

### RESIDUAL/TIME

- includes `res_time`, `wres_time`, `cwres_time`, `res_tad`,
`wres_tad`, `cwres_tad`
- x- and y-values must be numeric and must exist
in the data frame, or an error is generated
- time unit can be changed via `xunit` argument

### ETA or continuous value/Continuous value

- includes `eta_cont`, `cont_cont`,
`res_cont`, `wres_cont`, `cwres_cont`,
`res_pred`, `wres_pred`, `cwres_pred`
- a single continuous value is specified in
`col//short title` format (e.g. `WT//Weight (kg)`)
- x- and y-values must be numeric and must exist
in the data frame, or an error is generated
- a loess line (blue, dashed) is drawn (can be omitted)
- other reference lines are added outside of
the function

### ETA or continuous variable/Categorical value

- includes `eta_cat`, `cont_cat`, `res_cat`,
`wres_cat`, `cwres_cat`
- a single categorical value is specified in
`col//short title` format (eg. `RFSTAGE//Renal function stage`)
- y-value must be numeric and must exist
in the data frame, or an error is generated
- the categorical value must be either
factor, character, or logical value in
the data frame, or an error is generated
- By default, the number of rows with 
non-NA values (based on the y-column) in each box
is included under each x-axis tick label; this
can be suppressed by function argument

### QQ

- includes `wres_q`, `cwres_q`
- both `wres` and `cwres` must be numeric
and exist in the data frame, or an error is 
generated
- a line of identity is included
- points are in blue
- y-label is `[C]WRES distribution quantile`
- x-label is `Standard normal quantile`

### Histograms

- includes `eta_hist`
- etas are specified in `col//title`
format (e.g. `ETA1//ETA-KA`)
- etas must be numeric and must exist in the 
data frame, or an error is generated
- `fill`, `col` and `alpha` can be set
through function arguments which are
passed to `geom_histogram`
