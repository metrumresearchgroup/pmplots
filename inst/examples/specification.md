
# Input data specification

## Data column names

These are default column names.  All of the defaults can easily be
changed with function arguments.

- `CWRES` conditional weighted residual
- `WRES` weighted residual
- `RES` residual
- `TIME` time after first dose
- `TAD` time after dose
- `PRED` population predicted value
- `IPRED` individual predicted value
- `DV` observed data

## Other default names

- `hr` default time unit


## Default style

- Data point color: `black`
- Loess line: `dashed` and `blue`
- Line of identity: `black`
- `residual` is always singular not plural (e.g. "Weighted residual" rather than "Weighted residuals")
- Legend is located at the top of the plot
- x scales are modified 


# Plot specifictions

## Any plot
- any continuous x-scale can be modified by
passing a list as `xs` containing arguments to 
`scale_x_continuous`
- any continuous y-scale can be modified by
passing a list as `ys` containing arguments to
`scale_y_continuous`
- any discrete x-scale can be modified by
passing a list as `xs` containing arguments
to `scale_x_discrete`

## DV/PRED
- includes `dv_pred` and `dv_ipred`
- x- and y-scales have the same limits
- a line of identity is drawn (can be omitted)
- a loess line (blue, dashed) is drawn (can be omitted)
- switch to make both scales log-transformed

## RESIDUAL/TIME
- includes `res_time`, `wres_time`, `cwres_time`, `res_tad`, 
`wres_tad`, `cwres_tad`
- time unit can be changed via `xunit` argument

## RESIDUAL/Continuous value
- includes `res_cont`, `wres_cont`, `cwres_cont`, 
`cont_cont`, `res_pred`, `wres_pred`, `cwres_pred`
- a loess line (blue, dashed) is drawn (can be omitted)
- other reference lines are added outside of 
the function

## RESIDUAL/Categorical value
- includes `res_cat`, `wres_cat`, `cwres_cat`, 
`cont_cat`, `eta_cat`

## ETA/Continuous value
- includes `eta_cont`
- a single continuous value is specified in 
`col//short title` format (e.g. `WT//Weight (kg)`)
- the continuous value must be a numeric column
in the data frame, or an error is generated

## ETA/Categorical value
- includes `eta_cat`
- a single categorical value is specified in 
`col//short title` format (eg. `RFSTAGE//Renal function stage`)
- the categorical value must be either 
factor, character, or logical value in 
the data frame, or an error is generated


## QQ
- includes `res_q`, `wres_q`, `cwres_q`
- 


## Histograms
- includes `eta_hist`
- etas are specified in `col//title`
format (e.g. `ETA1//ETA-KA`)

