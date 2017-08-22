
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
- Loess line: `dashes` and `blue`
- Line of identity: `black`
- `residual` is always singular not plural (e.g. "Weighted residual" rather than
"Weighted residuals")
- Legend is located at the top of the plot
