# 0.1.0.9002
- Added `res_hist`, `wres_hist`, and `cwres_hist` along with 
the more general `cont_hist` functions for plotting residuals
or other continuous variables as histograms
- Vectorized `eta_cont`, `(cw)res_cont`, `eta_cat`, and
`(cw)res_cat`; see function help for more details about 
what this means
- Added `cont_cont_list` and `cont_cat_list` as vectorized
versions of `cont_cont` and `cont_cat`
- Added internal functions that facilitate the creation
of vectorized plotting function calls
- Reorganized the R source files and split several
composite help topics into more focused topics

# 0.1.0.9001

- Fixed `logbr3` so that the values are sorted; this 
fixes an issue where the grid lines were not propoerly
displayed with these breaks
- Fixed issue in `dv_time` and `dv_pred` when 
modifying the x-scale and y-scale through 
xs and ys arguments
- Changed `dv_time` and `dv_pred` so that 
breaks are at half-log units when `log = TRUE` or 
`loglog = TRUE`


# 0.1.0

- Initial validated version
