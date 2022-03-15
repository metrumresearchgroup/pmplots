# pmplots (development version)

- `pm_grid()` now uses 'patchwork' to arrange plots; 'patchwork' becomes
  a suggested package and 'cowplot' is dropped #49. 

- `scatt()` gains `alpha` argument with default provided by `pm_opts$scatter.alpha` 
  #42, #48. 

- `split_plot()` gains `labeller` argument which gets passed to `facet_wrap()` 
  #43, #47.

- `label_tex()` replaces `label_parse_label()` for labelling facets via 
  `latex2exp::TeX()`; `label_parse_label()` remains as an alias #47. 

- `label_fun` argument for wrap plots is renamed `labeller`; applies to 
  the following plots: `wrap_cont_cont()`, `wrap_hist()`, `wrap_cont_cat()`, 
  and any plot built on these (see `?wrap_plots`) #43, #47.

- Fix bug where points overlaid on boxplots were jittered in both directions; 
  jitter is now only in x-direction #45, #46. 

# pmplots 0.3.3

- Add vignettes into package (#37)

- Re-work README (#37)

# pmplots 0.3.2

- `pairs_plot()` and `eta_pairs()` now will properly call user-supplied
  functions for creating panels on the upper and lower triangle 
  (#2)

- The upper panels in `pairs_plot()` and `eta_pairs()` are not customizable
  using `pm_opts()` (#6)
  
- The reference lines at -3 and 3 on the y-axis of `npde` plots are no longer
  drawn by default (#22)
  
- `dv_pred()` and `dv_ipred()` gain an argument (`logbr`) that lets the user 
  pick the interval for tick marks when `loglog` plots are drawn; options are: 
  "full" (full log units - 10, 100 ,1000), "half" (half log units - 30, 100, 
  300) or "null" (let ggplot decide); the default is now to show full log unit
  breaks rather than half log unit breaks (#21)

- `pairs_plot()` gains an argument (`lower_plot`) which lets the user 
  more easily customize the scatter plot that usually appears on the lower
  triangle (#26)

- `wrap_cont_cat()` is a new wrap function that lets the user make 
  faceted plots of continuous variables versus categorical variables; 
  (#14)


# pmplots 0.3.1
- Add `label_fun` function to `pairs_plot`, `wrap_cont_cont` and 
  `wrap_hist` allowing strip labels to incorporate
  expressions via `pmplots:::label_parse` #45
- `parse_label` and `label_parse_label` are exported
- Add option system #70
- Add better control over tick label rotation and justification #60
- Optionally plot subject ID rather than points in scatter plots #63
- CWRESI plots now have default axis title "CWRES with interaction" #87
- Add dv_pred_ipred plot function #89
- Optionally overlay points on top of boxplots #13
- Refactor `col_label` so that punctuation in `col//label` does not 
  result in error #72

# pmplots 0.2.0
- Qualification under CR-88 and released to r_validated

# pmplots 0.1.0.9002
- Added non-exported functions to generate default col-label information 
(see `?pm_axis_functions`)
- Added `npde_time`, `npde_tad` and `npde_tafd`, 
`npde_pred`, `npde_hist`, and `npde_q` functions
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
- Added `pairs_plot` as a more-generic front end to 
`ggpairs`
- Dots are now passed through `eta_pairs`
and `pairs_plot` to `GGally::ggpairs`; this allows users to 
pass in a mapping for colors etc
- Added arguments for `pairs_plot` to allow users to pass in 
custom functions for `upper` and `lower`
- For the default `lower` function in `pairs_plot`, the mapping object is checked
for `smooth_color` and `smooth_lty` for more control over
the smoothing line
- Remove `shk` argument to `pairs_plot`
- Re-configured code to handle user-supplied `xs` and `ys` 
information; axis titles respect user-supplied `name` 
settings for scales
- Added functions to `rot_x` and `rot_y` to rotate
tick labels 
- Added `scales` argument to `dv_pred` and `dv_ipred`; by default
(`scales = "fixed"`), the x- and y-axis have the same limits; 
when `scales = "free"` limits are chosen by ggplot2 and will likely 
be different for x- and y-axis
- Fixed bug in `split_plot` when splitting by a factor that 
doesn't have all level present
- Fixed bug in `wres_q` related to indexing a tibble
- Added functions to export look and feel: `pm_theme`, 
`pm_abline`, `pm_smooth`, `pm_hline`, `pm_histogram`; these functions
help make the plots look consistent with other plots if you are 
making reference lines, smooths, etc; `pm_theme` is still just a wrapper
around `theme_bw()`
- `eta_pairs` dispatches to `eta_hist` when there is only one eta 
in the mix #14
- A density line is by default plotted over histograms 
for `cwres`, `wres`, and `npde`
- Changed all plots so that axis titles are generated with `labs` 
rather than via `scale_(x|y)_continuous` #23
- ggplot2 version 3.0.0 or greater is now required
- `col_label` specification can now be automatically parsed to 
generate expressions to be rendered with plotmath;  if two `$` symbols
are found in the title, pmplots will try to load the latex2exp
namespace and pass the axis title through `latex2exp::TeX`.  Otherwise, 
if the axis title begins with `!!`, then pmplots will parse the 
column title to generate the expression
- Added cowplot as a suggested package to work with `pm_grid`
- Fixed a bug introduced with ggplot2 version 3 when used with 
`eta_pairs` #17
- `split_plot` now accepts a `grouped` data frame as input; the list of plots
will be generated by splitting the data frame on grouping column #32
- Added faceted plots: `wrap_cont_cont`, `wrap_cont_time`, `wrap_res_time`, 
`wrap_eta_cont`, `wrap_hist`, and `wrap_dv_preds`

# pmplots 0.1.0.9001

- Fixed `logbr3` so that the values are sorted; this 
fixes an issue where the grid lines were not properly
displayed with these breaks
- Fixed issue in `dv_time` and `dv_pred` when 
modifying the x-scale and y-scale through 
xs and ys arguments
- Changed `dv_time` and `dv_pred` so that 
breaks are at half-log units when `log = TRUE` or 
`loglog = TRUE`


# pmplots 0.1.0

- Initial validated version 
