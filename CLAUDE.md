# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

**pmplots** is an R package providing ggplot2-based pharmacometric diagnostic and exploratory plots. It wraps common plot types used in PK/PD analyses following NONMEM conventions (DV, PRED, IPRED, CWRES, NPDE, ETA, etc.).

## Common Commands

```bash
make doc        # Regenerate roxygen2 documentation (run after editing roxygen headers)
make build      # Build package tarball (no vignettes)
make install    # Install package
make all        # doc + build + install
make test       # Run testthat tests
make check      # R CMD CHECK (ignores vignettes)
make spelling   # Check spelling
make covr       # Generate coverage report
make pkgdown    # Build docs site locally
```

To run a single test file:
```r
testthat::test_file("tests/testthat/test-dv_pred.R")
```

## Architecture

### Column/Label Syntax

Functions accept a `"COLUMN//Display Label"` string syntax parsed by `col_label()`:
- `"WT//Weight (kg)"` → uses column `WT`, shows axis label `"Weight (kg)"`
- Default axis labels are provided by `pm_axis_*()` helpers (e.g., `pm_axis_pred()`, `pm_axis_cwres()`)

### Plot Construction Pattern

Most plot functions follow this pattern:
1. Parse x/y column names and labels via `col_label()`
2. Validate columns with `require_column()`, `require_numeric()`, etc.
3. Build a base scatter via `scatt()` or box/hist geom
4. Add reference layers: `layer_a()` (identity line), `layer_h()` (hline at 0), `layer_s()` (loess smoother)
5. Return a ggplot object

### Layering System (`R/layer.R`)

Predefined layer combiners: `layer_s()` (smoother), `layer_h()` (hline), `layer_a()` (abline), and combinations like `layer_hs()`, `layer_as()`, etc. The underlying geom arguments come from global options via `gs()`, `gh()`, `ga()`.

### Global Options (`R/opts.R`)

`pm_opts` (also aliased as `pm()`) is an environment-based options system:
- Get: `pm$smooth.lwd`, `pm$get("scatter.col")`
- Set: `pm$set(smooth.lwd = 2)`
- Options control default aesthetics for smoothers, reference lines, scatter points, etc.

### Multi-Plot Displays (`R/displays.R`)

Functions like `eta_covariate()` and `npde_panel()` generate grids of related plots. They:
- Build a list of ggplot objects internally
- Arrange with `pm_grid()` (a thin wrapper around `patchwork::wrap_plots()`)
- Support a `transpose` argument to group by covariate vs. outcome
- Have `_list` variants (e.g., `eta_covariate_list()`) returning unarranged lists

### Discrete vs Continuous Detection

`.is_discrete()` returns TRUE for character/factor/logical columns → categorical (box) plots.
`.is_continuous()` returns TRUE for numeric → continuous scatter plots.
`cont_cont()` and `cont_cat()` dispatch based on this.

### CWRES/CWRESI Handling

`supplement_cwres()` creates a `CWRES` column from `CWRESI` when `CWRES` is absent, allowing functions to work with either column name transparently.

### Test Data

Example datasets are in `inst/exdata/`:
- `pmplots_data.RDS` — full observation-level data
- `pmplots_data_obs.RDS` — observations only (MDV==0)
- `pmplots_data_id.RDS` — one row per subject

Loaded in tests via `pmplots_data()`, `pmplots_data_obs()`, `pmplots_data_id()`.
