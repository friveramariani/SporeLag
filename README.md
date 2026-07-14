# SporeLag

> Lagged and Moving-Average Exposure Features for Aeroallergen Epidemiology

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R CMD check](https://github.com/friveramariani/SporeLag/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/friveramariani/SporeLag/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

SporeLag provides deterministic, group-safe utilities for transforming daily
environmental exposure series — pollen counts, spore counts, ozone, PM, and
similar time-varying exposures — into analysis-ready features for
environmental epidemiology and public health research.

Every function follows a single contract: a `data.frame` goes in, the same
`data.frame` comes back with new columns appended. No input columns are
modified, dropped, or reordered. Operations are computed strictly within
group, so values never bleed across sites, seasons, or study participants.

## Installation

SporeLag is in active development and not yet on CRAN. Install from GitHub:

```r
# install.packages("pak")
pak::pak("friveramariani/SporeLag")
```

Or with remotes:

```r
# install.packages("remotes")
remotes::install_github("friveramariani/SporeLag")
```

Requires R ≥ 4.1.0. The only runtime dependencies are
[cli](https://cli.r-lib.org) and [rlang](https://rlang.r-lib.org).

## Usage

### Build a complete daily grid

Before computing lags or moving averages, every time series must be a
complete daily grid — one row per calendar day, no gaps, no duplicate
dates within a group. `complete_daily_grid()` enforces this explicitly
rather than silently filling gaps at the moment you least expect it.

```r
library(SporeLag)

daily <- data.frame(
  site  = c("Miami", "Miami", "Miami", "Orlando", "Orlando"),
  date  = as.Date(c(
    "2023-03-01", "2023-03-02", "2023-03-04",   # gap on Mar 3
    "2023-03-01", "2023-03-03"                   # gap on Mar 2
  )),
  pollen = c(12, 35, 80, 5, 22)
)

complete_daily_grid(daily, date = "date", by = "site")
#>      site       date pollen
#> 1   Miami 2023-03-01     12
#> 2   Miami 2023-03-02     35
#> 3   Miami 2023-03-03     NA   # <-- inserted, pollen unknown
#> 4   Miami 2023-03-04     80
#> 5 Orlando 2023-03-01      5
#> 6 Orlando 2023-03-02     NA   # <-- inserted, pollen unknown
#> 7 Orlando 2023-03-03     22
```

Gaps are filled with `NA` — SporeLag never guesses at missing values.
Functions that compute lags or moving averages require a complete grid as
input and raise an informative error if one is not provided.

## Functions

| Function | Status | Purpose |
|---|---|---|
| `complete_daily_grid()` | ✅ Available | Fill gaps to produce a complete daily series |
| `assign_iso_week()` | 🔨 Phase 2 | Append ISO 8601 week and year columns |
| `assign_season()` | 🔨 Phase 2 | Append a configurable season label column |
| `apply_lag()` | 🔨 Phase 3 | Append lagged copies of an exposure column |
| `build_moving_average()` | 🔨 Phase 3 | Append windowed moving-average exposure columns |
| `impute_weekly_mean()` | 🔨 Phase 4 | Impute `NA` values using within-group weekly means |

## Design

**Minimal dependencies.** Runtime imports are `cli` and `rlang` only — no
tidyverse, no zoo, no slider. Rolling statistics and group operations are
implemented in base R so the package can be used in any environment without
pulling in a transitive dependency chain.

**Errors over silence.** When a function detects a problem — a gapped series
passed to a lag function, duplicate dates within a group, a non-`Date` column
— it raises a classed condition (e.g. `"sporelag_error_gaps"`) rather than
silently producing a plausible-looking but wrong result. Tests catch errors
by class, not by message string.

**Defaults are analytic decisions.** Window alignment, inclusivity, minimum
observations, season boundaries, and missing-day treatment all change what
an exposure variable *means* in a regression model. Every default is
documented explicitly in each function's `@details`. They are not cosmetic.

See [`inst/DESIGN-DECISIONS.md`](inst/DESIGN-DECISIONS.md) for the
architectural decisions behind the dependency policy, ISO week
implementation, and gap-handling strategy.

## License

MIT © Félix E. Rivera-Mariani
