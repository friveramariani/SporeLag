# SporeLag 0.1.1

Maintenance and packaging fixes; no user-facing API changes.

* Fixed the installation instructions in the README (replaced the `<user>`
  placeholder with the repository owner).
* Removed build and check artifacts (`*.Rcheck/`, source tarball, `README.html`)
  from version control and added them to `.gitignore`.
* Cleaned up the `Authors@R` field and citation metadata (`inst/CITATION`,
  `CITATION.cff`), removing a name suffix that was embedded in the family name.
* `R CMD check` now passes with no errors, warnings, or notes.

# SporeLag 0.1.0

First release.


## Exported functions

* `complete_daily_grid()` inserts rows for absent calendar days, within group.
* `assign_iso_week()` appends ISO 8601 week and year.
* `assign_season()` appends a configurable season label (meteorological,
  astronomical, or custom pollen-season breaks).
* `impute_weekly_mean()` fills missing daily values with the ISO-week mean and
  flags what it filled.
* `build_moving_average()` appends trailing (or centred) windowed means.
* `apply_lag()` appends time-indexed lags.

## Data

* `pollen_demo`: synthetic daily pollen counts from two sites over one spring
  season, containing both gaps and missing values.

## Design decisions

Two decisions with cross-cutting consequences were made before implementation
and are recorded in `inst/DESIGN-DECISIONS.md`:

* **DD-01.** ISO weeks are computed in base R via the nearest-Thursday rule,
  not via `strftime()`'s `%V`/`%G`, whose behaviour has varied by platform.
  Output is identical on every operating system.
* **DD-02.** `apply_lag()` and `build_moving_average()` raise an error on a
  gapped daily grid rather than completing it silently. Call
  `complete_daily_grid()` first.

## A note on defaults

Every default in this package changes what an exposure variable *means* —
moving-average alignment, window inclusivity, `min_obs`, the season definition,
the treatment of gaps. Changing a default is therefore a **breaking change to
the estimand**, even when it is not a breaking change to the API, and will be
documented here as such.
