#' Synthetic daily pollen counts from two monitoring sites
#'
#' A **synthetic** dataset of daily airborne pollen concentrations for a single
#' spring tree-pollen season at two hypothetical monitoring stations. It exists
#' to demonstrate and test SporeLag and is used throughout
#' `vignette("getting-started")`.
#'
#' @format A data frame with three columns:
#' \describe{
#'   \item{site}{`character`. Monitoring station: `"North"` or `"South"`.}
#'   \item{date}{`Date`. Calendar day, spanning 2024-02-15 to 2024-06-30.}
#'   \item{count}{`integer`. Pollen concentration in grains per cubic metre.
#'     `NA` where sampling failed on a day that was otherwise monitored.}
#' }
#'
#' @details
#' The data deliberately contain **both** of the defects SporeLag is designed
#' to handle, because they are different problems and are frequently confused:
#'
#' \describe{
#'   \item{Gaps}{Calendar days with **no row at all**, representing periods
#'     when the sampler was offline: 2024-03-18 to 2024-03-22 at North, and
#'     2024-04-01 to 2024-04-03 at South. A gap is invisible to `is.na()` — the
#'     row simply is not there. Gaps must be closed with
#'     [complete_daily_grid()] before any lag or moving average is computed, or
#'     the shift will be by row position rather than by time.}
#'   \item{Missing values}{Days that **are** present but whose `count` is `NA`,
#'     representing failed samples on otherwise-monitored days (roughly 6% of
#'     days at each site). These are candidates for [impute_weekly_mean()].}
#' }
#'
#' The South gap falls at that site's seasonal peak — the point at which losing
#' data does the most damage to an exposure estimate, and where a naive
#' positional lag would silently produce the largest misalignment.
#'
#' Counts were drawn from a negative binomial distribution around a Gaussian
#' seasonal curve, so they are overdispersed in the way real aeroallergen counts
#' are. The generating script is `data-raw/make_pollen_demo.R` and is fully
#' deterministic.
#'
#' @source Simulated. **These are not real surveillance data** and must not be
#'   used for any substantive inference about pollen exposure. See
#'   `data-raw/make_pollen_demo.R` for the generating code.
#'
#' @examples
#' str(pollen_demo)
#'
#' # Gaps are absent rows, not NA values -- is.na() cannot see them:
#' table(pollen_demo$site)          # neither site has a full 137 days
#' sum(is.na(pollen_demo$count))    # the NAs are a separate problem
#'
#' # The canonical pipeline:
#' pollen_demo |>
#'   complete_daily_grid(date = "date", by = "site") |>
#'   assign_iso_week(date = "date") |>
#'   impute_weekly_mean(value = "count", by = "site") |>
#'   apply_lag(value = "count_imputed", lags = 0:2, date = "date", by = "site") |>
#'   head()
#'
#' @seealso [complete_daily_grid()], [impute_weekly_mean()]
"pollen_demo"
