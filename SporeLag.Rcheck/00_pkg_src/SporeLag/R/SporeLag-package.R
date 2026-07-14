#' @keywords internal
#'
#' @section Package conventions:
#'
#' All exported functions in SporeLag share a single contract. These are
#' guarantees, not conventions of style, and downstream code may rely on them.
#'
#' \describe{
#'   \item{Input and output}{A `data.frame` goes in; the same `data.frame`
#'     comes out with new columns \emph{appended}. Input columns are never
#'     dropped, reordered, or modified in place.}
#'   \item{Grouping}{The `by` argument is always a character vector of column
#'     names. All operations are computed strictly within group. Values never
#'     carry across a group boundary.}
#'   \item{Determinism}{No randomness anywhere. Repeated calls on identical
#'     input return identical output.}
#'   \item{Column naming}{New columns follow `{value}_lag{n}`, `{value}_ma{k}`,
#'     and `{value}_imputed`.}
#'   \item{Temporal safety}{Functions that index by time require a complete
#'     daily grid and raise an error otherwise, rather than silently shifting
#'     by row position. See [complete_daily_grid()].}
#'   \item{Errors}{Conditions are raised with a message class (for example
#'     `"sporelag_error_gaps"`) so that callers and tests can handle them
#'     specifically.}
#' }
#'
#' @section Defaults are analytic decisions:
#'
#' Every default in this package changes what an exposure variable
#' \emph{means}. Moving-average alignment, window inclusivity, the minimum
#' number of observations required, the season definition, and the treatment
#' of missing days are all documented explicitly in the `@details` of each
#' function. They are not cosmetic, and changing one changes the estimand.
#'
#' @importFrom rlang abort
#' @importFrom cli cli_abort
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
