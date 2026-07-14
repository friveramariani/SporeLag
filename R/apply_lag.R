#' Apply time-indexed lags to an exposure series
#'
#' @description
#' `apply_lag()` appends one column per requested lag, shifting the exposure
#' series backwards in time within each group. A lag of `n` places the value
#' observed `n` days earlier alongside the current day.
#'
#' @details
#' **This function indexes by position, and therefore requires a complete daily
#' grid.** On a gapped series, "lag 1" would mean "the previous *row*", which
#' may be one day earlier or thirty. The resulting exposure column looks
#' entirely plausible, runs cleanly through a regression, and yields a
#' silently misaligned lag–response estimate. `apply_lag()` therefore refuses
#' to operate on a gapped grid and raises `sporelag_error_gaps`. Call
#' [complete_daily_grid()] first. This is a deliberate design decision, not an
#' oversight; see `vignette("getting-started")`.
#'
#' **Documented defaults:**
#'
#' * Lags are computed strictly **within** each group defined by `by`. Values
#'   never carry across a group boundary — the first `n` days of each site's
#'   series are `NA`, not the tail of the previous site's series.
#' * `lags = 0` is permitted and yields a copy of the same-day value. This
#'   exists so a uniform model matrix (`lag0, lag1, lag2, ...`) can be built
#'   without special-casing the contemporaneous term.
#' * **Negative values are an error.** A negative lag is a *lead*: it aligns
#'   an exposure with an outcome that preceded it. That is almost never
#'   intended, and when it is (e.g. a negative-control exposure), it should be
#'   written explicitly rather than arrived at by a sign slip.
#' * Row order is preserved. Columns are appended.
#'
#' @param data A `data.frame` on a complete daily grid.
#' @param value A single string naming a numeric exposure column.
#' @param lags A vector of non-negative whole numbers, e.g. `0:3`.
#' @param date A single string naming a `Date` column.
#' @param by A character vector of grouping column names. Default
#'   `character()` (no grouping — the whole frame is one series).
#'
#' @return `data` with one column appended per lag, named `{value}_lag{n}`.
#'   Row order and all input columns are unchanged.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_gaps`}{The daily grid is incomplete within at least
#'     one group. Call [complete_daily_grid()] first.}
#'   \item{`sporelag_error_duplicate_dates`}{A date appears more than once
#'     within a group.}
#'   \item{`sporelag_error_bad_input`}{Bad types; missing columns; `value` is
#'     not numeric; `lags` contains negative or non-whole numbers; an output
#'     column name already exists.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   site  = rep(c("A", "B"), each = 4),
#'   date  = rep(seq(as.Date("2024-05-01"), by = "day", length.out = 4), 2),
#'   count = c(10, 20, 30, 40, 100, 200, 300, 400)
#' )
#'
#' apply_lag(df, value = "count", lags = 0:2, date = "date", by = "site")
#'
#' @seealso [complete_daily_grid()], [build_moving_average()]
#' @export
apply_lag <- function(data, value, lags, date, by = character()) {
  .check_temporal_input(data, value, date, by)
  lags <- .check_lags(lags)

  new_cols <- paste0(value, "_lag", lags)
  .check_output_cols(data, new_cols)

  if (nrow(data) == 0L) {
    for (nm in new_cols) data[[nm]] <- data[[value]][0]
    return(data)
  }

  gid <- .group_ids(data, by)
  idx <- split(seq_len(nrow(data)), gid)

  for (j in seq_along(lags)) {
    n   <- lags[j]
    out <- data[[value]][NA_integer_][rep(1L, nrow(data))]  # type-preserving NA

    for (rows in idx) {
      o <- rows[order(data[[date]][rows])]   # chronological within group
      out[o] <- .lag_vec(data[[value]][o], n)
    }
    data[[new_cols[j]]] <- out
  }

  data
}

# --- internals ---------------------------------------------------------------

# Shift x backwards by n positions. Type-preserving: integer stays integer.
.lag_vec <- function(x, n) {
  if (n == 0L) return(x)
  len <- length(x)
  pad <- x[NA_integer_][rep(1L, min(n, len))]
  if (n >= len) return(pad[seq_len(len)])
  c(pad, x[seq_len(len - n)])
}

.check_lags <- function(lags, call = rlang::caller_env()) {
  ok <- is.numeric(lags) && length(lags) >= 1L && !anyNA(lags) &&
    all(is.finite(lags)) && all(lags == trunc(lags))
  if (!ok) {
    cli::cli_abort(
      "{.arg lags} must be a vector of whole numbers.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (any(lags < 0)) {
    cli::cli_abort(
      c("{.arg lags} must be non-negative; got {.val {lags[lags < 0]}}.",
        x = "A negative lag is a {.emph lead}: it aligns an exposure with an
             outcome that preceded it.",
        i = "If a lead is genuinely intended, construct it explicitly."),
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (anyDuplicated(lags)) {
    cli::cli_abort(
      "{.arg lags} contains duplicate values: {.val {unique(lags[duplicated(lags)])}}.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  as.integer(lags)
}
