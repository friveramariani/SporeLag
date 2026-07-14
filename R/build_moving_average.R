#' Build moving-average exposure windows
#'
#' @description
#' `build_moving_average()` appends one column per requested window, containing
#' the mean exposure over that window, computed within each group.
#'
#' @details
#' Like [apply_lag()], this function indexes by position and therefore
#' **requires a complete daily grid**; it raises `sporelag_error_gaps`
#' otherwise. Call [complete_daily_grid()] first.
#'
#' **Documented defaults — each of these changes what the variable means:**
#'
#' \describe{
#'   \item{`align = "right"` (default)}{The window is **trailing** and
#'     **inclusive of the current day**: `window = 3` averages today and the
#'     two preceding days. This is the alignment appropriate for
#'     exposure→outcome inference.}
#'   \item{`align = "center"`}{The window is symmetric around the current day
#'     and therefore **incorporates future exposure**. If the outcome is
#'     measured on the current day, a centred window conditions on information
#'     that did not exist at the time — it can induce apparent associations
#'     with no causal interpretation. It is provided for smoothing and
#'     descriptive display, not for exposure construction. Requires an odd
#'     `window`.}
#'   \item{Edge windows are `NA`}{A window that extends past the start (or,
#'     when centred, either end) of a group's series is incomplete by
#'     construction and returns `NA`. The first `window - 1` days of each
#'     trailing series are `NA`. They are not back-filled with a shorter
#'     average, because a "7-day mean" computed from 2 days is not a 7-day
#'     mean.}
#'   \item{`min_obs = window` (default)}{The minimum number of **non-missing**
#'     observations required *within* an otherwise complete window. At the
#'     default, any `NA` in the window yields `NA`. Lowering it (e.g.
#'     `min_obs = 5` with `window = 7`) tolerates missing days and averages
#'     over those present — which shrinks variance and can bias a
#'     lag–response estimate toward the null. Lower it deliberately, and
#'     report that you did.}
#' }
#'
#' @param data A `data.frame` on a complete daily grid.
#' @param value A single string naming a numeric exposure column.
#' @param window A vector of positive whole numbers, e.g. `c(3, 7)`.
#' @param date A single string naming a `Date` column.
#' @param by A character vector of grouping column names. Default `character()`.
#' @param align `"right"` (default, trailing) or `"center"`. See Details.
#' @param min_obs Minimum non-missing observations required within a window.
#'   Defaults to `window` (no missingness tolerated). Recycled if `window` has
#'   length > 1.
#'
#' @return `data` with one `double` column appended per window, named
#'   `{value}_ma{k}`. Row order and all input columns are unchanged.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_gaps`}{The daily grid is incomplete within at least
#'     one group.}
#'   \item{`sporelag_error_bad_input`}{Bad types; `value` is not numeric;
#'     `window` is not a positive whole number; `min_obs` exceeds `window`;
#'     `align = "center"` with an even `window`; an output column already
#'     exists.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   site  = rep(c("A", "B"), each = 5),
#'   date  = rep(seq(as.Date("2024-05-01"), by = "day", length.out = 5), 2),
#'   count = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
#' )
#'
#' build_moving_average(df, value = "count", window = 3, date = "date",
#'                      by = "site")
#'
#' @seealso [complete_daily_grid()], [apply_lag()]
#' @export
build_moving_average <- function(data, value, window, date,
                                 by = character(),
                                 align = c("right", "center"),
                                 min_obs = window) {
  .check_temporal_input(data, value, date, by)
  align  <- rlang::arg_match(align)
  window <- .check_window(window, align)
  min_obs <- .check_min_obs(min_obs, window)

  new_cols <- paste0(value, "_ma", window)
  .check_output_cols(data, new_cols)

  if (nrow(data) == 0L) {
    for (nm in new_cols) data[[nm]] <- numeric(0)
    return(data)
  }

  gid <- .group_ids(data, by)
  idx <- split(seq_len(nrow(data)), gid)

  for (j in seq_along(window)) {
    out <- rep(NA_real_, nrow(data))

    for (rows in idx) {
      o <- rows[order(data[[date]][rows])]
      out[o] <- .roll_mean(as.numeric(data[[value]][o]),
                           k = window[j], align = align,
                           min_obs = min_obs[j])
    }
    data[[new_cols[j]]] <- out
  }

  data
}

# --- internals ---------------------------------------------------------------

# Rolling mean over a fixed window. Base R via stats::filter.
#
# sides = 1 : window covers positions (i - k + 1) ... i  -> trailing, inclusive
# sides = 2 : window is symmetric about i                -> centred (odd k only)
#
# stats::filter returns NA where the window overruns the series edge, which is
# exactly the desired behaviour: an edge window is incomplete by construction.
#
# NAs are handled by summing the non-missing values and counting them, rather
# than by mean(na.rm = TRUE), so that `min_obs` can gate the result.
.roll_mean <- function(x, k, align, min_obs) {
  n <- length(x)
  if (k > n) return(rep(NA_real_, n))

  sides <- if (align == "right") 1L else 2L
  w     <- rep(1, k)

  present <- !is.na(x)
  xz      <- x
  xz[!present] <- 0

  sums   <- as.numeric(stats::filter(xz, w, sides = sides))
  counts <- as.numeric(stats::filter(as.numeric(present), w, sides = sides))

  out <- sums / counts
  out[is.na(counts) | counts < min_obs] <- NA_real_
  out
}

.check_window <- function(window, align, call = rlang::caller_env()) {
  ok <- is.numeric(window) && length(window) >= 1L && !anyNA(window) &&
    all(is.finite(window)) && all(window == trunc(window)) && all(window > 0)
  if (!ok) {
    cli::cli_abort(
      "{.arg window} must be a vector of positive whole numbers.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (anyDuplicated(window)) {
    cli::cli_abort(
      "{.arg window} contains duplicate values: {.val {unique(window[duplicated(window)])}}.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (align == "center" && any(window %% 2L == 0L)) {
    cli::cli_abort(
      c("{.arg window} must be odd when {.code align = \"center\"}.",
        x = "Even window{?s} {.val {window[window %% 2L == 0L]}} cannot be
             centred symmetrically.",
        i = "Use an odd window, or {.code align = \"right\"}."),
      class = "sporelag_error_bad_input", call = call
    )
  }
  as.integer(window)
}

.check_min_obs <- function(min_obs, window, call = rlang::caller_env()) {
  ok <- is.numeric(min_obs) && !anyNA(min_obs) && all(is.finite(min_obs)) &&
    all(min_obs == trunc(min_obs)) && all(min_obs >= 1)
  if (!ok) {
    cli::cli_abort(
      "{.arg min_obs} must be positive whole number{?s}.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (length(min_obs) != length(window)) {
    if (length(min_obs) != 1L) {
      cli::cli_abort(
        "{.arg min_obs} must be length 1 or the same length as {.arg window}.",
        class = "sporelag_error_bad_input", call = call
      )
    }
    min_obs <- rep(min_obs, length(window))
  }
  if (any(min_obs > window)) {
    bad <- which(min_obs > window)
    cli::cli_abort(
      c("{.arg min_obs} cannot exceed {.arg window}.",
        x = "min_obs = {.val {min_obs[bad]}} with window = {.val {window[bad]}}
             can never be satisfied; every value would be {.code NA}."),
      class = "sporelag_error_bad_input", call = call
    )
  }
  as.integer(min_obs)
}
