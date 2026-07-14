#' Build a complete daily grid
#'
#' @description
#' `complete_daily_grid()` fills gaps in a daily time series so that every
#' calendar day between the minimum and maximum date (within each group) is
#' represented by exactly one row. Newly inserted rows carry `NA` for every
#' column except `date` and the columns named in `by`.
#'
#' @details
#' Functions in SporeLag that index by time (lags, moving averages) assume
#' the day-to-day step is exactly one day with no repeats and no gaps within
#' a group. Rather than silently shifting values by row position when that
#' assumption fails, those functions raise a classed error. This function is
#' the documented mechanism for producing that guarantee before calling them.
#'
#' `complete_daily_grid()` never guesses at missing values: inserted rows
#' get `NA`, and it is the caller's responsibility to decide whether and how
#' to impute them.
#'
#' @param data A `data.frame`.
#' @param date A single string giving the name of a `Date` column in `data`.
#' @param by A character vector of column names in `data` to group by.
#'   Defaults to `character()`, meaning no grouping (the whole `data.frame`
#'   is treated as a single daily series). Gaps are filled, and dates are
#'   checked for duplicates, independently within each group.
#'
#' @return A `data.frame` with the same columns as `data`, in the same
#'   order, with one row per calendar day between `min(date)` and
#'   `max(date)` within each group. Rows are sorted by `by`, then `date`.
#'   No columns are added or removed; this function only inserts rows.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_bad_input`}{`data`, `date`, or `by` do not match
#'     the documented types, or name columns not present in `data`.}
#'   \item{`sporelag_error_duplicate_dates`}{The same date appears more than
#'     once within a single group. A grid cannot be built unambiguously, so
#'     the function errors instead of silently choosing a row.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   site = c("A", "A", "A", "B", "B"),
#'   date = as.Date(c(
#'     "2024-01-01", "2024-01-02", "2024-01-04",
#'     "2024-01-01", "2024-01-03"
#'   )),
#'   count = c(1, 2, 4, 10, 30)
#' )
#' complete_daily_grid(df, date = "date", by = "site")
#'
#' @export
complete_daily_grid <- function(data, date, by = character()) {
  validate_complete_daily_grid_inputs(data, date, by)

  if (nrow(data) == 0) {
    return(data)
  }

  if (length(by) == 0) {
    groups <- list(data)
  } else {
    split_factor <- interaction(data[by], drop = TRUE)
    groups <- split(data, split_factor)
  }

  groups <- lapply(groups, check_no_duplicate_dates, date = date)

  filled <- lapply(groups, fill_group_grid, date = date, by = by)
  result <- do.call(rbind, filled)

  result <- result[, names(data), drop = FALSE]
  ord <- do.call(order, unname(as.list(result[c(by, date)])))
  result <- result[ord, , drop = FALSE]
  rownames(result) <- NULL
  result
}

validate_complete_daily_grid_inputs <- function(data, date, by) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data.frame, not {.cls {class(data)}}.",
      class = "sporelag_error_bad_input"
    )
  }
  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort(
      "{.arg date} must be a single column name.",
      class = "sporelag_error_bad_input"
    )
  }
  if (!is.character(by)) {
    cli::cli_abort(
      "{.arg by} must be a character vector of column names.",
      class = "sporelag_error_bad_input"
    )
  }
  missing_cols <- setdiff(c(date, by), names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column(s) {.val {missing_cols}} not found in {.arg data}.",
      class = "sporelag_error_bad_input"
    )
  }
  if (!inherits(data[[date]], "Date")) {
    cli::cli_abort(
      "Column {.val {date}} must be a {.cls Date}, not {.cls {class(data[[date]])}}.",
      class = "sporelag_error_bad_input"
    )
  }
}

check_no_duplicate_dates <- function(group, date) {
  dates <- group[[date]]
  if (anyDuplicated(dates)) {
    dup_dates <- unique(dates[duplicated(dates)])
    cli::cli_abort(
      "Duplicate date(s) found within a group: {.val {as.character(dup_dates)}}.",
      class = "sporelag_error_duplicate_dates"
    )
  }
  group
}

fill_group_grid <- function(group, date, by) {
  full_dates <- seq(min(group[[date]]), max(group[[date]]), by = "day")
  grid <- data.frame(full_dates)
  names(grid) <- date
  for (col in by) {
    grid[[col]] <- group[[col]][1]
  }
  merge(grid, group, by = c(date, by), all.x = TRUE, sort = FALSE)
}
