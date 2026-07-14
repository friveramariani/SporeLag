#' Assign ISO 8601 week and year
#'
#' @description
#' `assign_iso_week()` appends the ISO 8601 week number and ISO year for each
#' date. It is a row-wise mapping: no grouping, no temporal indexing, no
#' requirement that the grid be complete.
#'
#' @details
#' **The ISO year is not the calendar year, and this matters.** Late-December
#' and early-January dates routinely belong to an ISO week owned by the
#' adjacent year:
#'
#' \tabular{lll}{
#'   **Date**     \tab **Weekday** \tab **ISO** \cr
#'   2019-12-30   \tab Monday      \tab 2020-W01 \cr
#'   2020-12-31   \tab Thursday    \tab 2020-W53 \cr
#'   2021-01-01   \tab Friday      \tab 2020-W53 \cr
#'   2023-01-01   \tab Sunday      \tab 2022-W52 \cr
#' }
#'
#' Consequently, **week number alone is not a valid grouping key.** Grouping a
#' multi-year series by `iso_week` pools week 1 of every year together, and
#' pools the days either side of a New Year boundary into different weeks than
#' the calendar suggests. Always group by `c(iso_year, iso_week)`. Downstream
#' aggregation functions in SporeLag require both columns for this reason.
#'
#' ISO weeks are computed in base R via the nearest-Thursday rule rather than
#' `strftime()`'s `%V`/`%G` format codes, whose behaviour has historically
#' varied by platform. Output is therefore identical on every operating system.
#'
#' @param data A `data.frame`.
#' @param date A single string giving the name of a `Date` column in `data`.
#' @param week_col Name of the ISO week column to append. Default `"iso_week"`.
#' @param year_col Name of the ISO year column to append. Default `"iso_year"`.
#'
#' @return `data` with two integer columns appended: `week_col` (1–53) and
#'   `year_col`. Row order and all input columns are unchanged.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_bad_input`}{`data`, `date`, `week_col`, or
#'     `year_col` do not match the documented types; `date` is not a `Date`
#'     column; or an output column name already exists in `data`.}
#'   \item{`sporelag_error_missing_date`}{`date` contains `NA`.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   date = as.Date(c("2019-12-30", "2020-12-31", "2021-01-01")),
#'   count = c(1, 2, 3)
#' )
#' assign_iso_week(df, date = "date")
#'
#' @seealso [assign_season()]
#' @export
assign_iso_week <- function(data,
                            date,
                            week_col = "iso_week",
                            year_col = "iso_year") {
  .check_df(data)
  .check_col_name(date, "date")
  .check_col_name(week_col, "week_col")
  .check_col_name(year_col, "year_col")
  .check_cols_exist(data, date)
  .check_date_col(data, date)
  .check_date_complete(data, date)
  .check_output_cols(data, c(week_col, year_col))

  if (nrow(data) == 0L) {
    data[[week_col]] <- integer(0)
    data[[year_col]] <- integer(0)
    return(data)
  }

  x <- data[[date]]
  data[[week_col]] <- .iso_week(x)
  data[[year_col]] <- .iso_year(x)
  data
}
