#' Impute missing daily values with the ISO-week mean
#'
#' @description
#' `impute_weekly_mean()` fills missing daily exposure values with the mean of
#' the observed values in the same ISO week, within group. It appends the
#' completed series and a flag marking which values were imputed. **The input
#' column is never modified.**
#'
#' @details
#' Requires the ISO week and year columns produced by [assign_iso_week()].
#' Typically it also follows [complete_daily_grid()], since a day that is
#' absent from the data entirely cannot be imputed — it must first exist as a
#' row with an `NA` value.
#'
#' The canonical pipeline is:
#'
#' ```
#' data |>
#'   complete_daily_grid(date = "date", by = "site") |>
#'   assign_iso_week(date = "date") |>
#'   impute_weekly_mean(value = "count", by = "site")
#' ```
#'
#' **Grouping is by `c(by, year_col, week_col)` — never by week alone.** ISO
#' week 1 recurs every year, and the ISO year of a late-December date is often
#' the *following* calendar year. Grouping on week number alone would pool
#' observations from different years into a single mean. See [assign_iso_week()].
#'
#' **Documented defaults:**
#'
#' \describe{
#'   \item{`min_obs = 1`}{The minimum number of observed days required in a
#'     week before its mean is used. At the default, a single observed day is
#'     enough to fill the other six. Raise it (e.g. `min_obs = 4`) if you are
#'     unwilling to extrapolate a week from one observation; weeks below the
#'     threshold are left `NA` and flagged `FALSE`.}
#'   \item{Weeks with no observations stay `NA`}{Nothing is carried across a
#'     week boundary. There is no interpolation between weeks, no
#'     last-observation-carried-forward, and no global mean fallback.}
#'   \item{Output is `double`}{Even when `value` is an integer count. A mean is
#'     not a count, and rounding an imputed 12.5 to 12 or 13 would fabricate
#'     precision. If your model needs integers, round explicitly and say so.}
#' }
#'
#' \strong{Epidemiologic caution.} Weekly-mean imputation replaces day-to-day
#' variation with a constant, which \emph{shrinks the variance of the exposure}.
#' In a lag–response model, an exposure with attenuated variance typically
#' produces effect estimates biased \emph{toward the null}, and standard errors
#' that do not reflect the true uncertainty (the imputed values are treated as
#' though observed). The `{value}_imputed_flag` column exists so that you can
#' quantify this: report the proportion imputed, and re-run the analysis on
#' complete cases as a sensitivity check. This function makes imputation
#' convenient; it does not make it innocuous.
#'
#' @param data A `data.frame` containing ISO week and year columns.
#' @param value A single string naming a numeric exposure column.
#' @param by A character vector of grouping column names. Default `character()`.
#' @param week_col Name of the ISO week column. Default `"iso_week"`.
#' @param year_col Name of the ISO year column. Default `"iso_year"`.
#' @param min_obs Minimum observed days required in a week for its mean to be
#'   used. Default `1`.
#'
#' @return `data` with two columns appended:
#'   \describe{
#'     \item{`{value}_imputed`}{`double`. The completed series.}
#'     \item{`{value}_imputed_flag`}{`logical`. `TRUE` where a value was filled.}
#'   }
#'   Row order and all input columns, including `value`, are unchanged.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_missing_col`}{`week_col` or `year_col` is absent.
#'     Call [assign_iso_week()] first.}
#'   \item{`sporelag_error_bad_input`}{Bad types; `value` is not numeric;
#'     `min_obs` is not a positive whole number; an output column already
#'     exists.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   site  = rep("A", 7),
#'   date  = seq(as.Date("2024-05-06"), by = "day", length.out = 7),  # Mon-Sun
#'   count = c(10, NA, 30, NA, 50, 60, NA)
#' )
#'
#' df |>
#'   assign_iso_week(date = "date") |>
#'   impute_weekly_mean(value = "count", by = "site")
#'
#' @seealso [assign_iso_week()], [complete_daily_grid()]
#' @export
impute_weekly_mean <- function(data,
                               value,
                               by = character(),
                               week_col = "iso_week",
                               year_col = "iso_year",
                               min_obs = 1L) {
  .check_df(data)
  .check_col_name(value, "value")
  .check_col_name(week_col, "week_col")
  .check_col_name(year_col, "year_col")
  by <- .check_by(by)
  .check_iso_cols(data, week_col, year_col)
  .check_cols_exist(data, c(value, by))
  .check_group_complete(data, by)
  .check_numeric_col(data, value)
  min_obs <- .check_positive_int(min_obs, "min_obs")
  if (length(min_obs) != 1L) {
    cli::cli_abort(
      "{.arg min_obs} must be a single number.",
      class = c("sporelag_error_type", "sporelag_error_bad_input")
    )
  }

  val_col  <- paste0(value, "_imputed")
  flag_col <- paste0(value, "_imputed_flag")
  .check_output_cols(data, c(val_col, flag_col))

  if (nrow(data) == 0L) {
    data[[val_col]]  <- numeric(0)
    data[[flag_col]] <- logical(0)
    return(data)
  }

  x <- as.numeric(data[[value]])

  # Group by c(by, year, week). Week alone is NOT a valid key: ISO week 1
  # recurs annually, and a late-December date can belong to the NEXT ISO year.
  gid <- .group_ids(data, c(by, year_col, week_col))

  filled <- x
  flag   <- rep(FALSE, nrow(data))

  for (rows in split(seq_len(nrow(data)), gid)) {
    xi      <- x[rows]
    present <- !is.na(xi)
    n_obs   <- sum(present)

    if (n_obs < min_obs || n_obs == 0L) next   # leave NA, leave flag FALSE

    mu <- mean(xi[present])
    to_fill <- rows[!present]

    filled[to_fill] <- mu
    flag[to_fill]   <- TRUE
  }

  data[[val_col]]  <- filled
  data[[flag_col]] <- flag
  data
}

# --- internals ---------------------------------------------------------------

.check_iso_cols <- function(data, week_col, year_col,
                            call = rlang::caller_env()) {
  missing <- setdiff(c(week_col, year_col), names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c("ISO column{?s} {.val {missing}} not found in {.arg data}.",
        x = "Weekly means must be grouped by ISO year AND week.",
        ">" = "Call {.fn assign_iso_week} first."),
      class = c("sporelag_error_missing_col", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}
