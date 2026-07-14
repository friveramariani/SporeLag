#' Assign a season label
#'
#' @description
#' `assign_season()` appends a season label to each date. The definition is an
#' explicit argument with no silent default behaviour, because "season" means
#' materially different things in different literatures.
#'
#' @details
#' **`definition = "meteorological"`** (the default) uses whole calendar months
#' — Northern Hemisphere: Winter = Dec–Feb, Spring = Mar–May, Summer = Jun–Aug,
#' Autumn = Sep–Nov. Boundaries are exact and leap-year stable.
#'
#' **`definition = "astronomical"`** uses **fixed conventional dates** (Mar 20,
#' Jun 21, Sep 22, Dec 21). True equinoxes and solstices drift by up to a day
#' or two across years and depend on time zone; computing them exactly requires
#' an ephemeris, which is out of scope for a zero-dependency package. If your
#' analysis is sensitive to a one-day boundary shift, do not use this option —
#' supply exact dates via `definition = "custom"`.
#'
#' **`definition = "custom"`** takes `breaks`: a named character vector of
#' `"MM-DD"` start dates, where the names are the season labels. Each season
#' runs from its start date until the day before the next, wrapping the year
#' boundary. This is the option to use for **pollen seasons**, which are
#' taxon- and region-specific and bear no necessary relation to the calendar.
#'
#' \strong{Epidemiologic caution.} In aeroallergen research, "season" usually
#' means \emph{pollen season} — a taxon-specific exposure window, often defined
#' by a cumulative-count threshold, that varies by region and by year. It is
#' not the astronomical or meteorological calendar. Using a calendar season as
#' a proxy for a pollen season will misclassify exposure windows. The default
#' here is a calendar convenience, not an exposure definition; state which you
#' used in any analysis.
#'
#' The returned column is an **ordered factor** whose levels follow the
#' chronological cycle, so its behaviour in a model matrix is predictable. The
#' reference level is the first level; set it explicitly with [stats::relevel()]
#' if your model requires a different baseline.
#'
#' @param data A `data.frame`.
#' @param date A single string giving the name of a `Date` column in `data`.
#' @param definition One of `"meteorological"` (default), `"astronomical"`, or
#'   `"custom"`.
#' @param hemisphere One of `"northern"` (default) or `"southern"`. Ignored
#'   when `definition = "custom"`, since custom breaks are absolute.
#' @param breaks Required when `definition = "custom"`. A named character
#'   vector of `"MM-DD"` season start dates, e.g.
#'   `c(Ragweed = "08-15", Grass = "05-01", Tree = "02-15", Dormant = "11-01")`.
#'   Order does not matter; breaks are sorted internally.
#' @param season_col Name of the column to append. Default `"season"`.
#'
#' @return `data` with an ordered factor column appended. Row order and all
#'   input columns are unchanged.
#'
#' @section Errors:
#' \describe{
#'   \item{`sporelag_error_bad_input`}{Bad types; `date` is not a `Date`
#'     column; `season_col` already exists; `breaks` missing, unnamed,
#'     duplicated, or not in `"MM-DD"` form.}
#'   \item{`sporelag_error_missing_date`}{`date` contains `NA`.}
#' }
#'
#' @examples
#' df <- data.frame(date = as.Date(c("2024-01-15", "2024-04-15", "2024-07-15")))
#'
#' assign_season(df, date = "date")
#' assign_season(df, date = "date", hemisphere = "southern")
#'
#' # A taxon-specific pollen season
#' assign_season(
#'   df,
#'   date = "date",
#'   definition = "custom",
#'   breaks = c(Dormant = "11-01", Tree = "02-15", Grass = "05-01",
#'              Ragweed = "08-15")
#' )
#'
#' @seealso [assign_iso_week()]
#' @export
assign_season <- function(data,
                          date,
                          definition = c("meteorological", "astronomical",
                                         "custom"),
                          hemisphere = c("northern", "southern"),
                          breaks = NULL,
                          season_col = "season") {
  .check_df(data)
  .check_col_name(date, "date")
  .check_col_name(season_col, "season_col")
  .check_cols_exist(data, date)
  .check_date_col(data, date)
  .check_date_complete(data, date)
  .check_output_cols(data, season_col)

  definition <- rlang::arg_match(definition)
  hemisphere <- rlang::arg_match(hemisphere)

  if (definition == "custom") {
    breaks <- .check_breaks(breaks)
  } else if (!is.null(breaks)) {
    cli::cli_abort(
      c("{.arg breaks} is only used when {.code definition = \"custom\"}.",
        i = "You passed {.code definition = \"{definition}\"}."),
      class = "sporelag_error_bad_input"
    )
  }

  if (nrow(data) == 0L) {
    lv <- switch(definition,
                 custom = names(sort(.md_int(breaks))),
                 .season_levels(hemisphere)
    )
    data[[season_col]] <- factor(character(0), levels = lv, ordered = TRUE)
    return(data)
  }

  md <- .month_day(data[[date]])

  data[[season_col]] <- switch(definition,
                               meteorological = .season_calendar(md, .met_starts(), hemisphere),
                               astronomical   = .season_calendar(md, .ast_starts(), hemisphere),
                               custom         = .season_custom(md, breaks)
  )

  data
}

# --- internals ---------------------------------------------------------------

.season_levels <- function(hemisphere) {
  if (hemisphere == "northern") {
    c("Winter", "Spring", "Summer", "Autumn")
  } else {
    c("Summer", "Autumn", "Winter", "Spring")
  }
}

# Northern-hemisphere start dates, in chronological order within the year.
.met_starts <- function() {
  c(Winter = 1201L, Spring = 301L, Summer = 601L, Autumn = 901L)
}

# Fixed conventional approximations. See @details.
.ast_starts <- function() {
  c(Winter = 1221L, Spring = 320L, Summer = 621L, Autumn = 922L)
}

# Map a season name to its southern-hemisphere counterpart (6-month offset).
.flip_hemisphere <- function(nm) {
  c(Winter = "Summer", Spring = "Autumn",
    Summer = "Winter", Autumn = "Spring")[nm]
}

.season_calendar <- function(md, starts, hemisphere) {
  s   <- sort(starts)
  idx <- findInterval(md, s)
  idx[idx == 0L] <- length(s)          # before the first break -> wraps to last
  lab <- names(s)[idx]

  if (hemisphere == "southern") {
    lab <- unname(.flip_hemisphere(lab))
  }

  factor(lab, levels = .season_levels(hemisphere), ordered = TRUE)
}

# Convert a Date vector to MMDD integers (e.g., Jan 15 -> 115, Dec 21 -> 1221).
.month_day <- function(d) {
  as.integer(format(d, "%m")) * 100L + as.integer(format(d, "%d"))
}

.md_int <- function(breaks) {
  # as.integer() drops names, so preserve them explicitly.
  result <- as.integer(sub("-", "", breaks, fixed = TRUE))
  names(result) <- names(breaks)
  result
}

.season_custom <- function(md, breaks) {
  s   <- sort(.md_int(breaks))
  idx <- findInterval(md, s)
  idx[idx == 0L] <- length(s)          # wraps the year boundary
  factor(names(s)[idx], levels = names(s), ordered = TRUE)
}

.check_breaks <- function(breaks, call = rlang::caller_env()) {
  if (is.null(breaks)) {
    cli::cli_abort(
      "{.arg breaks} is required when {.code definition = \"custom\"}.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (!is.character(breaks) || length(breaks) < 2L) {
    cli::cli_abort(
      "{.arg breaks} must be a character vector of at least two season starts.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  nm <- names(breaks)
  if (is.null(nm) || anyNA(nm) || !all(nzchar(nm))) {
    cli::cli_abort(
      c("{.arg breaks} must be {.emph named}; names are the season labels.",
        i = 'e.g. {.code c(Tree = "02-15", Grass = "05-01")}'),
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (anyDuplicated(nm)) {
    cli::cli_abort(
      "{.arg breaks} contains duplicate season label{?s}: {.val {unique(nm[duplicated(nm)])}}.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (!all(grepl("^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", breaks))) {
    bad <- breaks[!grepl("^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", breaks)]
    cli::cli_abort(
      c("{.arg breaks} must be {.str MM-DD} strings.",
        x = "Invalid: {.val {bad}}."),
      class = "sporelag_error_bad_input", call = call
    )
  }
  if (anyDuplicated(.md_int(breaks))) {
    cli::cli_abort(
      "{.arg breaks} contains duplicate start dates.",
      class = "sporelag_error_bad_input", call = call
    )
  }
  breaks
}
