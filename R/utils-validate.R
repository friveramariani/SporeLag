# Internal validators. None are exported.
#
# Every condition carries a SPECIFIC class for precise tests, plus the PARENT
# class `sporelag_error_bad_input` so a coarse handler catches all of them:
#
#   sporelag_error_type            - wrong type for an argument
#   sporelag_error_missing_col     - named column absent from `data`
#   sporelag_error_date_type       - date column is not class Date
#   sporelag_error_missing_date    - NA in the date column
#   sporelag_error_group_na        - NA in a grouping column
#   sporelag_error_duplicate_dates - duplicate date within a group
#   sporelag_error_gaps            - date sequence is not a complete daily grid
#
# Convention: `by` is a character vector; `character()` means "no grouping".
# NULL is tolerated for backwards compatibility.

# --- Types -------------------------------------------------------------------

.check_df <- function(data, arg = "data", call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame, not {.obj_type_friendly {data}}.",
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

.check_col_name <- function(x, arg, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single, non-empty column name.",
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(x)
}

.check_by <- function(by, call = rlang::caller_env()) {
  if (is.null(by)) return(invisible(character()))
  if (!is.character(by) || anyNA(by) || !all(nzchar(by))) {
    cli::cli_abort(
      c("{.arg by} must be a character vector of column names.",
        i = "Use {.code character()} for no grouping, or e.g. {.code by = \"site\"}."),
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  if (anyDuplicated(by)) {
    cli::cli_abort(
      "{.arg by} contains duplicate column names: {.val {unique(by[duplicated(by)])}}.",
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(by)
}

.check_positive_int <- function(x, arg, allow_zero = FALSE,
                                call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) >= 1L && !anyNA(x) &&
    all(is.finite(x)) && all(x == trunc(x)) &&
    all(if (allow_zero) x >= 0 else x > 0)
  if (!ok) {
    floor_txt <- if (allow_zero) "non-negative" else "positive"
    cli::cli_abort(
      "{.arg {arg}} must be {floor_txt} whole number{?s}.",
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(as.integer(x))
}

# --- Columns -----------------------------------------------------------------

.check_cols_exist <- function(data, cols, call = rlang::caller_env()) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c("Column{?s} not found in {.arg data}: {.val {missing}}.",
        i = "Available column{?s}: {.val {names(data)}}."),
      class = c("sporelag_error_missing_col", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# Refuse to silently clobber an existing column. The package contract is
# "columns are appended, never modified in place" -- overwriting would break it.
.check_output_cols <- function(data, cols, call = rlang::caller_env()) {
  clash <- intersect(cols, names(data))
  if (length(clash) > 0L) {
    cli::cli_abort(
      c("Column{?s} {.val {clash}} already exist{?s/} in {.arg data}.",
        x = "SporeLag never overwrites an existing column.",
        i = "Pass a different output name, or drop the existing column first."),
      class = "sporelag_error_bad_input",
      call  = call
    )
  }
  invisible(data)
}

.check_numeric_col <- function(data, value, call = rlang::caller_env()) {
  x <- data[[value]]
  if (!is.numeric(x)) {
    cli::cli_abort(
      c("Column {.val {value}} must be numeric, not {.cls {class(x)}}.",
        i = "Lags and moving averages are only defined for numeric exposures."),
      class = c("sporelag_error_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# --- Dates -------------------------------------------------------------------
# .check_date_col() is TYPE only; .check_date_complete() is NA only.
# They are always called as a pair. Kept separate so the error a user sees
# names exactly one problem.

.check_date_col <- function(data, date, call = rlang::caller_env()) {
  x <- data[[date]]
  if (!inherits(x, "Date")) {
    cli::cli_abort(
      c("Column {.val {date}} must be class {.cls Date}, not {.cls {class(x)}}.",
        x = "SporeLag does not guess date formats or coerce silently.",
        i = "Convert explicitly first, e.g. {.code as.Date(x, format = \"%Y-%m-%d\")}."),
      class = c("sporelag_error_date_type", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

.check_date_complete <- function(data, date, call = rlang::caller_env()) {
  x <- data[[date]]
  if (anyNA(x)) {
    n <- sum(is.na(x))
    cli::cli_abort(
      c("Column {.val {date}} contains {n} missing date{?s}.",
        x = "A row with no date cannot be placed on a temporal grid.",
        i = "Drop or repair these rows before calling SporeLag functions."),
      class = c("sporelag_error_missing_date", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# --- Groups ------------------------------------------------------------------

.check_group_complete <- function(data, by, call = rlang::caller_env()) {
  if (length(by) == 0L) return(invisible(data))
  bad <- by[vapply(by, function(cl) anyNA(data[[cl]]), logical(1L))]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c("Grouping column{?s} {.val {bad}} contain{?s/} missing values.",
        x = "Rows with an unknown group would be silently dropped when
             splitting the data into series.",
        i = "Recode {.code NA} to an explicit level, or drop these rows."),
      class = c("sporelag_error_group_na", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# Deterministic within-group identifier. Levels are sorted, so split() iterates
# groups reproducibly. "\r" cannot occur in a plausible site/taxon label, so it
# cannot collide the way interaction()'s "." separator can.
.group_ids <- function(data, by) {
  if (is.null(by) || length(by) == 0L) {
    return(factor(rep.int("__all__", nrow(data))))
  }
  keys <- lapply(by, function(cl) as.character(data[[cl]]))
  factor(do.call(paste, c(keys, list(sep = "\r"))))
}

.check_no_dup_dates <- function(data, date, by = character(),
                                call = rlang::caller_env()) {
  if (nrow(data) == 0L) return(invisible(data))

  gid <- .group_ids(data, by)
  key <- paste(as.integer(gid), as.integer(data[[date]]), sep = "\r")
  dup <- duplicated(key)

  if (any(dup)) {
    n     <- sum(dup)
    first <- data[[date]][which(dup)[1L]]
    where <- if (length(by) == 0L) "" else " within a group"
    cli::cli_abort(
      c("Found {n} duplicate date{?s}{where}.",
        x = "First duplicate: {.val {format(first)}}.",
        i = "SporeLag requires one row per date per group.",
        i = if (length(by) == 0L) {
          "If this is multi-site data, pass {.arg by} (e.g. {.code by = \"site\"})."
        } else {
          "Aggregate to one observation per date before proceeding."
        }),
      class = c("sporelag_error_duplicate_dates", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# --- DD-02: the gate ---------------------------------------------------------
# apply_lag() and build_moving_average() index by POSITION. That is equivalent
# to indexing by TIME only if the daily grid is complete. If it is not, a
# "1-day lag" silently becomes a lag of however many days separate two adjacent
# rows. Never auto-complete: that hides a data-quality problem at precisely the
# moment the analyst needs to be interrupted.
#
# Gaps are detected PER GROUP. The union of dates across sites can be complete
# while any individual site is gapped.

.check_regular_grid <- function(data, date, by = character(),
                                call = rlang::caller_env()) {
  .check_no_dup_dates(data, date, by, call = call)
  if (nrow(data) == 0L) return(invisible(data))

  d   <- data[[date]]
  gid <- .group_ids(data, by)
  idx <- split(seq_len(nrow(data)), gid)

  n_missing <- vapply(idx, function(rows) {
    dd <- d[rows]
    as.integer(as.numeric(max(dd) - min(dd)) + 1L - length(dd))
  }, integer(1L))

  if (any(n_missing > 0L)) {
    total <- sum(n_missing)
    n_grp <- sum(n_missing > 0L)
    detail <- if (length(by) == 0L) {
      "Found {total} missing day{?s}."
    } else {
      "Found {total} missing day{?s} across {n_grp} group{?s}."
    }
    cli::cli_abort(
      c("Date sequence is not a complete daily grid.",
        x = detail,
        i = "Lagging a gapped series shifts by {.emph position}, not by
             {.emph time}, which silently misaligns exposure and outcome.",
        ">" = "Call {.fn complete_daily_grid} first, then re-run."),
      class = c("sporelag_error_gaps", "sporelag_error_bad_input"),
      call  = call
    )
  }
  invisible(data)
}

# --- Shared preflight for the temporal functions -----------------------------
# Single entry point so the DD-02 gate CANNOT be forgotten. If each function
# assembled its own preflight, the day someone adds a third temporal function
# and omits .check_regular_grid() is the day the package's core guarantee
# quietly dies.

.check_temporal_input <- function(data, value, date, by,
                                  call = rlang::caller_env()) {
  .check_df(data, call = call)
  .check_col_name(value, "value", call = call)
  .check_col_name(date, "date", call = call)
  by <- .check_by(by, call = call)
  .check_cols_exist(data, c(value, date, by), call = call)
  .check_date_col(data, date, call = call)
  .check_date_complete(data, date, call = call)
  .check_group_complete(data, by, call = call)
  .check_numeric_col(data, value, call = call)
  .check_regular_grid(data, date, by, call = call)  # also checks duplicates
  invisible(data)
}
