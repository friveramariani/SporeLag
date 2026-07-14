# Internal validators. None are exported.
#
# All conditions carry a class so tests and downstream callers can catch them
# specifically:
#   sporelag_error_type            - wrong type for an argument
#   sporelag_error_missing_col     - named column absent from `data`
#   sporelag_error_date_type       - date column is not class Date
#   sporelag_error_missing_date    - NA present in the date column
#   sporelag_error_group_na        - NA present in a grouping column
#   sporelag_error_duplicate_dates - duplicate date within a group
#   sporelag_error_gaps            - date sequence is not a complete daily grid

.check_df <- function(data, arg = "data", call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame, not {.obj_type_friendly {data}}.",
      class = "sporelag_error_type",
      call  = call
    )
  }
  invisible(data)
}

.check_col_name <- function(x, arg, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single, non-empty column name.",
      class = "sporelag_error_type",
      call  = call
    )
  }
  invisible(x)
}

.check_by <- function(by, call = rlang::caller_env()) {
  if (is.null(by)) return(invisible(NULL))
  if (!is.character(by) || anyNA(by) || !all(nzchar(by))) {
    cli::cli_abort(
      c("{.arg by} must be {.code NULL} or a character vector of column names.",
        i = "Pass column names as strings, e.g. {.code by = c(\"site\", \"taxon\")}."),
      class = "sporelag_error_type",
      call  = call
    )
  }
  if (anyDuplicated(by)) {
    cli::cli_abort(
      "{.arg by} contains duplicate column names: {.val {unique(by[duplicated(by)])}}.",
      class = "sporelag_error_type",
      call  = call
    )
  }
  invisible(by)
}

.check_cols_exist <- function(data, cols, call = rlang::caller_env()) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c("Column{?s} not found in {.arg data}: {.val {missing}}.",
        i = "Available column{?s}: {.val {names(data)}}."),
      class = "sporelag_error_missing_col",
      call  = call
    )
  }
  invisible(data)
}

.check_date_col <- function(data, date_col, call = rlang::caller_env()) {
  x <- data[[date_col]]

  if (!inherits(x, "Date")) {
    cli::cli_abort(
      c("Column {.val {date_col}} must be class {.cls Date}, not {.cls {class(x)}}.",
        x = "SporeLag does not guess date formats or coerce silently.",
        i = "Convert explicitly first, e.g. {.code as.Date(x, format = \"%Y-%m-%d\")}."),
      class = "sporelag_error_date_type",
      call  = call
    )
  }

  if (anyNA(x)) {
    n <- sum(is.na(x))
    cli::cli_abort(
      c("Column {.val {date_col}} contains {n} missing date{?s}.",
        x = "A row with no date cannot be placed on a temporal grid.",
        i = "Drop or repair these rows before calling SporeLag functions."),
      class = "sporelag_error_missing_date",
      call  = call
    )
  }

  invisible(data)
}

.check_group_cols <- function(data, by, call = rlang::caller_env()) {
  if (is.null(by)) return(invisible(data))
  bad <- by[vapply(by, function(cl) anyNA(data[[cl]]), logical(1L))]
  if (length(bad) > 0L) {
    cli::cli_abort(
      c("Grouping column{?s} {.val {bad}} contain{?s/} missing values.",
        x = "Rows with an unknown group cannot be assigned to a time series.",
        i = "Recode {.code NA} to an explicit level, or drop these rows."),
      class = "sporelag_error_group_na",
      call  = call
    )
  }
  invisible(data)
}

.check_positive_int <- function(x, arg, allow_zero = FALSE,
                                call = rlang::caller_env()) {
  ok <- is.numeric(x) && length(x) >= 1L && !anyNA(x) &&
    all(x == trunc(x)) && all(is.finite(x)) &&
    all(if (allow_zero) x >= 0 else x > 0)
  if (!ok) {
    floor_txt <- if (allow_zero) "non-negative" else "positive"
    cli::cli_abort(
      "{.arg {arg}} must be {floor_txt} whole number{?s}.",
      class = "sporelag_error_type",
      call  = call
    )
  }
  invisible(as.integer(x))
}

# Stable, deterministic within-group identifier.
# Returns a factor whose levels are sorted, so split() ordering is reproducible.
.group_id <- function(data, by) {
  if (is.null(by) || length(by) == 0L) {
    return(factor(rep.int("__all__", nrow(data))))
  }
  keys <- lapply(by, function(cl) as.character(data[[cl]]))
  # "\r" is not a plausible value in a site/taxon label, so it is a safe separator
  factor(do.call(paste, c(keys, list(sep = "\r"))))
}

.check_no_dup_dates <- function(data, date_col, by = NULL,
                                call = rlang::caller_env()) {
  gid <- .group_id(data, by)
  key <- paste(as.integer(gid), as.integer(data[[date_col]]), sep = "\r")
  dup <- duplicated(key)
  if (any(dup)) {
    n     <- sum(dup)
    first <- data[[date_col]][which(dup)[1L]]
    where <- if (is.null(by)) "" else " within a group"
    cli::cli_abort(
      c("Found {n} duplicate date{?s}{where}.",
        x = "First duplicate: {.val {format(first)}}.",
        i = "SporeLag requires one row per date per group.",
        i = if (is.null(by)) {
          "If this is multi-site data, pass {.arg by} (e.g. {.code by = \"site\"})."
        } else {
          "Aggregate to one observation per date before proceeding."
        }),
      class = "sporelag_error_duplicate_dates",
      call  = call
    )
  }
  invisible(data)
}

# DD-02: hard error on gaps. Never auto-complete.
.check_regular_grid <- function(data, date_col, by = NULL,
                                call = rlang::caller_env()) {
  .check_no_dup_dates(data, date_col, by, call = call)
  if (nrow(data) == 0L) return(invisible(data))

  d   <- data[[date_col]]
  gid <- .group_id(data, by)
  idx <- split(seq_len(nrow(data)), gid)

  n_missing <- vapply(idx, function(rows) {
    dd <- d[rows]
    as.integer(as.numeric(max(dd) - min(dd)) + 1L - length(dd))
  }, integer(1L))

  if (any(n_missing > 0L)) {
    total  <- sum(n_missing)
    n_grp  <- sum(n_missing > 0L)
    grp_ln <- if (is.null(by)) "" else " across {n_grp} group{?s}"
    cli::cli_abort(
      c("Date sequence is not a complete daily grid.",
        x = paste0("Found {total} missing day{?s}", grp_ln, "."),
        i = "Lagging a gapped series shifts by {.emph position}, not by {.emph time},
             which silently misaligns exposure and outcome.",
        ">" = "Call {.fn complete_daily_grid} first, then re-run."),
      class = "sporelag_error_gaps",
      call  = call
    )
  }
  invisible(data)
}

# Deterministic within-group identifier.
# "\r" cannot occur in a plausible site/taxon label, so it cannot collide
# the way interaction()'s "." separator can.
.group_ids <- function(data, by) {
  if (length(by) == 0) {
    return(factor(rep.int("__all__", nrow(data))))
  }
  keys <- lapply(by, function(cl) as.character(data[[cl]]))
  factor(do.call(paste, c(keys, list(sep = "\r"))))
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

.check_group_complete <- function(data, by, call = rlang::caller_env()) {
  if (length(by) == 0) return(invisible(data))
  bad <- by[vapply(by, function(cl) anyNA(data[[cl]]), logical(1))]
  if (length(bad) > 0) {
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
