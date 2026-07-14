# One full ISO week, Mon 2024-05-06 through Sun 2024-05-12 (2024-W19).
wk <- function(count) {
  data.frame(
    site  = rep("A", 7),
    date  = seq(as.Date("2024-05-06"), by = "day", length.out = 7),
    count = count
  ) |>
    assign_iso_week(date = "date")
}

# --- Core behaviour -----------------------------------------------------------

test_that("missing days are filled with the mean of the observed days", {
  out <- wk(c(10, NA, 30, NA, 50, 60, NA)) |>
    impute_weekly_mean(value = "count", by = "site")

  mu <- mean(c(10, 30, 50, 60))              # 37.5
  expect_equal(out$count_imputed, c(10, mu, 30, mu, 50, 60, mu))
})

test_that("the flag marks exactly the filled positions", {
  out <- wk(c(10, NA, 30, NA, 50, 60, NA)) |>
    impute_weekly_mean(value = "count", by = "site")

  expect_equal(out$count_imputed_flag, c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("the input column is NOT modified in place", {
  raw <- c(10, NA, 30, NA, 50, 60, NA)
  out <- wk(raw) |> impute_weekly_mean(value = "count", by = "site")

  expect_equal(out$count, raw)               # raw column survives intact
  expect_true("count" %in% names(out))
})

test_that("a complete week is unchanged and flagged FALSE throughout", {
  out <- wk(1:7) |> impute_weekly_mean(value = "count", by = "site")

  expect_equal(out$count_imputed, as.numeric(1:7))
  expect_false(any(out$count_imputed_flag))
})

test_that("output is double even when input is integer", {
  out <- wk(c(10L, NA, 30L, NA, 50L, 60L, NA)) |>
    impute_weekly_mean(value = "count", by = "site")

  expect_type(out$count_imputed, "double")   # a mean is not a count
  expect_type(out$count_imputed_flag, "logical")
})

# --- min_obs ------------------------------------------------------------------

test_that("min_obs default of 1: a single observed day fills the week", {
  out <- wk(c(NA, NA, 30, NA, NA, NA, NA)) |>
    impute_weekly_mean(value = "count", by = "site")

  expect_true(all(out$count_imputed == 30))
  expect_equal(sum(out$count_imputed_flag), 6L)
})

test_that("min_obs not met: the week is left NA and flagged FALSE", {
  out <- wk(c(NA, NA, 30, NA, NA, NA, NA)) |>
    impute_weekly_mean(value = "count", by = "site", min_obs = 4)

  expect_equal(out$count_imputed, c(NA, NA, 30, NA, NA, NA, NA))
  expect_false(any(out$count_imputed_flag))  # nothing was imputed, so nothing is flagged
})

test_that("an all-NA week stays NA and does not become NaN", {
  out <- wk(rep(NA_real_, 7)) |>
    impute_weekly_mean(value = "count", by = "site")

  expect_true(all(is.na(out$count_imputed)))
  expect_false(any(is.nan(out$count_imputed)))   # mean(numeric(0)) = NaN must not leak
  expect_false(any(out$count_imputed_flag))
})

# --- ISO-year boundary: the bug this design exists to prevent ------------------

test_that("weeks are NOT pooled across ISO years", {
  # 2020-12-31 (Thu) is 2020-W53. 2021-01-04 (Mon) is 2021-W01.
  # Grouping on week number alone would be fine here, but grouping on
  # calendar year would wrongly split 2020-W53 -- and grouping on week alone
  # across MULTIPLE years pools them. This fixture pins the year-aware key.
  df <- data.frame(
    site  = rep("A", 4),
    date  = as.Date(c("2020-12-30", "2020-12-31",   # 2020-W53
                      "2021-01-04", "2021-01-05")), # 2021-W01
    count = c(10, NA, 100, NA)
  ) |>
    assign_iso_week(date = "date")

  out <- impute_weekly_mean(df, value = "count", by = "site")

  expect_equal(out$count_imputed, c(10, 10, 100, 100))  # NOT c(10, 55, 100, 55)
})

test_that("the same ISO week in different years is not pooled", {
  df <- data.frame(
    site  = rep("A", 4),
    date  = as.Date(c("2023-05-08", "2023-05-09",   # 2023-W19
                      "2024-05-06", "2024-05-07")), # 2024-W19
    count = c(10, NA, 100, NA)
  ) |>
    assign_iso_week(date = "date")

  out <- impute_weekly_mean(df, value = "count", by = "site")

  expect_equal(out$count_imputed, c(10, 10, 100, 100))
  expect_false(any(out$count_imputed == 55))   # would be the pooled mean
})

# --- Group safety -------------------------------------------------------------

test_that("means do not bleed across `by` groups", {
  df <- data.frame(
    site  = rep(c("A", "B"), each = 3),
    date  = rep(as.Date(c("2024-05-06", "2024-05-07", "2024-05-08")), 2),
    count = c(10, NA, 30, 1000, NA, 3000)
  ) |>
    assign_iso_week(date = "date")

  out <- impute_weekly_mean(df, value = "count", by = "site")

  expect_equal(out$count_imputed[2], 20)      # mean(10, 30)     -- site A only
  expect_equal(out$count_imputed[5], 2000)    # mean(1000, 3000) -- site B only
})

test_that("forgetting `by` pools sites into one mean (documented, not silent)", {
  # Without `by`, sites ARE pooled -- this is arithmetically correct given the
  # arguments, and it is why `by` must be passed for multi-site data.
  df <- data.frame(
    site  = rep(c("A", "B"), each = 2),
    date  = rep(as.Date(c("2024-05-06", "2024-05-07")), 2),
    count = c(10, NA, 1000, NA)
  ) |>
    assign_iso_week(date = "date")

  out <- impute_weekly_mean(df, value = "count")
  expect_equal(out$count_imputed[2], 505)     # mean(10, 1000) -- pooled
})

# --- Contract & determinism ---------------------------------------------------

test_that("row order is preserved", {
  df <- wk(c(10, NA, 30, NA, 50, 60, NA))
  shuffled <- df[c(5, 1, 7, 3, 2, 6, 4), ]

  out <- impute_weekly_mean(shuffled, value = "count", by = "site")
  expect_equal(out$date, shuffled$date)
  expect_equal(out$count, shuffled$count)
})

test_that("output is bit-identical across runs", {
  a <- wk(c(10, NA, 30, NA, 50, 60, NA)) |>
    impute_weekly_mean(value = "count", by = "site")
  b <- wk(c(10, NA, 30, NA, 50, 60, NA)) |>
    impute_weekly_mean(value = "count", by = "site")

  expect_identical(a, b)
})

test_that("no complete grid is required (imputation does not index by position)", {
  gapped <- data.frame(
    site  = rep("A", 3),
    date  = as.Date(c("2024-05-06", "2024-05-08", "2024-05-10")),
    count = c(10, NA, 30)
  ) |>
    assign_iso_week(date = "date")

  expect_no_error(impute_weekly_mean(gapped, value = "count", by = "site"))
})

# --- Errors -------------------------------------------------------------------

test_that("missing ISO columns point the user at assign_iso_week()", {
  df <- data.frame(
    date  = as.Date("2024-05-06"),
    count = 10
  )
  expect_error(
    impute_weekly_mean(df, value = "count"),
    class = "sporelag_error_missing_col"
  )
  expect_error(impute_weekly_mean(df, value = "count"), regexp = "assign_iso_week")
})

test_that("non-numeric value column is refused", {
  df <- wk(c(10, NA, 30, NA, 50, 60, NA))
  df$count <- as.character(df$count)
  expect_error(
    impute_weekly_mean(df, value = "count", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("invalid min_obs is refused", {
  df <- wk(c(10, NA, 30, NA, 50, 60, NA))
  expect_error(impute_weekly_mean(df, value = "count", by = "site", min_obs = 0),
               class = "sporelag_error_bad_input")
  expect_error(impute_weekly_mean(df, value = "count", by = "site", min_obs = 2.5),
               class = "sporelag_error_bad_input")
})

test_that("existing output columns are not silently overwritten", {
  df <- wk(c(10, NA, 30, NA, 50, 60, NA))
  df$count_imputed <- 999
  expect_error(
    impute_weekly_mean(df, value = "count", by = "site"),
    class = "sporelag_error_bad_input"
  )
})
