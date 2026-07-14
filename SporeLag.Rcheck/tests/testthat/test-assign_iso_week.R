# --- Boundary fixtures (the whole point of DD-01) -----------------------------

test_that("ISO year/week are correct at the Dec/Jan boundary", {
  # Hand-verified against the ISO 8601 nearest-Thursday rule.
  df <- data.frame(date = as.Date(c(
    "2019-12-30",  # Mon -> 2020-W01  (week 1 begins in the PREVIOUS calendar year)
    "2020-12-31",  # Thu -> 2020-W53  (a 53-week year)
    "2021-01-01",  # Fri -> 2020-W53  (January date, previous ISO year)
    "2023-01-01",  # Sun -> 2022-W52
    "2026-01-01",  # Thu -> 2026-W01
    "2027-01-01"   # Fri -> 2026-W53
  )))
  out <- assign_iso_week(df, date = "date")

  expect_equal(out$iso_year, c(2020L, 2020L, 2020L, 2022L, 2026L, 2026L))
  expect_equal(out$iso_week, c(1L,    53L,   53L,   52L,   1L,    53L))
})

test_that("iso_year differs from calendar year exactly when it should", {
  df <- data.frame(date = as.Date(c("2021-01-01", "2021-06-15")))
  out <- assign_iso_week(df, date = "date")

  expect_equal(out$iso_year[1], 2020L)   # calendar 2021, ISO 2020
  expect_equal(out$iso_year[2], 2021L)   # mid-year: they agree
})

test_that("week is always in 1:53 and year is integer", {
  df <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2029-12-31"), by = "day"))
  out <- assign_iso_week(df, date = "date")

  expect_true(all(out$iso_week >= 1L & out$iso_week <= 53L))
  expect_type(out$iso_week, "integer")
  expect_type(out$iso_year, "integer")
})

test_that("leap day is handled", {
  df <- data.frame(date = as.Date("2024-02-29"))   # Thursday
  out <- assign_iso_week(df, date = "date")
  expect_equal(out$iso_year, 2024L)
  expect_equal(out$iso_week, 9L)
})

# --- Contract ----------------------------------------------------------------

test_that("columns are appended, input is preserved, row order unchanged", {
  df <- data.frame(
    z = c(9, 7),
    date = as.Date(c("2024-03-05", "2024-03-01")),
    a = c("x", "y")
  )
  out <- assign_iso_week(df, date = "date")

  expect_equal(names(out), c("z", "date", "a", "iso_week", "iso_year"))
  expect_equal(out$z, df$z)            # NOT re-sorted: this is a row-wise map
  expect_equal(out$date, df$date)
})

test_that("custom output column names are honoured", {
  df <- data.frame(date = as.Date("2024-03-01"))
  out <- assign_iso_week(df, date = "date", week_col = "wk", year_col = "yr")
  expect_true(all(c("wk", "yr") %in% names(out)))
})

test_that("zero-row input returns zero-row output with the columns present", {
  df <- data.frame(date = as.Date(character(0)), val = integer(0))
  out <- assign_iso_week(df, date = "date")

  expect_equal(nrow(out), 0L)
  expect_true(all(c("iso_week", "iso_year") %in% names(out)))
})

test_that("no complete grid is required (this is a row-wise map)", {
  gapped <- data.frame(date = as.Date(c("2024-01-01", "2024-06-01")))
  expect_no_error(assign_iso_week(gapped, date = "date"))
})

# --- Errors ------------------------------------------------------------------

test_that("existing output column is not silently overwritten", {
  df <- data.frame(date = as.Date("2024-03-01"), iso_week = 99L)
  expect_error(assign_iso_week(df, date = "date"),
               class = "sporelag_error_bad_input")
})

test_that("bad input raises the documented classes", {
  df <- data.frame(date = as.Date("2024-03-01"))

  expect_error(assign_iso_week(list(date = 1), date = "date"),
               class = "sporelag_error_bad_input")
  expect_error(assign_iso_week(df, date = "nope"),
               class = "sporelag_error_bad_input")
  expect_error(assign_iso_week(data.frame(date = "2024-03-01"), date = "date"),
               class = "sporelag_error_bad_input")
  expect_error(assign_iso_week(data.frame(date = as.Date(c("2024-03-01", NA))),
                               date = "date"),
               class = "sporelag_error_missing_date")
})
