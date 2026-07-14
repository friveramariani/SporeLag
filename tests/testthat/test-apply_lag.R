grid2 <- function() {
  data.frame(
    site  = rep(c("A", "B"), each = 4),
    date  = rep(seq(as.Date("2024-05-01"), by = "day", length.out = 4), 2),
    count = c(10L, 20L, 30L, 40L, 100L, 200L, 300L, 400L)
  )
}

# --- Core arithmetic ---------------------------------------------------------

test_that("lag shifts the series backwards by the requested number of days", {
  out <- apply_lag(grid2(), value = "count", lags = 1, date = "date", by = "site")
  expect_equal(out$count_lag1[out$site == "A"], c(NA, 10L, 20L, 30L))
})

test_that("lag 0 is a same-day copy", {
  out <- apply_lag(grid2(), value = "count", lags = 0, date = "date", by = "site")
  expect_equal(out$count_lag0, out$count)
})

test_that("multiple lags produce one column each, correctly named", {
  out <- apply_lag(grid2(), value = "count", lags = 0:2, date = "date", by = "site")
  expect_true(all(c("count_lag0", "count_lag1", "count_lag2") %in% names(out)))
  expect_equal(out$count_lag2[out$site == "A"], c(NA, NA, 10L, 20L))
})

test_that("lag longer than the series yields all NA, not an error", {
  out <- apply_lag(grid2(), value = "count", lags = 10, date = "date", by = "site")
  expect_true(all(is.na(out$count_lag10)))
})

test_that("integer input stays integer", {
  out <- apply_lag(grid2(), value = "count", lags = 1, date = "date", by = "site")
  expect_type(out$count_lag1, "integer")
})

# --- GROUP SAFETY: the highest-priority correctness test ---------------------

test_that("lags do not bleed across `by` groups", {
  # Site B's first row sits immediately after site A's last row. A
  # position-based lag that ignores grouping would put A's 40 into B's lag1.
  out <- apply_lag(grid2(), value = "count", lags = 1, date = "date", by = "site")

  b <- out[out$site == "B", ]
  expect_true(is.na(b$count_lag1[1]))             # NOT 40
  expect_equal(b$count_lag1, c(NA, 100L, 200L, 300L))
})

test_that("group safety holds when groups are interleaved in row order", {
  df <- grid2()[order(grid2()$date), ]            # A,B,A,B,A,B,A,B
  out <- apply_lag(df, value = "count", lags = 1, date = "date", by = "site")

  expect_equal(out$count_lag1[out$site == "A"], c(NA, 10L, 20L, 30L))
  expect_equal(out$count_lag1[out$site == "B"], c(NA, 100L, 200L, 300L))
})

test_that("unsorted input gives the same answer as sorted input", {
  df <- grid2()
  shuffled <- df[c(6, 1, 8, 3, 5, 2, 7, 4), ]

  a <- apply_lag(shuffled, value = "count", lags = 1, date = "date", by = "site")
  a <- a[order(a$site, a$date), ]
  b <- apply_lag(df, value = "count", lags = 1, date = "date", by = "site")

  expect_equal(a$count_lag1, b$count_lag1)
})

test_that("row order of the input is preserved in the output", {
  df <- grid2()[c(3, 1, 8, 5, 2, 7, 4, 6), ]
  out <- apply_lag(df, value = "count", lags = 1, date = "date", by = "site")
  expect_equal(out$count, df$count)               # rows were not re-sorted
  expect_equal(out$date, df$date)
})

# --- DD-02: the gate ---------------------------------------------------------

test_that("a gapped grid is refused", {
  gapped <- data.frame(
    date  = as.Date(c("2024-05-01", "2024-05-02", "2024-05-05")),
    count = c(1, 2, 3)
  )
  expect_error(
    apply_lag(gapped, value = "count", lags = 1, date = "date"),
    class = "sporelag_error_gaps"
  )
})

test_that("a gap in ONE group is refused even when the date union is complete", {
  # Union of dates = May 1-3 with no holes. Site B alone is missing May 2.
  # A global gap check passes this and hands back a corrupted lag.
  df <- data.frame(
    site  = c("A", "A", "A", "B", "B"),
    date  = as.Date(c("2024-05-01", "2024-05-02", "2024-05-03",
                      "2024-05-01", "2024-05-03")),
    count = c(1, 2, 3, 10, 30)
  )
  expect_error(
    apply_lag(df, value = "count", lags = 1, date = "date", by = "site"),
    class = "sporelag_error_gaps"
  )
})

test_that("complete_daily_grid() -> apply_lag() is the sanctioned pipeline", {
  gapped <- data.frame(
    date  = as.Date(c("2024-05-01", "2024-05-02", "2024-05-05")),
    count = c(1, 2, 3)
  )
  out <- complete_daily_grid(gapped, date = "date") |>
    apply_lag(value = "count", lags = 1, date = "date")

  # May 5's lag-1 is May 4, which was NOT OBSERVED -> NA. It is emphatically
  # not 2 (May 2), which is what a position-based lag on the gapped frame
  # would have silently produced.
  expect_equal(out$count_lag1, c(NA, 1, 2, NA, NA))
})

# --- Errors ------------------------------------------------------------------

test_that("negative lags are refused (a lead is not a lag)", {
  expect_error(
    apply_lag(grid2(), value = "count", lags = -1, date = "date", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("non-numeric value column is refused", {
  df <- grid2(); df$count <- as.character(df$count)
  expect_error(
    apply_lag(df, value = "count", lags = 1, date = "date", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("duplicate dates within a group are refused", {
  df <- grid2(); df$date[2] <- df$date[1]
  expect_error(
    apply_lag(df, value = "count", lags = 1, date = "date", by = "site"),
    class = "sporelag_error_duplicate_dates"
  )
})

test_that("existing output column is not silently overwritten", {
  df <- grid2(); df$count_lag1 <- 999
  expect_error(
    apply_lag(df, value = "count", lags = 1, date = "date", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("forgetting `by` on multi-site data errors rather than interleaving", {
  expect_error(
    apply_lag(grid2(), value = "count", lags = 1, date = "date"),
    class = "sporelag_error_duplicate_dates"
  )
})

# --- Determinism -------------------------------------------------------------

test_that("output is bit-identical across runs", {
  a <- apply_lag(grid2(), value = "count", lags = 0:3, date = "date", by = "site")
  b <- apply_lag(grid2(), value = "count", lags = 0:3, date = "date", by = "site")
  expect_identical(a, b)
})
