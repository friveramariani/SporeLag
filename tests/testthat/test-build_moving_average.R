grid5 <- function() {
  data.frame(
    site  = rep(c("A", "B"), each = 5),
    date  = rep(seq(as.Date("2024-05-01"), by = "day", length.out = 5), 2),
    count = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )
}

# --- Core arithmetic ---------------------------------------------------------

test_that("trailing window includes the current day", {
  out <- build_moving_average(grid5(), value = "count", window = 3,
                              date = "date", by = "site")
  a <- out$count_ma3[out$site == "A"]

  expect_equal(a, c(NA, NA, 20, 30, 40))   # mean(10,20,30)=20; mean(20,30,40)=30
})

test_that("edge windows are NA, not a shorter average", {
  out <- build_moving_average(grid5(), value = "count", window = 3,
                              date = "date", by = "site")
  a <- out$count_ma3[out$site == "A"]

  expect_true(is.na(a[1]))    # a 3-day mean from 1 day is not a 3-day mean
  expect_true(is.na(a[2]))
})

test_that("window = 1 returns the raw series", {
  out <- build_moving_average(grid5(), value = "count", window = 1,
                              date = "date", by = "site")
  expect_equal(out$count_ma1, out$count)
})

test_that("window longer than the series yields all NA", {
  out <- build_moving_average(grid5(), value = "count", window = 10,
                              date = "date", by = "site")
  expect_true(all(is.na(out$count_ma10)))
})

test_that("multiple windows produce one column each", {
  out <- build_moving_average(grid5(), value = "count", window = c(3, 5),
                              date = "date", by = "site")
  expect_true(all(c("count_ma3", "count_ma5") %in% names(out)))
  expect_equal(out$count_ma5[out$site == "A"], c(NA, NA, NA, NA, 30))
})

test_that("centred window is symmetric and requires an odd width", {
  out <- build_moving_average(grid5(), value = "count", window = 3,
                              date = "date", by = "site", align = "center")
  a <- out$count_ma3[out$site == "A"]
  expect_equal(a, c(NA, 20, 30, 40, NA))   # both ends NA

  expect_error(
    build_moving_average(grid5(), value = "count", window = 4,
                         date = "date", by = "site", align = "center"),
    class = "sporelag_error_bad_input"
  )
})

# --- GROUP SAFETY ------------------------------------------------------------

test_that("windows do not bleed across `by` groups", {
  # Site B's first row follows site A's last. A naive rolling mean would pull
  # A's 40 and 50 into B's first window.
  out <- build_moving_average(grid5(), value = "count", window = 3,
                              date = "date", by = "site")
  b <- out$count_ma3[out$site == "B"]

  expect_true(is.na(b[1]))
  expect_true(is.na(b[2]))
  expect_equal(b[3], 200)      # mean(100, 200, 300) -- NOT contaminated by A
})

test_that("group safety holds when groups are interleaved in row order", {
  df <- grid5()[order(grid5()$date), ]
  out <- build_moving_average(df, value = "count", window = 3,
                              date = "date", by = "site")
  expect_equal(out$count_ma3[out$site == "A"], c(NA, NA, 20, 30, 40))
  expect_equal(out$count_ma3[out$site == "B"], c(NA, NA, 200, 300, 400))
})

test_that("row order of the input is preserved", {
  df <- grid5()[c(7, 2, 9, 4, 1, 10, 3, 8, 5, 6), ]
  out <- build_moving_average(df, value = "count", window = 3,
                              date = "date", by = "site")
  expect_equal(out$count, df$count)
})

# --- min_obs / NA handling ---------------------------------------------------

test_that("default min_obs = window: any NA in the window yields NA", {
  df <- grid5()
  df$count[3] <- NA                          # site A, day 3
  out <- build_moving_average(df, value = "count", window = 3,
                              date = "date", by = "site")
  a <- out$count_ma3[out$site == "A"]

  expect_true(all(is.na(a[3:5])))            # every window touching day 3
})

test_that("lowering min_obs averages over the observed days only", {
  df <- grid5()
  df$count[3] <- NA
  out <- build_moving_average(df, value = "count", window = 3, min_obs = 2,
                              date = "date", by = "site")
  a <- out$count_ma3[out$site == "A"]

  expect_equal(a[3], mean(c(10, 20)))        # day 3 window: 10, 20, NA
  expect_equal(a[4], mean(c(20, 40)))
})

test_that("min_obs greater than window is refused (unsatisfiable)", {
  expect_error(
    build_moving_average(grid5(), value = "count", window = 3, min_obs = 5,
                         date = "date", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("an all-NA window yields NA, not NaN", {
  df <- grid5()
  df$count[1:5] <- NA
  out <- build_moving_average(df, value = "count", window = 3, min_obs = 1,
                              date = "date", by = "site")
  a <- out$count_ma3[out$site == "A"]

  expect_true(all(is.na(a)))
  expect_false(any(is.nan(a)))               # 0/0 must not leak through
})

# --- DD-02: the gate ---------------------------------------------------------

test_that("a gapped grid is refused", {
  gapped <- data.frame(
    date  = as.Date(c("2024-05-01", "2024-05-02", "2024-05-05")),
    count = c(1, 2, 3)
  )
  expect_error(
    build_moving_average(gapped, value = "count", window = 3, date = "date"),
    class = "sporelag_error_gaps"
  )
})

test_that("the gap must be closed explicitly, and NA propagates honestly", {
  gapped <- data.frame(
    date  = as.Date(c("2024-05-01", "2024-05-02", "2024-05-05")),
    count = c(1, 2, 3)
  )
  out <- complete_daily_grid(gapped, date = "date") |>
    build_moving_average(value = "count", window = 3, date = "date")

  # Days 3 and 4 were never observed. Every window touching them is NA.
  # A naive rolling mean over the gapped frame would have reported
  # mean(1, 2, 3) = 2 for May 5 -- averaging values five days apart.
  expect_equal(out$count_ma3, rep(NA_real_, 5))
})

# --- Contract ----------------------------------------------------------------

test_that("output is double and deterministic", {
  a <- build_moving_average(grid5(), value = "count", window = c(3, 5),
                            date = "date", by = "site")
  b <- build_moving_average(grid5(), value = "count", window = c(3, 5),
                            date = "date", by = "site")
  expect_identical(a, b)
  expect_type(a$count_ma3, "double")
})
