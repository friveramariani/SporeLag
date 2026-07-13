test_that("single gap is filled with NA for value columns", {
  df <- data.frame(
    date  = as.Date(c("2024-01-01", "2024-01-03")),
    count = c(1L, 3L)
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(nrow(result), 3L)
  expect_equal(result$date, as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")))
  expect_equal(result$count, c(1L, NA_integer_, 3L))
})

test_that("multiple gaps are all filled", {
  df <- data.frame(
    date  = as.Date(c("2024-01-01", "2024-01-05")),
    x     = c(10, 50)
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(nrow(result), 5L)
  expect_equal(result$date, seq(as.Date("2024-01-01"), as.Date("2024-01-05"), by = "day"))
  expect_true(all(is.na(result$x[2:4])))
})

test_that("already-complete grid is returned unchanged", {
  df <- data.frame(
    date  = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    count = c(1L, 2L, 3L)
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(nrow(result), 3L)
  expect_equal(result$count, df$count)
})

test_that("single-row input is returned unchanged", {
  df <- data.frame(date = as.Date("2024-06-15"), value = 42)
  result <- complete_daily_grid(df, date = "date")

  expect_equal(nrow(result), 1L)
  expect_equal(result$value, 42)
})

# --- Grouped behaviour --------------------------------------------------------

test_that("gaps are filled independently within each group", {
  df <- data.frame(
    site  = c("A", "A", "A", "B", "B"),
    date  = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04",
                       "2024-01-01", "2024-01-03")),
    count = c(1L, 2L, 4L, 10L, 30L)
  )
  result <- complete_daily_grid(df, date = "date", by = "site")

  expect_equal(nrow(result), 7L)

  a_rows <- result[result$site == "A", ]
  b_rows <- result[result$site == "B", ]

  # Site A: 4 days, gap on Jan 3
  expect_equal(nrow(a_rows), 4L)
  expect_true(is.na(a_rows$count[a_rows$date == as.Date("2024-01-03")]))

  # Site B: 3 days, gap on Jan 2
  expect_equal(nrow(b_rows), 3L)
  expect_true(is.na(b_rows$count[b_rows$date == as.Date("2024-01-02")]))
})

test_that("group-safety: original values are not altered by filling another group", {
  df <- data.frame(
    site  = c("A", "A", "B", "B"),
    date  = as.Date(c("2024-01-01", "2024-01-03",
                       "2024-01-01", "2024-01-03")),
    count = c(1L, 3L, 100L, 300L)
  )
  result <- complete_daily_grid(df, date = "date", by = "site")

  a_filled <- result[result$site == "A" & result$date == as.Date("2024-01-02"), ]
  b_filled <- result[result$site == "B" & result$date == as.Date("2024-01-02"), ]

  # Filled rows in each group must be NA, not each other's values
  expect_true(is.na(a_filled$count))
  expect_true(is.na(b_filled$count))
})

test_that("same date appearing in different groups is not a duplicate error", {
  df <- data.frame(
    site = c("A", "B"),
    date = as.Date(c("2024-01-01", "2024-01-01")),
    val  = c(1, 2)
  )
  expect_no_error(complete_daily_grid(df, date = "date", by = "site"))
})

test_that("multiple grouping columns are supported", {
  df <- data.frame(
    site   = c("A", "A", "B", "B"),
    year   = c(2023L, 2023L, 2023L, 2023L),
    date   = as.Date(c("2023-01-01", "2023-01-03",
                        "2023-01-01", "2023-01-03")),
    count  = c(1L, 3L, 10L, 30L)
  )
  result <- complete_daily_grid(df, date = "date", by = c("site", "year"))

  expect_equal(nrow(result), 6L)
  expect_equal(ncol(result), ncol(df))
})

# --- Return contract ----------------------------------------------------------

test_that("output has the same columns as input, in the same order", {
  df <- data.frame(
    z    = c(9, 7),
    date = as.Date(c("2024-03-01", "2024-03-03")),
    a    = c("x", "y"),
    stringsAsFactors = FALSE
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(names(result), names(df))
})

test_that("no extra columns are added", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-03")),
    val  = c(1, 3)
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(ncol(result), ncol(df))
})

test_that("output is sorted by by then date", {
  df <- data.frame(
    site  = c("B", "A", "B", "A"),
    date  = as.Date(c("2024-01-02", "2024-01-01", "2024-01-01", "2024-01-02")),
    count = 1:4
  )
  result <- complete_daily_grid(df, date = "date", by = "site")

  expect_equal(result$site, c("A", "A", "B", "B"))
  expect_equal(result$date, as.Date(c("2024-01-01", "2024-01-02",
                                       "2024-01-01", "2024-01-02")))
})

test_that("row names are reset to NULL (sequential integers)", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-03")),
    val  = c(1, 3)
  )
  result <- complete_daily_grid(df, date = "date")

  expect_equal(rownames(result), c("1", "2", "3"))
})

test_that("result is a data.frame", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    val  = c(1, 2)
  )
  expect_s3_class(complete_daily_grid(df, date = "date"), "data.frame")
})

# --- Edge cases ---------------------------------------------------------------

test_that("zero-row input is returned unchanged", {
  df <- data.frame(date = as.Date(character(0)), val = integer(0))
  result <- complete_daily_grid(df, date = "date")

  expect_equal(nrow(result), 0L)
  expect_equal(names(result), names(df))
})

test_that("groups with different date ranges are handled independently", {
  df <- data.frame(
    site  = c("A", "A", "B", "B"),
    date  = as.Date(c("2024-01-01", "2024-01-05",
                       "2024-06-01", "2024-06-04")),
    count = c(1L, 5L, 10L, 40L)
  )
  result <- complete_daily_grid(df, date = "date", by = "site")

  expect_equal(nrow(result[result$site == "A", ]), 5L)
  expect_equal(nrow(result[result$site == "B", ]), 4L)
})

# --- Error: sporelag_error_bad_input ------------------------------------------

test_that("non-data.frame input raises sporelag_error_bad_input", {
  expect_error(
    complete_daily_grid(list(date = as.Date("2024-01-01")), date = "date"),
    class = "sporelag_error_bad_input"
  )
})

test_that("non-string date argument raises sporelag_error_bad_input", {
  df <- data.frame(date = as.Date("2024-01-01"), val = 1)
  expect_error(
    complete_daily_grid(df, date = 1L),
    class = "sporelag_error_bad_input"
  )
})

test_that("date argument of length > 1 raises sporelag_error_bad_input", {
  df <- data.frame(date = as.Date("2024-01-01"), val = 1)
  expect_error(
    complete_daily_grid(df, date = c("date", "val")),
    class = "sporelag_error_bad_input"
  )
})

test_that("non-character by argument raises sporelag_error_bad_input", {
  df <- data.frame(date = as.Date("2024-01-01"), val = 1)
  expect_error(
    complete_daily_grid(df, date = "date", by = 1L),
    class = "sporelag_error_bad_input"
  )
})

test_that("missing date column raises sporelag_error_bad_input", {
  df <- data.frame(val = 1)
  expect_error(
    complete_daily_grid(df, date = "date"),
    class = "sporelag_error_bad_input"
  )
})

test_that("missing by column raises sporelag_error_bad_input", {
  df <- data.frame(date = as.Date("2024-01-01"), val = 1)
  expect_error(
    complete_daily_grid(df, date = "date", by = "site"),
    class = "sporelag_error_bad_input"
  )
})

test_that("non-Date date column raises sporelag_error_bad_input", {
  df <- data.frame(date = "2024-01-01", val = 1, stringsAsFactors = FALSE)
  expect_error(
    complete_daily_grid(df, date = "date"),
    class = "sporelag_error_bad_input"
  )
})

test_that("POSIXct date column raises sporelag_error_bad_input", {
  df <- data.frame(date = as.POSIXct("2024-01-01"), val = 1)
  expect_error(
    complete_daily_grid(df, date = "date"),
    class = "sporelag_error_bad_input"
  )
})

# --- Error: sporelag_error_duplicate_dates ------------------------------------

test_that("duplicate dates in ungrouped series raise sporelag_error_duplicate_dates", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-01")),
    val  = c(1, 2)
  )
  expect_error(
    complete_daily_grid(df, date = "date"),
    class = "sporelag_error_duplicate_dates"
  )
})

test_that("duplicate dates within a group raise sporelag_error_duplicate_dates", {
  df <- data.frame(
    site = c("A", "A", "B"),
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    val  = c(1, 2, 3)
  )
  expect_error(
    complete_daily_grid(df, date = "date", by = "site"),
    class = "sporelag_error_duplicate_dates"
  )
})
