d <- data.frame(
  site  = c("A", "A", "B"),
  date  = as.Date(c("2024-03-01", "2024-03-02", "2024-03-01")),
  count = c(1, 2, 3)
)

test_that("type validators fire with the right class", {
  expect_error(.check_df(1:3), class = "sporelag_error_type")
  expect_error(.check_col_name(c("a", "b"), "date_col"), class = "sporelag_error_type")
  expect_error(.check_by(1:2), class = "sporelag_error_type")
  expect_error(.check_by(c("site", "site")), class = "sporelag_error_type")
  expect_error(.check_positive_int(0, "window"), class = "sporelag_error_type")
  expect_error(.check_positive_int(2.5, "window"), class = "sporelag_error_type")
  expect_silent(.check_positive_int(0, "lags", allow_zero = TRUE))
})

test_that("missing columns are named in the error", {
  expect_error(.check_cols_exist(d, "nope"), class = "sporelag_error_missing_col")
  expect_error(.check_cols_exist(d, "nope"), regexp = "nope")
})

test_that("date column must be Date, and complete", {
  chr <- transform(d, date = as.character(date))
  expect_error(.check_date_col(chr, "date"), class = "sporelag_error_date_type")

  pos <- d; pos$date <- as.POSIXct(pos$date)
  expect_error(.check_date_col(pos, "date"), class = "sporelag_error_date_type")

  na_d <- d; na_d$date[2] <- NA
  expect_error(.check_date_complete(na_d, "date"), class = "sporelag_error_missing_date")

  expect_silent(.check_date_col(d, "date"))
})

test_that("NA in a grouping column errors", {
  bad <- d; bad$site[2] <- NA
  expect_error(.check_group_complete(bad, "site"), class = "sporelag_error_group_na")
})

test_that("duplicate dates error, and grouping resolves the false positive", {
  # same date twice, different sites: legal WITH by, illegal without
  expect_silent(.check_no_dup_dates(d, "date", by = "site"))
  expect_error(.check_no_dup_dates(d, "date"), class = "sporelag_error_duplicate_dates")

  dup <- rbind(d, d[1, ])
  expect_error(.check_no_dup_dates(dup, "date", by = "site"),
               class = "sporelag_error_duplicate_dates")
})

test_that(".check_regular_grid enforces DD-02 (hard error on gaps)", {
  gapped <- data.frame(
    date  = as.Date(c("2024-03-01", "2024-03-02", "2024-03-05")),
    count = c(1, 2, 3)
  )
  expect_error(.check_regular_grid(gapped, "date"), class = "sporelag_error_gaps")

  full <- data.frame(
    date  = seq(as.Date("2024-03-01"), as.Date("2024-03-05"), by = "day"),
    count = 1:5
  )
  expect_silent(.check_regular_grid(full, "date"))
})

test_that("gaps are detected per group, not globally", {
  # Union of dates is complete; site B alone is gapped.
  x <- data.frame(
    site = c("A", "A", "A", "B", "B"),
    date = as.Date(c("2024-03-01", "2024-03-02", "2024-03-03",
                     "2024-03-01", "2024-03-03")),
    count = 1:5
  )
  expect_error(.check_regular_grid(x, "date", by = "site"),
               class = "sporelag_error_gaps")
})

test_that(".group_id is deterministic and order-stable", {
  expect_identical(.group_id(d, "site"), .group_id(d, "site"))
  expect_identical(levels(.group_id(d, "site")), c("A", "B"))
  expect_length(levels(.group_id(d, NULL)), 1L)
})
