test_that("pollen_demo has the documented structure", {
  expect_s3_class(pollen_demo, "data.frame")
  expect_identical(names(pollen_demo), c("site", "date", "count"))
  expect_s3_class(pollen_demo$date, "Date")
  expect_type(pollen_demo$count, "integer")
  expect_setequal(unique(pollen_demo$site), c("North", "South"))
})

test_that("pollen_demo contains BOTH defects the package addresses", {
  # (1) Gaps: rows absent entirely. is.na() cannot see these.
  expect_error(
    apply_lag(pollen_demo, value = "count", lags = 1, date = "date", by = "site"),
    class = "sporelag_error_gaps"
  )

  # (2) Missing values: rows present, count is NA.
  expect_true(anyNA(pollen_demo$count))
  expect_true(all(tapply(pollen_demo$count, pollen_demo$site, anyNA)))
})

test_that("the documented gap windows are the ones actually missing", {
  north <- pollen_demo$date[pollen_demo$site == "North"]
  south <- pollen_demo$date[pollen_demo$site == "South"]

  expect_false(as.Date("2024-03-20") %in% north)   # documented North outage
  expect_true(as.Date("2024-03-20") %in% south)    # South was fine that day

  expect_false(as.Date("2024-04-02") %in% south)   # documented South outage
  expect_true(as.Date("2024-04-02") %in% north)
})

test_that("no duplicate dates within a site", {
  expect_equal(anyDuplicated(pollen_demo[c("site", "date")]), 0L)
})

test_that("the canonical pipeline runs end to end on pollen_demo", {
  out <- pollen_demo |>
    complete_daily_grid(date = "date", by = "site") |>
    assign_iso_week(date = "date") |>
    assign_season(date = "date") |>
    impute_weekly_mean(value = "count", by = "site") |>
    build_moving_average(value = "count_imputed", window = c(3, 7),
                         date = "date", by = "site") |>
    apply_lag(value = "count_imputed", lags = 0:3, date = "date", by = "site")

  expect_true(all(c(
    "iso_week", "iso_year", "season",
    "count_imputed", "count_imputed_flag",
    "count_imputed_ma3", "count_imputed_ma7",
    "count_imputed_lag0", "count_imputed_lag1",
    "count_imputed_lag2", "count_imputed_lag3"
  ) %in% names(out)))

  # complete_daily_grid() closed the gaps, so both sites now span 137 days
  expect_equal(nrow(out), 2 * 137)

  # Raw column untouched
  expect_true("count" %in% names(out))
})
