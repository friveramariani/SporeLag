# --- Meteorological -----------------------------------------------------------

test_that("meteorological seasons follow calendar months (northern)", {
  df <- data.frame(date = as.Date(c(
    "2024-01-15", "2024-03-01", "2024-06-01", "2024-09-01", "2024-12-01"
  )))
  out <- assign_season(df, date = "date")

  expect_equal(as.character(out$season),
               c("Winter", "Spring", "Summer", "Autumn", "Winter"))
})

test_that("December wraps to Winter, not to a phantom fifth season", {
  df <- data.frame(date = as.Date(c("2024-12-31", "2024-01-01")))
  out <- assign_season(df, date = "date")
  expect_equal(as.character(out$season), c("Winter", "Winter"))
})

test_that("southern hemisphere is offset by six months", {
  df <- data.frame(date = as.Date(c("2024-01-15", "2024-07-15")))
  out <- assign_season(df, date = "date", hemisphere = "southern")
  expect_equal(as.character(out$season), c("Summer", "Winter"))
})

# --- Astronomical -------------------------------------------------------------

test_that("astronomical boundaries fall on the documented fixed dates", {
  df <- data.frame(date = as.Date(c(
    "2024-03-19", "2024-03-20",   # last day of Winter | first of Spring
    "2024-06-20", "2024-06-21",
    "2024-09-21", "2024-09-22",
    "2024-12-20", "2024-12-21"
  )))
  out <- assign_season(df, date = "date", definition = "astronomical")

  expect_equal(as.character(out$season),
               c("Winter", "Spring", "Spring", "Summer",
                 "Summer", "Autumn", "Autumn", "Winter"))
})

# --- Custom (the pollen-season case) ------------------------------------------

test_that("custom breaks wrap the year boundary correctly", {
  # Dormant starts Nov 1 and runs THROUGH January into mid-February.
  brk <- c(Dormant = "11-01", Tree = "02-15", Grass = "05-01", Ragweed = "08-15")
  df <- data.frame(date = as.Date(c(
    "2024-01-15",  # Jan -> before the first break of the year -> wraps to Dormant
    "2024-02-15",  # Tree begins
    "2024-05-01",  # Grass begins
    "2024-08-15",  # Ragweed begins
    "2024-11-01",  # Dormant begins
    "2024-12-31"   # still Dormant
  )))
  out <- assign_season(df, date = "date", definition = "custom", breaks = brk)

  expect_equal(as.character(out$season),
               c("Dormant", "Tree", "Grass", "Ragweed", "Dormant", "Dormant"))
})

test_that("break order in the input vector is irrelevant", {
  a <- c(Tree = "02-15", Grass = "05-01", Dormant = "11-01")
  b <- c(Dormant = "11-01", Grass = "05-01", Tree = "02-15")
  df <- data.frame(date = as.Date(c("2024-01-15", "2024-03-01", "2024-06-01")))

  expect_identical(
    assign_season(df, date = "date", definition = "custom", breaks = a)$season,
    assign_season(df, date = "date", definition = "custom", breaks = b)$season
  )
})

# --- Factor contract ----------------------------------------------------------

test_that("season is an ordered factor with chronological levels", {
  df <- data.frame(date = as.Date("2024-06-01"))
  out <- assign_season(df, date = "date")

  expect_s3_class(out$season, "ordered")
  expect_equal(levels(out$season), c("Winter", "Spring", "Summer", "Autumn"))
})

test_that("levels are present even when a season is unobserved", {
  # Only summer dates -- but the factor must still carry all four levels,
  # or a model matrix built from a subset will silently differ from the full one.
  df <- data.frame(date = as.Date(c("2024-07-01", "2024-07-02")))
  out <- assign_season(df, date = "date")
  expect_equal(nlevels(out$season), 4L)
})

test_that("leap day resolves (does not fall through to NA)", {
  df <- data.frame(date = as.Date("2024-02-29"))
  expect_equal(as.character(assign_season(df, date = "date")$season), "Winter")
  expect_false(is.na(assign_season(df, date = "date",
                                   definition = "astronomical")$season))
})

# --- Errors ------------------------------------------------------------------

test_that("custom definition requires valid breaks", {
  df <- data.frame(date = as.Date("2024-03-01"))

  expect_error(assign_season(df, date = "date", definition = "custom"),
               class = "sporelag_error_bad_input")                       # missing
  expect_error(assign_season(df, date = "date", definition = "custom",
                             breaks = c("02-15", "05-01")),
               class = "sporelag_error_bad_input")                       # unnamed
  expect_error(assign_season(df, date = "date", definition = "custom",
                             breaks = c(A = "02-15", A = "05-01")),
               class = "sporelag_error_bad_input")                       # dup label
  expect_error(assign_season(df, date = "date", definition = "custom",
                             breaks = c(A = "13-01", B = "05-01")),
               class = "sporelag_error_bad_input")                       # bad month
  expect_error(assign_season(df, date = "date", definition = "custom",
                             breaks = c(A = "Feb 15", B = "05-01")),
               class = "sporelag_error_bad_input")                       # wrong form
})

test_that("breaks passed with a non-custom definition is an error, not ignored", {
  df <- data.frame(date = as.Date("2024-03-01"))
  expect_error(
    assign_season(df, date = "date", breaks = c(A = "02-15", B = "05-01")),
    class = "sporelag_error_bad_input"
  )
})

test_that("existing season column is not silently overwritten", {
  df <- data.frame(date = as.Date("2024-03-01"), season = "whatever")
  expect_error(assign_season(df, date = "date"),
               class = "sporelag_error_bad_input")
})

test_that("invalid definition or hemisphere is rejected", {
  df <- data.frame(date = as.Date("2024-03-01"))
  expect_error(assign_season(df, date = "date", definition = "lunar"))
  expect_error(assign_season(df, date = "date", hemisphere = "eastern"))
})
