test_that("base-R ISO implementation agrees with lubridate across 1990-2050", {
  skip_if_not_installed("lubridate")

  # lubridate is Suggests-only and used ONLY here, as an independent oracle.
  # This is what permanently retires the strftime("%V") portability risk
  # without paying the dependency cost. If this ever fails, DD-01 is wrong.
  x <- seq(as.Date("1990-01-01"), as.Date("2050-12-31"), by = "day")

  expect_identical(.iso_week(x), as.integer(lubridate::isoweek(x)))
  expect_identical(.iso_year(x), as.integer(lubridate::isoyear(x)))
})
