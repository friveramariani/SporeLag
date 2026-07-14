# ISO 8601 week/year, base R (DD-01).
#
# strftime(x, "%V") / "%G" is platform-dependent (historically on Windows),
# which is a reproducibility risk. This implementation is exact and portable.
#
# The ISO week containing a date is identified by its Thursday: the ISO year is
# the calendar year of that Thursday, and the week number is the count of
# 7-day blocks from Jan 1 of that year.

# Monday = 1 ... Sunday = 7
.iso_wday <- function(x) {
  wd <- as.POSIXlt(x)$wday          # 0 = Sunday ... 6 = Saturday
  ifelse(wd == 0L, 7L, as.integer(wd))
}

.iso_thursday <- function(x) x + (4L - .iso_wday(x))

.iso_year <- function(x) {
  as.integer(format(.iso_thursday(x), "%Y"))
}

.iso_week <- function(x) {
  thu  <- .iso_thursday(x)
  jan1 <- as.Date(paste0(format(thu, "%Y"), "-01-01"))
  as.integer(as.numeric(thu - jan1) %/% 7L) + 1L
}

# Complete daily sequence spanning the observed range.
.expand_dates <- function(x) {
  if (length(x) == 0L) return(as.Date(character(0)))
  seq(min(x), max(x), by = "day")
}
