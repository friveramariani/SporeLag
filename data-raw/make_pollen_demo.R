# Generates data/pollen_demo.rda -- a SYNTHETIC aeroallergen dataset.
#
# Deterministic: the RNG kind is pinned explicitly, not just the seed, so this
# reproduces identically across R versions. Re-running this script must produce
# a byte-identical .rda.
#
# The dataset deliberately contains BOTH data defects SporeLag addresses:
#   (1) GAPS          -- calendar days with no row at all  -> complete_daily_grid()
#   (2) MISSING VALUES -- rows present, count is NA         -> impute_weekly_mean()
# These are different problems and are commonly conflated. pollen_demo lets the
# vignette show why they need different tools.

set.seed(
  1017,
  kind          = "Mersenne-Twister",
  normal.kind   = "Inversion",
  sample.kind   = "Rejection"
)

# --- 1. A plausible spring tree-pollen season --------------------------------

start_date <- as.Date("2024-02-15")
end_date   <- as.Date("2024-06-30")
all_days   <- seq(start_date, end_date, by = "day")

sites <- data.frame(
  site      = c("North", "South"),
  peak_day  = as.Date(c("2024-04-12", "2024-04-02")),  # South peaks earlier
  peak_conc = c(820, 340),                              # North is a heavier site
  spread    = c(26, 22),                                # days (Gaussian sd)
  stringsAsFactors = FALSE
)

make_site <- function(site, peak_day, peak_conc, spread) {
  # Gaussian seasonal curve, floored so counts never go to exactly zero.
  offset <- as.numeric(all_days - peak_day)
  mu     <- peak_conc * exp(-0.5 * (offset / spread)^2) + 0.5

  # Negative binomial: pollen counts are overdispersed, not Poisson.
  count <- stats::rnbinom(length(all_days), mu = mu, size = 3)

  data.frame(
    site  = site,
    date  = all_days,
    count = as.integer(count),
    stringsAsFactors = FALSE
  )
}

pollen_demo <- do.call(
  rbind,
  Map(make_site, sites$site, sites$peak_day, sites$peak_conc, sites$spread)
)
rownames(pollen_demo) <- NULL

# --- 2. Inject MISSING VALUES (row present, count = NA) ----------------------
# Scattered single-day sampling failures: ~6% of days, independently by site.

for (s in sites$site) {
  in_site <- which(pollen_demo$site == s)
  n_na    <- round(0.06 * length(in_site))
  pollen_demo$count[sample(in_site, n_na)] <- NA_integer_
}

# --- 3. Inject GAPS (row absent entirely) ------------------------------------
# Deliberate, named outages so the vignette can point at them:
#   North: a 5-day instrument failure at the season's rising limb.
#   South: a 3-day outage near peak -- the worst possible time to lose data,
#          and exactly the case where a naive positional lag does most damage.

gap_north <- seq(as.Date("2024-03-18"), as.Date("2024-03-22"), by = "day")
gap_south <- seq(as.Date("2024-04-01"), as.Date("2024-04-03"), by = "day")

drop <- (pollen_demo$site == "North" & pollen_demo$date %in% gap_north) |
  (pollen_demo$site == "South" & pollen_demo$date %in% gap_south)

pollen_demo <- pollen_demo[!drop, ]
rownames(pollen_demo) <- NULL

# Guarantee the year-boundary case is NOT present here -- pollen_demo is a
# single season within one calendar year. ISO-year edge cases are covered by
# unit-test fixtures, not by the demo data.

# --- 4. Sanity checks (fail loudly rather than ship bad demo data) ------------

stopifnot(
  is.data.frame(pollen_demo),
  identical(names(pollen_demo), c("site", "date", "count")),
  inherits(pollen_demo$date, "Date"),
  is.integer(pollen_demo$count),
  # gaps really exist
  nrow(pollen_demo) < 2 * length(all_days),
  # missing values really exist
  anyNA(pollen_demo$count),
  # no duplicate dates within a site
  !anyDuplicated(pollen_demo[c("site", "date")]),
  # and both defects exist in BOTH sites
  all(tapply(pollen_demo$count, pollen_demo$site, anyNA))
)

usethis::use_data(pollen_demo, overwrite = TRUE)
