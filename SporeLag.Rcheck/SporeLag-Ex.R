pkgname <- "SporeLag"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SporeLag')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("assign_iso_week")
### * assign_iso_week

flush(stderr()); flush(stdout())

### Name: assign_iso_week
### Title: Assign ISO 8601 week and year
### Aliases: assign_iso_week

### ** Examples

df <- data.frame(
  date = as.Date(c("2019-12-30", "2020-12-31", "2021-01-01")),
  count = c(1, 2, 3)
)
assign_iso_week(df, date = "date")




cleanEx()
nameEx("assign_season")
### * assign_season

flush(stderr()); flush(stdout())

### Name: assign_season
### Title: Assign a season label
### Aliases: assign_season

### ** Examples

df <- data.frame(date = as.Date(c("2024-01-15", "2024-04-15", "2024-07-15")))

assign_season(df, date = "date")
assign_season(df, date = "date", hemisphere = "southern")

# A taxon-specific pollen season
assign_season(
  df,
  date = "date",
  definition = "custom",
  breaks = c(Dormant = "11-01", Tree = "02-15", Grass = "05-01",
             Ragweed = "08-15")
)




cleanEx()
nameEx("complete_daily_grid")
### * complete_daily_grid

flush(stderr()); flush(stdout())

### Name: complete_daily_grid
### Title: Build a complete daily grid
### Aliases: complete_daily_grid

### ** Examples

df <- data.frame(
  site = c("A", "A", "A", "B", "B"),
  date = as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-04",
    "2024-01-01", "2024-01-03"
  )),
  count = c(1, 2, 4, 10, 30)
)
complete_daily_grid(df, date = "date", by = "site")




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
