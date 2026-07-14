# Design Decisions

This file records architectural decisions made during SporeLag development.
Each entry captures what was decided, why, and what the decision forecloses —
so future contributors do not have to reconstruct the reasoning, and do not
silently reverse a decision by accepting an AI autocomplete.

---

## DD-01 — ISO week: base R implementation (not lubridate)

**Date:** 2026-07-13  
**Status:** Accepted  
**Affected phases:** Phase 1 (utils-dates.R), Phase 2 (assign_iso_week), Phase 4 (imputation)

### Decision

Implement `.iso_week()` and `.iso_year()` internally in base R using the
nearest-Thursday algorithm. `lubridate` remains in `Suggests` and is used
**only** as an independent test oracle in `tests/testthat/test-iso-oracle.R`.

### Implementation

The nearest-Thursday algorithm (~12 lines, fully vectorized):

```r
.iso_wday <- function(x) {
  wd <- as.POSIXlt(x)$wday        # 0 = Sunday ... 6 = Saturday
  ifelse(wd == 0L, 7L, as.integer(wd))
}

.iso_thursday <- function(x) x + (4L - .iso_wday(x))

.iso_year <- function(x) as.integer(format(.iso_thursday(x), "%Y"))

.iso_week <- function(x) {
  thu  <- .iso_thursday(x)
  jan1 <- as.Date(paste0(format(thu, "%Y"), "-01-01"))
  as.integer(as.numeric(thu - jan1) %/% 7L) + 1L
}
```

Verified boundary cases:

| Date       | Weekday | ISO result  |
|------------|---------|-------------|
| 2019-12-30 | Mon     | 2020-W01    |
| 2020-12-31 | Thu     | 2020-W53    |
| 2021-01-01 | Fri     | 2020-W53    |
| 2026-01-01 | Thu     | 2026-W01    |
| 2027-01-01 | Fri     | 2026-W53    |

### Rationale

`strftime(x, "%V")` / `"%G"` is platform-inconsistent (historically
incorrect on Windows), which is a reproducibility risk across operating
systems. The base-R implementation is exact, deterministic, and
dependency-free. Correctness is guaranteed by (a) pinned boundary tests
for known edge cases and (b) an oracle test asserting agreement with
`lubridate::isoweek()` / `isoyear()` across every day from 1990–2050.

### Consequence

- `Imports` stays at `{cli, rlang}`. This decision is the reason.
- Requires `tests/testthat/test-iso-oracle.R` with
  `skip_if_not_installed("lubridate")`.
- The helpers are internal (`.iso_week`, `.iso_year`) — not exported.
- Do **not** replace these with `lubridate` calls in `R/`, even if the
  platform issue is later resolved. The dependency cost is not worth it.

---

## DD-02 — Gap handling: hard error (not silent auto-completion)

**Date:** 2026-07-13  
**Status:** Accepted  
**Affected phases:** Phase 3 (apply_lag, build_moving_average)

### Decision

`apply_lag()` and `build_moving_average()` call an internal
`.check_regular_grid()` and abort with condition class
`"sporelag_error_gaps"` if the date sequence is not a complete daily grid.
They do **not** silently auto-complete the grid.

### Error message

```r
abort_gaps <- function(n_gaps, first_gap, by_label) {
  cli::cli_abort(
    c(
      "Date sequence is not a complete daily grid.",
      "x" = "Found {n_gaps} missing day{?s}{by_label}; first gap after {.val {first_gap}}.",
      "i" = "Lagging a gapped series shifts by {.emph position}, not by {.emph time}, \\
             which silently misaligns exposure and outcome.",
      ">" = "Call {.fn complete_daily_grid} first, then re-run."
    ),
    class = "sporelag_error_gaps"
  )
}
```

### Rationale

Lagging a gapped series shifts by **position**, not by **time**. The
resulting exposure column looks plausible, passes through a regression
without error, and yields a silently misaligned lag-response estimate —
the analyst has no feedback that anything is wrong. Auto-completion would
hide a data-quality problem at precisely the moment the analyst most
needs to be interrupted.

Making `complete_daily_grid()` an explicit, auditable step in every
pipeline is worth the extra line of code. It also makes the gap-filling
decision visible in version control and code review.

### Consequence

- Every pipeline (vignettes, examples, README) must open with
  `complete_daily_grid()`. This is intentional and pedagogical.
- The classed error (`"sporelag_error_gaps"`) means tests can catch it
  specifically with `expect_error(..., class = "sporelag_error_gaps")`.
- Do **not** add a `fill_gaps = TRUE` convenience argument to
  `apply_lag()` or `build_moving_average()`. It would silently re-open
  the failure mode this decision exists to prevent.

---

## DD-03 — R build toolchain: pin to `/usr/bin/clang` via `~/.R/Makevars`

**Date:** 2026-07-13  
**Status:** Accepted  
**Scope:** Developer machine (macOS 26 Tahoe, arm64) — not a package artifact

### Problem

`devtools::check()` failed with:

```
clang version 7.0.0 (tags/RELEASE_700/final)
clang-7: warning: using sysroot for 'MacOSX' but targeting 'iPhone'
ld: library 'System' not found
```

**Root cause:** `/usr/local/clang7/bin` appeared on `PATH` before `/usr/bin`,
so R resolved `clang` to a CRAN-provided LLVM 7 binary (~2018) instead of
Apple's system compiler. That old compiler targeted the iOS SDK rather than
macOS, making it impossible to link even a trivial C file.

A previous `~/.R/Makevars` compounded the problem by hardcoding
`-isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk` into
`CFLAGS`/`CXXFLAGS`, which is incompatible with the stale compiler.

### Fix

`~/.R/Makevars` was updated to pin `CC`/`CXX` to `/usr/bin/clang`, the macOS
shim that always resolves through `xcode-select -p` (currently
`/Applications/Xcode.app` → Apple clang 21). The stale `-isysroot` flags
were removed.

Current `~/.R/Makevars`:

```makefile
CC=/usr/bin/clang
CXX=/usr/bin/clang++
CXX11=/usr/bin/clang++
CXX14=/usr/bin/clang++
CXX17=/usr/bin/clang++
CXX20=/usr/bin/clang++

CPPFLAGS=-I/opt/homebrew/include -I/usr/local/include
LDFLAGS=-L/opt/homebrew/lib -L/usr/local/lib
```

### Verification

```r
pkgbuild::check_build_tools(debug = TRUE)
# Your system is ready to build packages!
# using C compiler: 'Apple clang version 21.0.0 (clang-2100.1.1.101)'
```

### If this breaks again

1. Run `which clang` — if it returns anything other than `/usr/bin/clang`,
   a PATH-resident compiler is intercepting R's lookup.
2. Run `cat ~/.R/Makevars` — confirm `CC=/usr/bin/clang` is present.
3. Run `xcode-select -p` — should point to
   `/Applications/Xcode.app/Contents/Developer`.
4. If Xcode was updated or reinstalled: `sudo xcode-select -r` to reset,
   then re-verify with `pkgbuild::check_build_tools(debug = TRUE)`.
