## Resubmission

This is a resubmission. In this version I have:

* Addressed the reviewer comment "Please provide a proper vignette, not just
  library(SporeLag), or omit it entirely." The `getting-started` vignette
  now gives a full, executable walkthrough of the package on the bundled
  `pollen_demo` data set.
* Increased the version number to 0.1.2.

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Test environments

* local macOS (R 4.6.1), R CMD check --as-cran
* win-builder (devel and release)

## Notes

* This is the first submission of SporeLag to CRAN.
* win-builder (R-devel) flags "Aeroallergen" in the DESCRIPTION as possibly
  misspelled. This is a false positive; "aeroallergen" is a standard term in
  environmental epidemiology.
