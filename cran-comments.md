## Resubmission

This is a maintenance update for IRTM.

Changes in this version:
- Fixed an anchor-handling issue that could lead to invalid sampler output in some cases.
- Updated package tests and cleaned package infrastructure.
- Transferred package maintainership to David Siegel <david.siegel@duke.edu>.

## Test environments
- local macOS, R version 4.4.1 (2024-06-14)
- devtools::check(cran = TRUE)

## R CMD check results
- 0 errors | 0 warnings | 1 note
- NOTE: unable to verify current time

## Additional comments
The current CRAN checks for the previous release show an installation error on r-devel-windows-x86_64,
while r-release and r-oldrel Windows checks are OK.
This appears to be a forward-compatibility issue in the Rcpp/C++ toolchain rather than a regression introduced in this update.
We will investigate this issue in a future update.
