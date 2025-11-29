# IRTM (development version)

* Added a `NEWS.md` file to track changes to the package.

# IRTM 1.0.0.9

## New Features

* Added continuous outcome support via `family = "continuous"` argument
  - New function: `M_constrained_irt_continuous()` for Gaussian measurement model
  - Updated `irt_m()` wrapper to route to binary or continuous sampler based on `family` parameter
  - New C++ sampler: `sample_constrained_irt_continuous()`

## Breaking Changes

* None - backward compatible with binary IRT-M workflows

## Limitations

* Neither mode supprots missing values (NAs), users must drop or pre-impute via algorithm of choice
* Assumes Gaussian measurement model; performance degrades under strong violations (e.g. log-normal)
