# IRT-M R package

The IRT-M R package is a package that allows users to estimate multiple, potentially correlated, latent dimensions with substantive meaning\(s\). This solves a long-running problem with Item Response Theory models, which allow for measurement of latent or abstract concepts by (1) identifying latent dimensions in data and (2) assigning data units to a position in the latent dimension\(s\). Classic IRT models produce latent dimensions by generating a model that best predicts the underlying data. Unfortunately, latent dimensions produced by IRT models do not intrinsically capture theoretical concepts. IRT models also place units along the dominant dimension\(s\) present in the data--- which may or may not be the dimension of interest.

## Getting Started

You can install the IRTM package using `devtools` or `pak` by using the following code:

Via `devtools`:

```
install.packages("devtools")
library(devtools)
devtools::install_github("dasiegel/IRT-M", force=TRUE)
library(IRTM)
```

Via `pak`:

```
install.packages("pak")
library(pak)
pak::pkg_install("dasiegel/IRT-M")
library(IRTM)
```

### Troubleshooting Installation Problems

One common installation problem on Mac OS machines arises when R is unable to link to the GCC compiler. When that happens, the package installer will throw an error that includes the following:

```
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [IRTM.so] Error 1
ERROR: compilation failed for package ‘IRTM’
```

We have had success with the following steps:

- Make sure you have Xcode installed and updated
- Reinstall gcc and gfortran using `brew`. You can do this by opening a terminal window and typing:
```
brew reinstall gcc
brew reinstall gfortran
```
- Update to the current release of gfortan by going to the CRAN distribution page: https://cran.r-project.org/bin/macosx/tools/
