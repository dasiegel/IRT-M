# IRT-M R package

The IRT-M R package is a package that allows users to estimate multiple, potentially correlated, latent dimensions with substantive meaning\(s\) and place data units on the dimensions. It does so by having users specify a constraints matrix for the test items (e.g., agreement features, survey and test questions, votes, etc.) before estimating an IRT model on the data.

IRT-M solves a long-running problem with Item Response Theory models: classic IRT models produce latent dimensions by generating a model that best predicts the underlying data and then places the data units on the discovered dimension. These dimensions do not intrinsically capture theoretical concepts and may or may not be the dimension of interest. Users of IRT models have to make post hoc interpretations of the resulting dimensions based on prior knowledge of the underlying units. 

IRT-M prompts users to specify a constraints matrix, which structures the dimensions that the model returns. Creating the constraints matrix based on one’s theory does impose upfront costs. However, this is the step that produces measurements of positions on conceptually meaningful latent dimensions while eliminating the need for exogenous information to identify the model.   If the theory (1) captures some aspect of the process that generated the data and (2) the constraint matrix coding is applied consistently, the model will produce measures of relevant theoretical concepts that are constant in meaning across disparate data sources and across time and place. The constraint matrix can be omitted for a conventional IRT estimation of the data. 

You can find additional motivating examples, developed applications, and a technical presentation of the IRT-M model in the [paper](https://arxiv.org/abs/2111.11979). 

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

## Use and Examples

The IRT-M model takes a set of data composed of an array of choices (e.g., answers to survey questions; votes on bills; elements of treaties) made by data units (e.g., survey respondents; legislators; peace treaties). 

The present iteration of IRT-M requires dichotomous choices, with any non-dichotomous choice needing to be reformatted into a series of dichotomous ones. 

In the pre-analysis step, the analyst hand-codes each choice according to its connection to each latent dimension, recalling
that latent dimensions capture specific theoretical concepts from one’s theory (e.g., perception of threat from immigration; ideological position; degree to which peace treaty captures minority rights and security).



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
