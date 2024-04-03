# IRT-M R package

The IRT-M R package is a package that allows users to estimate multiple, potentially correlated, latent dimensions with substantive meaning\(s\) and place data units on the dimensions. It does so by having users specify a constraints matrix for the test items (e.g., agreement features, survey and test questions, votes, etc.) before estimating an IRT model on the data.
The current version of the package works with binary data. Other data inputs, such as categorical survey questions or continuous measurements, must be reformatted into a binary format through one-hot encoding or another decision rule. 

IRT-M solves a long-running problem with Item Response Theory models: classic IRT models produce latent dimensions by generating a model that best predicts the underlying data and then places the data units on the discovered dimension. These dimensions do not intrinsically capture theoretical concepts and may or may not be the dimension of interest. Users of IRT models have to make post hoc interpretations of the resulting dimensions based on prior knowledge of the underlying units. 

IRT-M prompts users to specify a constraints matrix, which structures the dimensions that the model returns. Creating the constraints matrix entails hand-codes each choice in the data according to its connection to the latent dimensions of interest (e.g., perception of threat from immigration; ideological position; degree to which peace treaty captures minority rights and security). This imposes upfront costs; however, it is the step that produces measurements of positions on conceptually meaningful latent dimensions while eliminating the need for exogenous information to identify the model. If the theory (1) captures some aspect of the process that generated the data and (2) the constraint matrix coding is applied consistently, the model will produce measures of relevant theoretical concepts that are constant in meaning across disparate data sources and across time and place. The constraint matrix can be omitted for a conventional IRT estimation of the data. 

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

The IRT-M model takes a set of data composed of an array of choices (e.g., answers to survey questions; votes on bills; elements of treaties, indicators from research organizations) made by data units (e.g., survey respondents; legislators; peace treaties). 

Below we present some examples of IRT-M use cases. A full walkthrough can be found in the [vignette](https://github.com/dasiegel/IRT-M/blob/master/vignettes/introduction.Rmd).

(1) Estimated a latent "Satisfaction with the status quo" dimension from 32 countries surveyed by Afrobarometer across three collection waves. 

Without IRT-M, this is a difficult measure to produce for several reasons, including that:
- Respondent interpretations of survey questions vary contextually by location and time
- The survey instrument changes across waves

We address this source of incompatability by coding a separate constraints (M) matrix for each country-year. So long as our coding accurately captures the contextual variation in how respondents interpret and respond to questions, our measure will be consisent. As well, interested parties can review and the matrix coding and either confirm our interpretation or estimate an alternative version that reflects their own understanding.

![allThetasVisualized_UrbanRuralDivide](https://github.com/dasiegel/IRT-M/assets/10012916/65dffdf3-8a9a-4222-b897-53ead9a7e0d1)


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
