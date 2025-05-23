% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_analysis.R
\name{dim_corr}
\alias{dim_corr}
\title{learned correlations}
\usage{
dim_corr(cov_array, dim_names = NULL)
}
\arguments{
\item{cov_array}{An array of dimension (d x d x nsamp/thin) containing posterior samples of the relevant covariance matrix.}

\item{dim_names}{Vector of dimension names.}
}
\value{
A data frame containing the correlation matrix derived from t input covariance array, with rows and columns labeled according to dim_names (if provided). Each cell represents the correlation between the corresponding dimensions.
}
\description{
Takes as input either the Sigma covariance matrix, if the user has learned the factor covariance,
or the Omega covariance matrix, if the user has learned the loading covariance, as well as a vector of
dimension names.
Returns a correlation matrix with correlations between the dimensions.
}
