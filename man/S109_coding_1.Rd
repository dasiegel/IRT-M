% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{S109_coding_1}
\alias{S109_coding_1}
\title{Constraints matrix for analyzing the S109 Legislative session}
\format{
A data frame with observations on various policy dimensions:
\describe{
\item{Index1}{Index column from Excel},
\item{Clerk_Sn_Vote_Num}{},
\item{Vote_Num}{},
\item{Defense_Security_4}{Indicator for whether the vote related to defense or security issues (column 4)}
\item{Economic_Development}{Indicator for whether the vote related to economic development}
\item{Civil_Rights_Social_Equality_6}{Indicator for whether the vote related to civil rights or social equality (column 6)}
\item{Entitlements_Redistribution_Welfare}{Indicator for whether the vote related to entitlements, redistribution, or welfare programs}
\item{Socio_Cultural}{Indicator for whether the vote related to socio-cultural dimensions}
\item{Col_9}{Index column from Excel}
\item{Economic_Redistribution_10}{Indicator for economic or redistribution dimensions (column 10)}
\item{Social_Cultural_11}{Indicator for vote related to social or cultural dimensions (column 11)}
\item{Civil_Rights_Social_Equality_12}{Indicator for civil rights or social equality dimensions (column 12)}
\item{Defense_Security_13}{Indicator for defense or security dimensions (column 13)}
\item{Col_14}{Index column from Excel}
\item{Economic_Redistribution_15}{Indicator for vote related to economic or redistribution dimension (column 15)}
\item{Social_Cultural_16}{Indicator for social or cultural dimension (column 16)}
\item{Civil_Rights_Social_Equality_17}{Indicator for civil rights or social equality dimension (column 17)}
\item{Col_18}{Index column from Excel}
\item{Economic_Redistribution_19}{Indicator for economic or redistribution dimension (column 19)}
\item{Social_Cultural_Civil_Rights_Equality}{Indicator for social, cultural, civil rights, or equality dimension}
}

A data frame with observations on various policy dimensions:
}
\source{
Voting records for the 109th Congressional session with IRT-M coding for underlying dimensions.
}
\description{
A dataset of the constraints matrix for IRT-M analysis of votes in the 109th U.S. Senate.
The format is unlovely because it was coded in Excel and not harmonized with R.
It is retained as an intermediary step between research team processing and IRT-M analysis.
The column names were cleaned up in March 2025 to remove special characters
}
\details{
This dataset provides coding classifications for Senate votes across multiple policy dimensions.
The coding scheme categorizes votes into underlying dimensions including: defense/security, economic issues, civil rights,
entitlements, and socio-cultural. Columns represent different
coding schemes or levels of classification. The values take 1 for positive loading, -1 for negative loading, 0 for no loading, and NA for unknown.
}
\seealso{
\code{\link{S109_votes}}, \code{\link{S109_rollcalls}}
}
