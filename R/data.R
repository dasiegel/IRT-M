#' Datasets for IRT-M Package
#'
#' Datasets for constrained IRT examples. 
#' 
#' The S109 data is used in:
#' Morucci, Marco, Margaret J. Foster, Kaitlyn Webster, So Jin Lee, and David A. Siegel.
#'"Measurement That Matches Theory: Theory-Driven Identification in Item Response Theory Models."
#'  American Political Science Review (2024): 1-19.
#'  
#MCodes, synth_idvs, and synth_questions are included in the vignette

#' @name S109_rollcalls
#' @docType data
#' @title Roll Call Votes for S109 Legislative Session
#' @description 
#' Record of roll call votes during the S109 legislative session. 
#' Each vote includes the NOMINATE scores associated with the vote
#' 
#' @format A data frame with the following variables:
  #' \describe{
  #'   \item{congress}{Congressional session number (109)}
  #'   \item{chamber}{Chamber ("Senate")}
  #'   \item{rollnumber}{Identifying number for the vote}
  #'   \item{date}{Date when the vote occurred}
  #'   \item{session}{Session (1 or 2)}
  #'   \item{clerk_rollnumber}{Roll call number assigned by the Clerk of the House or Secretary of the Senate}
  #'   \item{yea_count}{Count of "Yea" affirmative votes}
  #'   \item{nay_count}{Count of "Nay" negative votes}
  #'   \item{nominate_mid_1}{Midpoint estimate for the first NOMINATE dimension}
  #'   \item{nominate_mid_2}{The midpoint estimate for the second NOMINATE dimension}
  #'   \item{nominate_spread_1}{Spread parameter for the first NOMINATE dimension}
  #'   \item{nominate_spread_2}{Spread parameter for the second NOMINATE dimension}
  #'   \item{nominate_log_likelihood}{Log-likelihood of the NOMINATE estimates}
  #'   \item{bill_number}{An identifying number of the bill or resolution being voted on}
  #'   \item{vote_result}{Vote outcome (e.g., "Passed" or "Failed")}
  #'   \item{vote_desc}{Text description of the substance of the vote}
  #'   \item{vote_question}{Parliamentary question being voted on (e.g., "On Passage", "On Motion to Suspend the Rules")}
  #'   \item{dtl_desc}{Detailed description of the bill, when avaliable}
  #' }
  #'
#' 
#' @source 
#' PSCL package: 
#' Jackman S (2024). pscl: Classes and Methods for R Developed in the Political Science Computational Laboratory.
#'  University of Sydney, Sydney, Australia. R package version 1.5.9, https://github.com/atahk/pscl/. 
#' 
#' @keywords datasets
NULL

#' @name S109_votes
#' @docType data
#' @title Comprehensive Voting Data
#' @description 
#' Aggregated voting information for the S109 legislative session.
#' 
#' @format Similar documentation as above...
#' congress	chamber	rollnumber	icpsr	cast_code	prob
#' @keywords datasets
NULL

#' @name MCodes
#' @docType data
#' @title Methodological Codes
#' @description 
#' Factor loading matrix for IRT-M vignette. This is a 793 row and 9 column dataset.
#' The rows are derived from the binary encoding of the synthetic survey, with a row for every
#' binarized question in the survey. The first 56 rows are retained metadata, and have lots of NA.
#' 
#' The data format is an intermediary processing for IRT-M, and is detailed in the vignette text.
#' 
#'  @format A data frame with the following variables:
#'  [MJF: to here 3/09]
#'#' \describe{
#'   \item{QCode}{Mapping of the dimension coding key to the underlying question in the original (synthetic) survey data.
#'    The first 56 rows are blank because they map to survey and respondent metadata that doesn't relate to dimensions.}
#'   \item{QMap}{Mapping to the question in data processed by the vignette for variable estimation. }
#'   \item{SubstantiveNotes}{Brief human readable comments on the substantive meaning of the coded questions. These are for convenience of reference.}
#'   \item{D1-Culture threat}{Loading vector for the cultural threat dimension. A 1 indicates that the question is expected to load.}
#'   \item{D2-ReligionThreat}{Loading vector for the religious threat dimension.}
#'   \item{D3-Economic Threat}{Loading vector for the economic threat dimension.}
#'   \item{D4-HealthThreat}{Loading vector for the health threat dimension.}
#'   \item{O1-OutcomeSupportImmigration}{Loading vector for the immigration support composite.}
#'   \item{O2-OutcomeSupportEU}{Loading vector for the European Union support composite.}
#' }

#' @source
#' IRT-M vignette walk through.
#' 
#' @keywords datasets
NULL

#' @name synth_idvs
#' @docType data
#' @title Synthetic Independent Variables
#' @description 
#' A synthetic dataset of independent variables for post-estimate analysis in the vignette.
#' Extraction of the data from the synthetic survey is described in the vignette.
#' 
#' @format A 3000 row and 27 column dataset of synthetic survey responses. This closely follows the 94.3 Eurobarometer survey in structure.
#' This dataset is a toy that is intended for the IRT-M vignette. For real analysis, see the original Eurobarometer data collection. 
#' 

#' @keywords datasets
NULL

#' @name synth_questions
#' @docType data
#' @title Questions for the Synthetic European sentiment survey in the vignette
#' @description 
#' A synthetic dataset with 3000 rows and 148 questions.
#' This data replicates the structure of questions following Eurobarometer 94.3 (2021).
#' It is not intended to be analyzed independently from the vignette.
#'
#'
#' @format A data frame containing synthetic survey question data
#' 
#' @keywords datasets
NULL

#' @name S109_coding_1
#' @docType data
#' @title Constraints matrix for analyzing the S109 Legislative session
#' @description
#'
#' A dataset of the constraints matrix for IRT-M analysis of votes in the 109th U.S. Senate.
#' The format is unlovely because it was coded in Excel and not harmonized with R.
#'  It is retained as an intermediary step between research team processing and IRT-M analysis.
#'  
#' @format A data frame with observations on various policy dimensions:
#' \describe{
#'   \item{...1}{Index or identifier column}
#'   \item{Clerk Sn Vote #}{Clerk Senate Vote Number identifier}
#'   \item{Vote #}{Vote number identifier}
#'   \item{Defense Security...4}{Indicator for whether the vote is positive, neutral, or negatively related to defense or security issues (column 4)}
#'   \item{Economic Development}{Indicator for whether the vote is positive, neutral, or negatively related to economic development}
#'   \item{Civil Rights Social Equality...6}{Indicator for whether the vote is positive, neutral, or negatively related to civil rights or social equality (column 6)}
#'   \item{Entitlements RedistributionWelfare}{Indicator for whether the vote is positive, neutral, or negatively related to entitlements, redistribution, or welfare programs}
#'   \item{Socio-Cultural}{Indicator for whether the vote is positive, neutral, or negatively related to socio-cultural issues}
#'   \item{...9}{Junk Excel column that should have been dropped in formatting, but is here to not interfere with the example code indexes.}
#'   \item{Economic Redistribution...10}{Indicator for whether the vote is positive, neutral, or negatively related to economic or redistribution issues (column 10)}
#'   \item{Social Cultural...11}{Indicator for whether the vote is positive, neutral, or negatively related to social or cultural issues (column 11)}
#'   \item{Civil Rights Social Equality...12}{Indicator for whether the vote is positive, neutral, or negatively related to civil rights or social equality (column 12)}
#'   \item{Defense Security...13}{Indicator for whether the vote is positive, neutral, or negatively related to defense or security issues (column 13)}
#'   \item{...14}{Excel formatting retained for indexing}
#'   \item{Economic Redistribution...15}{Indicator for whether the vote is positive, neutral, or negatively related to economic or redistribution issues (column 15)}
#'   \item{Social Cultural...16}{Indicator for whether the vote is positive, neutral, or negatively related to social or cultural issues (column 16)}
#'   \item{Civil Rights Social Equality...17}{Indicator for whether the vote is positive, neutral, or negatively related to civil rights or social equality (column 17)}
#'   \item{...18}{Excel formatting retained for indexing}
#'   \item{Economic Redistribution...19}{Indicator for whether the vote is positive, neutral, or negatively related to economic or redistribution issues (column 19)}
#'   \item{Social Cultural Civil Rights Equality}{Indicator for whether the vote is positive, neutral, or negatively related to social, cultural, civil rights, or equality issues}
#' }
#'
#' @details This dataset provides coding classifications for Senate votes across multiple policy dimensions.
#' The coding scheme categorizes votes into areas such as defense/security, economic issues, civil rights,
#' entitlements, and socio-cultural issues. Multiple columns for each category may represent different
#' coding schemes or levels of classification.
#'
#' @source Voting records for the 109th Congressional session with IRT-M coding for underlying dimensions.
#'
#'
#' @seealso \code{\link{S109_votes}}, \code{\link{S109_rollcalls}}
#' 
NULL