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
@format A data frame with the following variables:
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

## [MJF 3/8/25: TO HERE]
#' @name S109_votes
#' @docType data
#' @title Comprehensive Voting Data
#' @description 
#' Aggregated voting information for the S109 legislative session.
#' 
#' @format Similar documentation as above...
#' 
#' @keywords datasets
NULL

#' @name MCodes
#' @docType data
#' @title Methodological Codes
#' @description 
#' Coding scheme and reference codes used in the legislative analysis.
#' 
#' @format A data frame or other appropriate data structure
#' 
#' @keywords datasets
NULL

#' @name synth_idvs
#' @docType data
#' @title Synthetic Independent Variables
#' @description 
#' Synthetic dataset of independent variables for statistical analysis.
#' 
#' @format A data frame with synthetic research variables
#' 
#' @keywords datasets
NULL

#' @name synth_questions
#' @docType data
#' @title Synthetic Survey Questions
#' @description 
#' Synthetic dataset of survey questions for research purposes.
#' 
#' @format A data frame containing survey question data
#' 
#' @keywords datasets
NULL