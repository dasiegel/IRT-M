#' Datasets for IRT-M Package
#'
#' MCodes, synth_idvs, and synth_questions are included in the vignette
#'
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
#' '
#' @format A data frame with 3000 rows and 148 columns representing synthetic survey responses
#'
#' @keywords datasets
NULL
