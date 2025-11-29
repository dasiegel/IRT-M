#' @title irt_m
#' @description
#' A wrapper that simplifies the use of the IRT-M model implemented in
#' `M_constrained_irt_family()`. It checks consistency between the respondent-by-item
#' data and an optional constraint matrix, constructs synthetic anchors when needed,
#' prepares the combined dataset, runs the sampler, removes anchors, and returns
#' posterior samples.
#'
#' @details
#' **Inputs.**
#' - `Y_in` must be an `N × K` matrix or data frame with item identifiers matching
#'   the first column of `M_matrix` (if provided).
#' - `M_matrix` must be a `K × (d+1)` matrix with the first column giving item IDs and
#'   the remaining columns specifying diagonal constraint entries.
#'
#' **Family argument.**
#' The `family` argument selects the observation model:
#' - `"binary"` (default)
#' - `"continuous"`
#'
#' **Anchors.**
#' When constraints are provided (`M_matrix` not `NULL`), synthetic anchor responses
#' are generated via `pair_gen_anchors()` to fix the latent trait scale. These anchors
#' are included during sampling but removed from the returned posterior samples.
#'
#' **Learning covariance structures.**
#' If `learn_loadings = FALSE` (default), the sampler learns the covariance matrix
#' of latent traits (`Sigma`).
#' If `learn_loadings = TRUE`, the sampler instead learns the covariance matrix
#' of item loadings (`Omega`). These options are mutually exclusive.
#'
#' @param Y_in
#' A numeric `N × K` response matrix or data frame, with rows representing
#' respondents and columns items. Column names must match the
#' first column of `M_matrix` (if supplied).
#'
#' @param d
#' Integer. Number of latent dimensions.
#'
#' @param M_matrix
#' Optional `K × (d+1)` constraint matrix.
#' Column 1: item identifiers matching `colnames(Y_in)`.
#' Columns 2:(d+1): diagonal constraint entries for each dimension.
#' Default: `NULL`.
#'
#' @param family
#' Character string specifying the response family. One of:
#' - `"binary"`
#' - `"continuous"`
#' Default: `"binary"`.
#'
#' @param nburn
#' Number of burn-in MCMC iterations. Default: `1000`.
#'
#' @param nsamp
#' Number of post–burn-in MCMC iterations. Default: `1000`.
#'
#' @param thin
#' Thinning interval for MCMC samples. Default: `1`.
#'
#' @param learn_loadings
#' Logical. If `FALSE`, learn covariance of latent traits (`Sigma`).
#' If `TRUE`, learn covariance of item loadings (`Omega`).
#' These cannot be learned simultaneously.
#'
#' @return
#' A list containing posterior draws:
#' \itemize{
#'   \item{\code{lambda}}{A \((K × d × nsamp/thin)\) array of item discrimination samples.}
#'   \item{\code{b}}{A \((K × nsamp/thin)\) matrix of item difficulty samples.}
#'   \item{\code{theta}}{A \((N × d × nsamp/thin)\) array of respondent latent trait samples (anchors removed).}
#'   \item{\code{Sigma}}{A \((d × d × nsamp/thin)\) array of latent trait covariance matrices
#'       (returned when \code{learn_loadings = FALSE}).}
#'   \item{\code{Omega}}{A \((d × d × nsamp/thin)\) array of item-loading covariance matrices
#'       (returned when \code{learn_loadings = TRUE}).}
#' }
#'
#' @import RcppProgress
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom truncnorm rtruncnorm
#' @importFrom RcppArmadillo armadillo_version
#' @importFrom RcppDist bayeslm
#' @useDynLib IRTM, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @export


irt_m = function(Y_in, d, M_matrix=NULL,
                 family = c("binary", "continuous"),  ##continuous update
                 nburn=1000, nsamp=1000, thin=1,
                 learn_loadings=FALSE){

  ## Input validation: raise a problem if there are missing values:
  if (anyNA(Y_in)) {
    stop("IRT-M does not support missing values. Impute or remove NAs before fitting the model.")
  }

  ## Flag the data is identified as binary, but values are not 0, 1, NA
  if (family == "binary" && !all(Y_in %in% c(0, 1))) {
    stop("Binary IRT-M requires Y_in values in {0, 1}. Use family='continuous' for continuous outcomes.")
  }

  ## flag if data identified as continuous, but seem to be binary:
  if (family == "continuous" && all(Y_in %in% c(0, 1))) {
    stop("Data appear to be binary (values in {0, 1}) but family='continuous' was specified. Use family='binary' for dichotomous outcomes.")
  }

  family <- match.arg(family)  # switches between binary and continuous

  d_which_fix = NULL
  d_theta_fix = NULL

  Yfake = NULL #no Yfake, but object will exist if no M-matrix

  if (!is.null(M_matrix)) {
      #First check to ensure that the K column headers in the input data Y_in
      #match the elements in the first column of the constraint matrix M_matrix
      #exit if not
      if(length(setdiff(colnames(Y_in), M_matrix[,1]))!=0) {
        stop("Item labels do not match. Please address inconsistency before running again.")
      }
      #Next check to ensure that M has d+1 dimensions if not NULL.
      if (!is.null(M_matrix) && dim(M_matrix)[2]!=(d+1)) {
        stop("The number of latent dimensions does not match the number coded in M_matrix.")
      }

    #Create array of M-matrices
    M <- array(NA, c(d, d, ncol(Y_in)))
    #if only one dimension, force a matrix to be constructed
    if (d==1) {
      for (i in 1:ncol(Y_in)) {
        M[,,i] <- matrix(M_matrix[i,2], nrow = 1, ncol = 1)
      }
    }
    else {
      codings <- 2:(d+1)
      for (i in 1:ncol(Y_in)) {
        M[,,i] <- diag(M_matrix[i,codings])
      }
    }

    #Check dimensions of created objects and exit if incorrect for sampler
    if(dim(Y_in)[2] != dim(M)[3]) {
      stop("Dimensions of Data and Constraint do not match.")
    }
    if(dim(M)[1] != d || dim(M)[2]!=d) {
      stop("M is not dxd; check processing")
    }

    #First, compute synthetic anchor points using IRT-M's built-in function
    #The value of 5 is chosen to be an extreme value, but 5 is arbitrary
    l2 <- pair_gen_anchors(M,5)
    Yfake <- l2[[1]]
    colnames(Yfake) <- names(Y_in)
    theta_fake <- l2[[2]]
    #Second, create a data matrix for the sampler that contains the fake data as well.
    Y_all = as.matrix(rbind(Yfake, Y_in))
    d_which_fix <- 1:nrow(Yfake)
    d_theta_fix <- theta_fake
  }

  #Run constrained_IRT
  #Default to learning factor covariance unless user sets learn_loadings to TRUE.
  #Note: these are mutually exclusive: one can't learn both.
  #Setting both learn_Sigma and learn_Omega to TRUE defaults to learning factor covariance.
  learnS <- TRUE
  learnO <- FALSE
  if (learn_loadings==TRUE) {
    learnS <- FALSE
    learnO <- TRUE
  }

  #Run IRT-M, either binary or continuous
  irt <- M_constrained_irt_family(
    Y_all,
    d = d,
    family = family,
    M = M,
    theta_fix = d_theta_fix,
    which_fix = d_which_fix,
    nburn = nburn,
    nsamp = nsamp,
    thin = thin,
    learn_Sigma = learnS,
    learn_Omega = learnO
  )

  if (!is.null(Yfake)) {
    #Remove anchors before returning results
    anchors_end <- dim(Yfake)[1]
    irt$theta <- irt$theta[-(1:anchors_end), , , drop=FALSE]
  }
  return(irt)
}
