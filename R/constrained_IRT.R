#' @title M_constrained_irt
#' @description This function allows you to run the IRT model.
#' @param Y a N x K matrix of responses given by N respondents to K items. Can contain missing values.
#' @param d an integer specifying the number of latent dimensions.
#' @param M a list of K d x d matrices (default=NULL).
#' @param theta_fix a matrix with d columns containing the values of the latent dimensions for respondents that have pre-specified latent factors.
#' @param which_fix a vector containing the indices of the respondents for which latent factors have been fixed.
#' @param nburn an integer specifying the number of burn-in MCMC iterations.
#' @param nsamp an integer specifying the number of sampling MCMC iterations.
#' @param thin an integer specifying the number of thinning MCMC samples.
#' @param learn_Sigma a Boolean specifying whether a covariance matrix for the latent factors should be learned.
#' @param learn_Omega a Boolean specifying whether a covariance matrix for the latent loadings should be learned.
#' @param hyperparameters a list of hyperparameters for the model.
#' @param display_progress a Boolean specifying whether a progress bar should be displayed.
#' @return A list containing the following components:
#'   \item{lambda}{An array of dimension (K x d x nsamp/thin) containing posterior samples of item discrimination parameters.}
#'   \item{b}{A matrix of dimension (K x nsamp/thin) containing posterior samples of item difficulty parameters.}
#'   \item{theta}{An array of dimension (N x d x nsamp/thin) containing posterior samples of respondent latent trait values.}
#'   \item{Sigma}{An array of dimension (d x d x nsamp/thin) containing posterior samples of the covariance matrix of latent traits (only if learn_Sigma=TRUE).}
#'   \item{Omega}{An array of dimension (d x d x nsamp/thin) containing posterior samples of the covariance matrix of item loadings (only if learn_Omega=TRUE).}
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom truncnorm rtruncnorm
#' @importFrom RcppArmadillo armadillo_version
#' @importFrom RcppDist bayeslm
#' @export

## usethis namespace: start
#' @useDynLib IRTM, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end

M_constrained_irt = function(Y, d, M=NULL, theta_fix=NULL, which_fix=NULL,
                             nburn=1000, nsamp=1000, thin=10,
                             learn_Sigma=TRUE, learn_Omega=FALSE,
                             hyperparameters = list(),
                             display_progress=TRUE){

  N = nrow(Y)
  K = ncol(Y)
  S = nsamp + nburn

  if(is.null(hyperparameters[['S0']]))
    S0 = diag(d)
  else
    S0 = hyperparameters[['S0']]

  if(is.null(hyperparameters[['Omega']]))
    Omega = diag(d) * 25 # same as pscl default
  else
    Omega = hyperparameters[['O0']]

  if(is.null(hyperparameters[['nu0']]))
    nu0 = d
  else
    nu0 = hyperparameters[['nu0']]

  if(is.null(hyperparameters[['mu']]))
    mu = rep(0, d)
  else
    mu = hyperparameters[['mu']]

  if(is.null(M)){
    M = array(0, c(d, d, K))
    for(k in 1:K)
      M[,,k] = diag(d) * 2
  }

  lbs = matrix(NA, K, d)
  ubs = matrix(NA, K, d)
  for (k in 1:K){
    for (d in 1:d){
      if(is.na(M[d,d,k])){
        lbs[k, d] = -Inf
        ubs[k, d] = Inf
      }else if(M[d,d,k] == 0){
        lbs[k, d] = 0
        ubs[k, d] = 0
      }else if (M[d,d,k] == 1){
        lbs[k, d] = 0
        ubs[k, d] = Inf
      }else if (M[d,d,k] == -1){
        lbs[k, d] = -Inf
        ubs[k, d] = 0
      }else{
        lbs[k, d] = -Inf
        ubs[k, d] = Inf
      }
    }
  }

  ## initialize
  if(is.null(theta_fix)){
    ind = numeric(0)
    theta_fix = numeric(0)
  }else{
    if(is.null(which_fix))
      ind = 1:length(theta_fix)
    else
      ind = which_fix
  }
  fixed_vals = matrix(unlist(theta_fix), length(theta_fix), d, byrow=T)

  if(display_progress)
    message('Sampling...')
  res = sample_constrained_irt(Y, d, nu0, S0, lbs, ubs, ind, fixed_vals,
                               nburn, nsamp, thin, learn_Sigma, learn_Omega, display_progress)

  return(res)
}

#' Constrained IRT model for continuous outcomes
#'
#' This fits the same constrained multidimensional IRT model as
#' \code{M_constrained_irt()}, but assumes the observed responses
#' \code{Y} are continuous and directly follow a Gaussian measurement
#' model with mean \eqn{\lambda_k^\top \theta_i - b_k}.
#'
#' @inheritParams M_constrained_irt
#' @return A list with posterior samples for \code{lambda}, \code{b},
#'   \code{theta}, and optionally \code{Sigma} and \code{Omega}, in the
#'   same format as \code{M_constrained_irt()}.
#' @export
M_constrained_irt_continuous <- function(Y, d, M = NULL, theta_fix = NULL, which_fix = NULL,
                                         nburn = 1000, nsamp = 1000, thin = 10,
                                         learn_Sigma = TRUE, learn_Omega = FALSE,
                                         hyperparameters = list(),
                                         display_progress = TRUE){

  N <- nrow(Y)
  K <- ncol(Y)

  if (is.null(hyperparameters[["S0"]]))
    S0 <- diag(d)
  else
    S0 <- hyperparameters[["S0"]]

  if (is.null(hyperparameters[["Omega"]]))
    Omega <- diag(d) * 25
  else
    Omega <- hyperparameters[["O0"]]

  if (is.null(hyperparameters[["nu0"]]))
    nu0 <- d
  else
    nu0 <- hyperparameters[["nu0"]]

  if (is.null(hyperparameters[["mu"]]))
    mu <- rep(0, d)
  else
    mu <- hyperparameters[["mu"]]

  if(is.null(M)){
    M <- array(0, c(d, d, K))
    for(k in 1:K)
      M[,,k] <- diag(d) * 2 # default behavior is unconstrained
  }

  lbs <- matrix(NA, K, d)
  ubs <- matrix(NA, K, d)
  for (k in 1:K) {
    for (j in 1:d) {
      if (is.na(M[j, j, k])) {
        lbs[k, j] <- -Inf; ubs[k, j] <- Inf
      } else if (M[j, j, k] == 0) {
        lbs[k, j] <- 0;    ubs[k, j] <- 0
      } else if (M[j, j, k] == 1) {
        lbs[k, j] <- 0;    ubs[k, j] <- Inf
      } else if (M[j, j, k] == -1) {
        lbs[k, j] <- -Inf; ubs[k, j] <- 0
      } else {
        lbs[k, j] <- -Inf; ubs[k, j] <- Inf
      }
    }
  }

  # Fix points in theta (same logic as binary version)
  if (is.null(theta_fix)) {
    ind <- numeric(0)
    theta_fix_mat <- matrix(0, 0, d)
  } else {
    if (is.null(which_fix))
      ind <- seq_along(theta_fix)
    else
      ind <- which_fix
    theta_fix_mat <- matrix(unlist(theta_fix), length(theta_fix), d, byrow = TRUE)
  }

  if (display_progress)
    message("Sampling continuous constrained IRT model...")

  if(anyNA(Y)) {
    stop("Y has missing values, which IRT-M does not yet support. Impute or drop NAs to continue.")
  }
  res <- sample_constrained_irt_continuous(Y, d, nu0, S0, lbs, ubs,
                                           ind, theta_fix_mat,
                                           nburn, nsamp, thin,
                                           learn_Sigma, learn_Omega,
                                           display_progress)

  return(res)
}

#' Wrapper for IRT-M binary or continuous
#'
#' Convenience wrapper that dispatches to either
#'
#' \code{M_constrained_irt()} (binary probit) or
#' \code{M_constrained_irt_continuous()} (Gaussian) depending on
#' \code{family}.
#'
#' @param family Character string specifying the outcome type:
#'   \code{"binary"} or \code{"continuous"}.
#' @inheritParams M_constrained_irt
#'
#' @return A list with posterior samples for \code{lambda}, \code{b},
#'   \code{theta}, and optionally \code{Sigma} and \code{Omega}, in the
#'   same format as \code{M_constrained_irt()}.
#'
#' @examples
#' \dontrun{
#'   # Binary outcomes
#'   fit_bin <- M_constrained_irt_family(Y_bin, d = 2, family = "binary")
#'
#'   # Continuous outcomes
#'   fit_cont <- M_constrained_irt_family(Y_cont, d = 2, family = "continuous")
#' }
#'
#' @export
M_constrained_irt_family <- function(Y, d,
                                     family = c("binary", "continuous"),
                                     M = NULL,
                                     theta_fix = NULL,
                                     which_fix = NULL,
                                     nburn = 1000,
                                     nsamp = 1000,
                                     thin = 10,
                                     learn_Sigma = TRUE,
                                     learn_Omega = FALSE,
                                     hyperparameters = list(),
                                     display_progress = TRUE){

  family <- match.arg(family)

  if (family == "binary") {
    M_constrained_irt(Y = Y, d = d, M = M,
                      theta_fix = theta_fix, which_fix = which_fix,
                      nburn = nburn, nsamp = nsamp, thin = thin,
                      learn_Sigma = learn_Sigma, learn_Omega = learn_Omega,
                      hyperparameters = hyperparameters,
                      display_progress = display_progress)
  } else {
    M_constrained_irt_continuous(Y = Y, d = d,
                                 M = M,
                                 theta_fix = theta_fix,
                                 which_fix = which_fix,
                                 nburn = nburn,
                                 nsamp = nsamp, thin = thin,
                                 learn_Sigma = learn_Sigma, learn_Omega = learn_Omega,
                                 hyperparameters = hyperparameters,
                                 display_progress = display_progress)
  }
}


