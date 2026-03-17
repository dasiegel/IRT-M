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
#'   \item{hyperparameters}{A list of the four hyperparameters of the model: S0, O0, nu0, mu)}
#'   \item{constraints}{A list of lower and upper bounds for each item-dimension pair}
#' @import RcppProgress
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

  if(is.null(hyperparameters[['O0']]))
    O0 = diag(d) * 25 # same as pscl default
  else
    O0 = hyperparameters[['O0']]

  if(is.null(hyperparameters[['nu0']]))
    nu0 = d
  else
    nu0 = hyperparameters[['nu0']]

  if(is.null(hyperparameters[['mu']]))
    mu = rep(0, d)
  else
    mu = hyperparameters[['mu']]

  #This is here in case one wants to pass a NULL M directly to M_constrained_irt rather than going through irt_m.
  if(is.null(M)){
    M = array(NA, c(d, d, K))
    for(k in 1:K)
      M[,,k] = diag(d)*2
  }

  lbs = matrix(NA_real_, K, d)
  ubs = matrix(NA_real_, K, d)
  for (k in 1:K){
    for (j in 1:d){
      val <- M[j,j,k]
      if(is.na(val)){
        lbs[k, j] = -Inf
        ubs[k, j] = Inf
      }else if(val == 0){
        lbs[k, j] = 0
        ubs[k, j] = 0
      }else if (val == 1){
        lbs[k, j] = 0
        ubs[k, j] = Inf
      }else if (val == -1){
        lbs[k, j] = -Inf
        ubs[k, j] = 0
      }else{
        lbs[k, j] = -Inf
        ubs[k, j] = Inf
      }
    }
  }

  ## initialize
  if (is.null(theta_fix)) {
    ind <- integer(0)
    fixed_vals <- matrix(numeric(0), 0, d)
  } else {
    theta_fix <- as.matrix(theta_fix)
    if (ncol(theta_fix) != d) stop("theta_fix must have d columns.")
    ind <- if (is.null(which_fix)) seq_len(nrow(theta_fix)) else which_fix
    fixed_vals <- theta_fix
  }

  #Corrected [[check]]
  #   if(is.null(theta_fix)){
  #   ind = numeric(0)
  #   theta_fix = numeric(0)
  # }else{
  #   if(is.null(which_fix))
  #     ind = 1:length(theta_fix)
  #   else
  #     ind = which_fix
  # }
  # fixed_vals = matrix(unlist(theta_fix), length(theta_fix), d, byrow=T)

  if(display_progress)
    message('Sampling...')
  res = sample_constrained_irt(Y, d, nu0, S0, lbs, ubs, ind, fixed_vals,
                               nburn, nsamp, thin, learn_Sigma, learn_Omega, display_progress)

  res$hyperparameters <- list(S0=S0, O0=O0, nu0=nu0, mu=mu)
  res$constraints <- list(lbs=lbs, ubs=ubs)
  return(res)
}


