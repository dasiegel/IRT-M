#' @title irt_m
#' @description This function is a wrapper to enable easier use of the IRT-M model in M_constrained_irt.
#' It takes as input two data frames: a N x K data frame, and a K x (1+d) M-matrix.
#' The first column of the M-matrix should contain item identifiers that match the K column headers in
#' the N x K data frame. If they do not match, the wrapper exits with an error.
#' The wrapper computes anchors, Y_all (merged data and anchors), and a list of diagonal M-Matrices.
#' The second two are used as inputs to M_constrained_irt, which runs the sampler. Also used as input are
#' nburn (Default = 10^3), nsamp (Default = 10^3), thin (Default = 1), and
#' learn_loadings (Default = FALSE). This last one defaults to having the sampler learn factor covariances.
#' If set to true, it will learn loading covariances instead.
#' Finally, the wrapper removes the anchors and returns an irt list.
#' @param Y_in a N x K matrix of responses given by N respondents to K items. Can contain missing values. Column names should match first column in M_matrix.
#' @param d an integer specifying the number of latent dimensions.
#' @param M_matrix a K x (d+1) matrix of theoretical codings used to constrain IRT-M  (default=NULL). First column should match column names in Y_in.
#' @param nburn an integer specifying the number of burn-in MCMC iterations.
#' @param nsamp an integer specifying the number of sampling MCMC iterations.
#' @param thin an integer specifying the number of thinning MCMC samples.
#' @param learn_loadings a Boolean specifying whether a covariance matrix for the latent loadings should be learned, instead of the default covariance matrix for latent dimensions.
#' @return A list containing the following components:
#'   \item{lambda}{An array of dimension (K x d x nsamp/thin) containing posterior samples of item discrimination parameters.}
#'   \item{b}{A matrix of dimension (K x nsamp/thin) containing posterior samples of item difficulty parameters.}
#'   \item{theta}{An array of dimension (N x d x nsamp/thin) containing posterior samples of respondent latent trait values.}
#'   \item{Sigma}{An array of dimension (d x d x nsamp/thin) containing posterior samples of the covariance matrix of latent traits (only if learn_Sigma=TRUE).}
#'   \item{Omega}{An array of dimension (d x d x nsamp/thin) containing posterior samples of the covariance matrix of item loadings (only if learn_Omega=TRUE).}
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

irt_m = function(Y_in, d, M_matrix=NULL,
                 nburn=1000, nsamp=1000, thin=1,
                 learn_loadings=FALSE){

  d_which_fix = NULL
  d_theta_fix = NULL
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
  #Note: these ae mutually exclusive: one can't learn both.
  #Setting both learn_Sigma and learn_Omega to TRUE defaults to learning factor covariance.
  learnS <- TRUE
  learnO <- FALSE
  if (learn_loadings==TRUE) {
    learnS <- FALSE
    learnO <- TRUE
  }

  #Run IRT-M
  irt <- M_constrained_irt(Y_all,
                           d = d,
                           M= M,
                           theta_fix = d_theta_fix,
                           which_fix = d_which_fix,
                           nburn=nburn,
                           nsamp=nsamp,
                           thin=thin,
                           learn_Sigma=learnS, learn_Omega=learnO)

  if (!is.null(Yfake)) {
    #Remove anchors before returning results
    anchors_end <- dim(Yfake)[1]
    irt$theta <- irt$theta[-(1:anchors_end), , , drop=FALSE]
  }
  return(irt)
}
