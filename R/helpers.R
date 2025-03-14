#' @title Mean Squared Error
#' @description loss function for benchmarks
#' @param ytrue observed values,
#' @param ypred predicted values 
#' @param aggregate logical for whether to take mean of estimate
#' @param root logical for whether to return square root of MSE
#' @return mean squared error
#' @importFrom stats sd
#' @export

mse <- function(ytrue, ypred, aggregate=TRUE, root=FALSE){
  ytrue_sd <- apply(ytrue, 2, sd)
  ypred_sd <- apply(ypred, 2, sd)
  if (aggregate)
    ret <- colMeans((ytrue/ytrue_sd-ypred/ypred_sd)^2)
  else
    ret <- (ytrue/ytrue_sd-ypred/ypred_sd)^2
  if (root)
    ret <- sqrt(ret)

  return(ret)
}

#' @title Geweke Convergence
#' @description Runs Geweke tests to assess MCMC convergence
#' @param THETA Matrix of parameter estimates from IRTM
#' @return Proportion of values that fail the Geweke convergence test (p < 0.05) for each parameter
#' @importFrom coda geweke.diag
#' @importFrom stats pnorm
#' @export

Geweke_convergence <- function(THETA){
  N <- dim(THETA)[1]
  d <- dim(THETA)[2]
  count <- c(0,0,0)
  P <- matrix(NA, N-6, 3)
  for(i in 7:N){
    for(j in 1:3){
      z <- geweke.diag(THETA[i,j,], frac1=0.1, frac2=0.5)
      p <- 2*pnorm(-abs(z$z))
      P[i-6,j] <- p
      if(p < 0.05){
        count[j] = count[j] + 1
      }
    }
  }
  count/N
}

#' @title Standardize Theta
#' @description standardizes theta estimates
#' @param theta estimated object 
#' @param Sigma covariance matrix 
#' @return theta divided by sigma param
#' @export

standardize_theta <- function(theta, Sigma){
  theta_std <- t(t(theta)/sqrt(diag(Sigma)))
  theta_std
}

#' @title Theta Lambda Traceplots
#' @description Creates traceplots for IRT parameter convergence diagnostics
#' @param irt An object containing theta and lambda parameters from an IRTM model
#' @param i Index of the respondent to plot (randomly selected if NULL)
#' @param k Index of the item to plot (randomly selected if NULL)
#' @return Plots of theta, lambda, and their product across MCMC iterations
#' @importFrom graphics par
#' @export

theta_lambda_traceplots <- function(irt, i=NULL, k=NULL){
  theta = irt$theta
  lambda = irt$lambda
  if (is.null(i))
    i = sample(1:nrow(theta), 1)
  if (is.null(k))
    k = sample(1:nrow(lambda), 1)
  n = length(theta[i, 1, ])
  D = ncol(theta)
  par(mfcol=c(D, 3))
  for(d in 1:D){
    plot(1:n, theta[i, d, ], xlab = 'sample', ylab=paste('theta', d))
  }
  for(d in 1:D){
    plot(1:n, lambda[k, d, ], xlab = 'sample', ylab=paste('lambda', d))
  }
  for(d in 1:D){
    plot(1:n, theta[i, d, ] * lambda[k, d, ], xlab = 'sample', ylab=paste('theta * lambda', d))
  }
  par(mfrow=c(1,1))
}
