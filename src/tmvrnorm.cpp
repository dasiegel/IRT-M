#include "tmvrnorm.h"
/*
 * This file contains code relevant to a C++ implementation of a Gibbs sampler (a specific case of Metropolis-Hastings MCMC) to
 * sample from a truncated multivariate Gaussian distribution.
 */

// sub1 returns a matrix x[-i,-i]
arma::mat sub1(arma::mat x, int i) {
  x.shed_col(i);
  x.shed_row(i);
  return x;
}

// sub2 returns a matrix x[a,-b]
arma::mat sub2(arma::mat x, int a, int b){
  x.shed_col(b);
  return(x.row(a));
}

// negSubCol returns a column vector x[-i]
arma::vec negSubCol(arma::vec x, int i){
  x.shed_row(i);
  return(x);
}

// negSubRow returns a row vector x[-i]
arma::rowvec negSubRow(arma::rowvec x, int i){
  x.shed_col(i);
  return(x);
}


/*
 * rtnorm_gibbs returns a sample of size n from the specificed truncated Gaussian distribution.
 * n: integer number of samples to take
 * mu: mean of distribution
 * sigma: standard deviation of distribution
 * a: lower truncation bound
 * b: upper truncation bound
 */
arma::vec rtnorm_gibbs(int n, double mu, double sigma, double a, double b){

  //sample from uniform distribution on unit interval
  arma::vec F = runif(n);

  //Phi(a) and Phi(b)
  double Fa = R::pnorm(a,mu,sigma,1,0);
  double Fb = R::pnorm(b,mu,sigma,1,0);

  arma::vec F_out(n);
  for(int i=0; i < n; i++){
    double p_i = F[i] * (Fb - Fa) + Fa;
    F_out[i] = R::qnorm(p_i,0.0,1.0,1,0);
  }

  arma::vec out(n);
  for(int i=0; i < n; i++){
    out[i] = mu + sigma * F_out[i];
  }

  return(out);
}


double rtnorm_fast(double mu, double sigma, double a, double b){
  double z = r_truncnorm(mu, sigma, a, b);
  if (z == a || z == b){
    z = rtnorm_gibbs(1, mu, sigma, a, b)(0);
  }
  return z;
}



/*
 * rtmvnorm_gibbs returns a sample of size n from the specified truncated multivariate Gaussian distribution
 * n: integer number of samples to take
 * mu: vector mean of distribution
 * sigma: covariance matrix of target distribution
 * lower: vector lower truncation bound
 * upper: vector upper truncation bound
 * init_state: initial starting state of the MCMC chain
 */

arma::mat rtmvnorm_gibbs(int n, arma::vec mu, arma::mat sigma, arma::vec lower, arma::vec upper, arma::vec init_state){

  int d = mu.n_elem; //check dimension of target distribution
  arma::mat trace = arma::zeros(n,d); //trace of MCMC chain

  //draw from U(0,1)
  NumericVector U = runif(n*d);
  int l = 0; //iterator for U

  //calculate conditional standard deviations
  arma::vec sd(d);
  arma::cube P = arma::zeros(1,d-1,d);

  for(int i=0; i<d; i++){
    //partitioning of sigma
    arma::mat Sigma = sub1(sigma,i);
    double sigma_ii = sigma(i,i);
    arma::rowvec Sigma_i = sub2(sigma,i,i);

    P.slice(i) = Sigma_i * Sigma.i();
    double p_i = Rcpp::as<double>(wrap(P.slice(i) * Sigma_i.t()));
    sd(i) = sqrt(sigma_ii - p_i);
  }

  arma::vec x = init_state;

  //run Gibbs sampler for specified chain length (MCMC chain of n samples)
  for(int j=0; j<n; j++){

    //sample all conditional distributions
    for(int i=0; i<d; i++){

      //calculation of conditional expectation and conditional variance
      arma::rowvec slice_i = P.slice(i);
      arma::vec slice_i_times = slice_i * (negSubCol(x,i) - negSubCol(mu,i));
      double slice_i_times_double = Rcpp::as<double>(wrap(slice_i_times));
      double mu_i = mu(i) + slice_i_times_double;

      //transformation
      double Fa = R::pnorm(lower(i),mu_i,sd(i),1,0);
      double Fb = R::pnorm(upper(i),mu_i,sd(i),1,0);
      x(i) = mu_i + sd(i) * R::qnorm(U(l) * (Fb - Fa) + Fa,0.0,1.0,1,0);
      l = l + 1;

    }

    trace.row(j) = x.t();

  }

  return(trace);
}
