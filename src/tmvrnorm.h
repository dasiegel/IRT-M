#ifndef TMVRNORM
#define TMVRNORM

#include <RcppArmadillo.h>
#include <RcppDist.h>

// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
using namespace Rcpp;

arma::vec rtnorm_gibbs(int n, double mu, double sigma, double a, double b);
arma::mat rtmvnorm_gibbs(int n, arma::vec mu, arma::mat sigma, arma::vec lower, arma::vec upper, arma::vec init_state);
double rtnorm_fast(double mu, double sigma, double a, double b);

#endif
  