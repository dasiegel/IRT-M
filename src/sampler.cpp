#include <RcppArmadillo.h>
#include <cmath>
#include "tmvrnorm.h"
#include <progress.hpp>
#include <progress_bar.hpp>


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo, RcppProgress)]]
using namespace Rcpp;
using namespace arma;

void update_b(vec& b, const int& N, const int& K, const double& sigma_sqr,
             const mat& theta, const mat& lambda, const mat& Z){
  for (int k = 0; k < K; k++) {
    double mean = sigma_sqr * sum(theta * lambda.row(k).t() -
                                  Z.col(k)) / (sigma_sqr*N+1);
    double variance = std::sqrt(float(sigma_sqr/(sigma_sqr*N+1)));
    b[k] = R::rnorm(mean, variance);
  }
  return;
}

void update_lambda(mat& lambda, const int& K,
                  const mat& theta, const mat& lbs, const mat& ubs,
                  const vec& b, const mat& Omega, const mat& Z){
  mat V_lambda  = inv_sympd( theta.t() * theta + inv(Omega));
  mat lambda_old = lambda;
  for(int k = 0; k < K; k++){
    vec mu_lambda = V_lambda *  theta.t() * (b(k) + Z.col(k));

    lambda.row(k) = rtmvnorm_gibbs(1, mu_lambda.as_col(),
               V_lambda, lbs.row(k).as_col(), ubs.row(k).as_col(),
                                   mu_lambda.as_col()).as_row();
  }
  uvec nf = find_nonfinite(lambda);
  lambda(nf) = lambda_old(nf);
  lambda(find(lbs==ubs)).zeros();
  return;
}


// mat update_theta(const int& N, const mat& lambda_star, const vec& b,
//                  const mat& Z, const mat& Sigma, const vec& ind,
//                  const mat& theta_start, const int& d){
//   mat theta(N,d);
//   mat inv_Sigma = inv_sympd(Sigma);
//   for(int n = 0; n < N; n++){
//     mat V_theta = inv_sympd(lambda_star.t() * lambda_star + inv_Sigma);
//     auto it = std::find (std::begin(ind), std::end(ind), n+1);
//     if (it == std::end(ind)) {
//       vec mu_theta = V_theta * lambda_star.t() * (b + trans(Z.row(n)));
//       theta.row(n) = rmvnorm(1, mu_theta, V_theta);
//     } else {
//       theta.row(n) = theta_start.row(n);
//     }
//   }
//   return theta;
// }

void update_theta(mat& theta, const int& N, const int& d, const mat& lambda_star, const vec& b,
                 const mat& Z, const mat& Sigma, const uvec& ind){

  mat V_theta = inv_sympd(lambda_star.t() * lambda_star + inv_sympd(Sigma));
  for(int n = 0; n < N; n++){
    if (any(ind==n))
      continue;
    vec mu_theta = V_theta * lambda_star.t() * (b + trans(Z.row(n)));

    theta.row(n) = rmvnorm(1, mu_theta, V_theta);
    }
  return;
}

void update_Z(mat& Z, const int& N, const int& K, const mat& lambda, const mat& theta,
             const vec& b, const mat& Y){
  mat Z_old = Z;
  for (int i = 0; i < N; i++) {
    for (int k = 0; k < K; k++) {
      vec mu_z = lambda.row(k) * theta.row(i).t() - b(k);
      if(Y(i,k) == 1){
        Z(i,k) = rtnorm_fast(mu_z[0], 1, 0, R_PosInf);
      }
      else if(Y(i,k) == 0){
        Z(i,k) = rtnorm_fast(mu_z[0], 1, R_NegInf, 0);
      }
      else{
        Z(i,k) = R::rnorm(mu_z[0], 1);
      }
    }
  }
  uvec nf = find_nonfinite(Z);
  Z(nf) = Z_old(nf);
  return;
}

void update_Sigma(mat& Sigma, const int& N, const int& nu0, const mat& theta, const mat& S0){
  Sigma = riwish(nu0 + N, (theta.t() * theta + S0));

  // let's try fixing all variances
  for (int j = 0; j < Sigma.n_cols; j++){
    Sigma.row(j) = Sigma.row(j)/sqrt(Sigma(j,j));
    Sigma.col(j) = Sigma.row(j).as_col();
    Sigma(j, j) = 1.0;
  }

  return;
}

void update_Omega(mat& Omega, const int& K, const int& nu0, const mat& lambda, const mat& S0){
  Omega = riwish(nu0 + K, (lambda.t() * lambda + S0));

  // let's try fixing all variances
  for (int j = 0; j < Omega.n_cols; j++){
    Omega.row(j) = Omega.row(j)/sqrt(Omega(j,j));
    Omega.col(j) = Omega.row(j).as_col();
    Omega(j, j) = 1.0;
  }

  return;
}

// [[Rcpp::export]]
List sample_constrained_irt(const arma::mat& Y, const int& d,
                            const int& nu0, const arma::mat& S0,
                            const arma::mat& lbs, const arma::mat& ubs,
                            const arma::uvec& ind, const arma::mat& theta_fix,
                            const int& nburn, const int& nsamp, const int& thin,
                            const bool& learn_Sigma, const bool& learn_Omega,
                            const bool& display_progress){

  /* Initialize useful indicators*/
  int N = Y.n_rows;
  int K = Y.n_cols;
  int S = nburn + nsamp;

  /* Initialize parameters to be updated*/
  mat Z(N, K, fill::zeros);
  vec b(K, fill::zeros);
  mat theta(N, d, fill::zeros);
  mat lambda(K, d, fill::zeros);
  mat lambda_old = lambda;
  mat Sigma(d, d, fill::eye);
  mat Omega(d, d, fill::eye);
  double sigma_sqr = 25;

  /* Initialize storage for draws*/
  cube THETA(N, d, floor(nsamp/thin), fill::zeros);
  cube LAMBDA(K, d, floor(nsamp/thin), fill::zeros);
  mat B(K, floor(nsamp/thin), fill::zeros);
  cube SIGMA(d, d, floor(nsamp/thin), fill::zeros);
  cube OMEGA(d, d, floor(nsamp/thin), fill::zeros);
  int r = 0; // keeps track of storage iteration

  /* Fix points in theta*/
  if(ind.n_elem > 0)
    theta.rows(ind-1) = theta_fix;

  /* Run sampling iterations */
  Progress pbar(S, display_progress);
  for(int s=0; s < S; s++){
    if (Progress::check_abort() )
      return List::create();
    pbar.increment();

    update_Z(Z, N, K, lambda, theta, b, Y);
    //Rcout<<"Past z"<<endl;
    //if(Z.has_inf() || Z.has_nan())
    //  Rcout<<"inf or nan in Z at iteration "<<s<<endl;

    update_b(b, N, K, sigma_sqr, theta, lambda, Z);
    //Rcout<<"Past b"<<endl;
    //if(b.has_inf() || b.has_nan())
    //  Rcout<<"inf or nan in b at iteration "<<s<<endl;

    update_lambda(lambda, K, theta, lbs, ubs, b, Omega, Z);
    //Rcout<<"Past lambda"<<endl;
    // if(lambda.has_inf() || lambda.has_nan())
    //   Rcout<<"inf or nan in lambda at iteration "<<s<<endl;

    update_theta(theta, N, d, lambda, b, Z, Sigma, ind-1);
    //Rcout<<"Past theta"<<endl;
    // if(theta.has_inf() || theta.has_nan())
    //   Rcout<<"inf or nan in theta at iteration "<<s<<endl;

    if(learn_Sigma==true){
    update_Sigma(Sigma, N, nu0, theta, S0);
      // Rcout<<"Past Sigma"<<endl;
      // if(Sigma.has_inf() || Sigma.has_nan())
      //   Rcout<<"inf or nan in Sigma at iteration "<<s<<endl;
    }else if (learn_Omega==true){
      update_Omega(Omega, K, nu0, lambda, S0);
    }

    /* Store after burn in */
    if((s >= nburn) && ((s - nburn) % thin == 0)) {
      THETA.slice(r) = theta;
      LAMBDA.slice(r) = lambda;
      B.col(r) = b;
      SIGMA.slice(r)= Sigma;
      OMEGA.slice(r)= Omega;
      r++;
    }
  }

  return List::create(Named("theta")=THETA, Named("lambda")=LAMBDA,
                      Named("b")=B,  Named("Sigma")=SIGMA, Named("Omega")=OMEGA);

}

