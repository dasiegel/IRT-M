#include <RcppArmadillo.h>
#include <cmath>
#include "tmvrnorm.h"
#include <progress.hpp>
#include <progress_bar.hpp>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo, RcppProgress)]]

using namespace Rcpp;
using namespace arma;

// ---------------------------------------------------------
// Helpers for mixed sampler
// ---------------------------------------------------------

// Update b for categorical items (binary + ordered)
// Same structure as in sample_constrained_irt, but skip continuous items.
void update_b_mixed(vec& b, const int& N, const int& K, const double& sigma_sqr,
                    const mat& theta, const mat& lambda, const mat& Z,
                    const ivec& item_type) {
  for (int k = 0; k < K; k++) {
    if (item_type(k) == 3) continue; // no b for continuous-only items
    double mean = sigma_sqr * sum(theta * lambda.row(k).t() - Z.col(k)) /
      (sigma_sqr * N + 1.0);
    double sd = std::sqrt(float(sigma_sqr / (sigma_sqr * N + 1.0)));
    b[k] = R::rnorm(mean, sd);
  }
  return;
}

// Update Sigma (same as binary sampler)
void update_Sigma_mixed(mat& Sigma, const int& N, const int& nu0,
                        const mat& theta, const mat& S0) {
  Sigma = riwish(nu0 + N, (theta.t() * theta + S0));

  // fix all variances to 1
  for (int j = 0; j < Sigma.n_cols; j++) {
    Sigma.row(j) = Sigma.row(j) / std::sqrt(Sigma(j, j));
    Sigma.col(j) = Sigma.row(j).as_col();
    Sigma(j, j) = 1.0;
  }
  return;
}

// Update Omega (same as binary sampler)
void update_Omega_mixed(mat& Omega, const int& K, const int& nu0,
                        const mat& lambda, const mat& S0) {
  Omega = riwish(nu0 + K, (lambda.t() * lambda + S0));

  // fix all variances to 1
  for (int j = 0; j < Omega.n_cols; j++) {
    Omega.row(j) = Omega.row(j) / std::sqrt(Omega(j, j));
    Omega.col(j) = Omega.row(j).as_col();
    Omega(j, j) = 1.0;
  }
  return;
}

// Update Z for categorical items (binary + ordered)
void update_Z_mixed(mat& Z, const int& N, const int& K,
                    const mat& lambda, const mat& theta,
                    const vec& b, const mat& Y_cat,
                    const ivec& item_type,
                    const List& tau_list) {
  mat Z_old = Z;

  for (int i = 0; i < N; i++) {
    for (int k = 0; k < K; k++) {
      // continuous items do not use Z
      if (item_type(k) == 3) {
        Z(i, k) = 0.0;
        continue;
      }

      vec mu_z = lambda.row(k) * theta.row(i).t() - b(k);

      if (!std::isfinite(Y_cat(i, k))) {
        // missing categorical response: untruncated normal
        Z(i, k) = R::rnorm(mu_z[0], 1.0);
      } else if (item_type(k) == 1) {
        // binary probit
        if (Y_cat(i, k) == 1) {
          Z(i, k) = rtnorm_fast(mu_z[0], 1.0, 0.0, R_PosInf);
        } else {
          Z(i, k) = rtnorm_fast(mu_z[0], 1.0, R_NegInf, 0.0);
        }
      } else if (item_type(k) == 2) {
        // ordered probit
        NumericVector tau_k = tau_list[k];
        tau_k.insert(tau_k.begin(),R_NegInf);
        tau_k.insert(tau_k.end(),R_PosInf);
        double y_n = Y_cat(i, k);      // in [0, 1]
        int C = tau_k.size()-1;      // number of categories

        // map normalized value back to category index 1..C
        int c = 1 + (int)std::round(y_n * (C - 1));
        if (c < 1) c = 1;
        if (c > C) c = C;

        double low  = tau_k[c - 1];
        double high = tau_k[c];
        Z(i,k) = rtnorm_fast(mu_z[0], 1.0, low, high);
      }
    }
  }

  uvec nf = find_nonfinite(Z);
  Z(nf) = Z_old(nf);
  return;
}

// Update theta (vectorized, mixed items)
void update_theta_mixed(
    mat& theta,
    const mat& lambda,
    const vec& b,
    const mat& Z,
    const mat& Sigma,
    const uvec& ind_fix,
    const mat& theta_fix,
    const mat& Y_cont,
    const ivec& item_type,
    const vec& alpha,
    const vec& sigma2
) {
  using namespace arma;

  const int N = theta.n_rows;
  const int d = theta.n_cols;

  mat inv_Sigma = inv_sympd(Sigma);

  // indices by type
  uvec idx_bin  = find(item_type == 1);
  uvec idx_ord  = find(item_type == 2);
  uvec idx_cat  = join_cols(idx_bin, idx_ord);
  uvec idx_cont = find(item_type == 3);

  for (int n = 0; n < N; n++) {

    // anchors
    if (any(ind_fix == (unsigned)n)) {
      uvec pos = find(ind_fix == (unsigned)n);
      theta.row(n) = theta_fix.row(pos(0));
      continue;
    }

    mat Prec = inv_Sigma;
    vec mean = zeros<vec>(d);

    // categorical items
    if (!idx_cat.is_empty()) {
      mat lambda_cat = lambda.rows(idx_cat);

      rowvec Z_row_full = Z.row(n);
      rowvec Z_row = Z_row_full.elem(idx_cat);
      rowvec b_row = b.elem(idx_cat).t();

      uvec finite_idx = find_finite(Z_row);
      if (!finite_idx.is_empty()) {
        mat L = lambda_cat.rows(finite_idx);
        rowvec Zb = Z_row.elem(finite_idx) + b_row.elem(finite_idx);

        Prec += L.t() * L;
        mean += L.t() * Zb.t();
      }
    }

    // continuous items
    if (!idx_cont.is_empty()) {
      rowvec Y_row_full = Y_cont.row(n);
      rowvec Y_row = Y_row_full.elem(idx_cont);

      uvec finite_idx = find_finite(Y_row);
      if (!finite_idx.is_empty()) {
        mat beta_sub = lambda.rows(idx_cont); // in Option 1, lambda is the loading
        mat B = beta_sub.rows(finite_idx);

        vec sigma_sub = sigma2.elem(idx_cont);
        vec w = 1.0 / sigma_sub.elem(finite_idx);

        vec alpha_sub = alpha.elem(idx_cont);
        vec y_adj = Y_row.elem(finite_idx).t() - alpha_sub.elem(finite_idx);

        Prec += B.t() * diagmat(w) * B;
        mean += B.t() * (w % y_adj);
      }
    }

    mat V_theta = inv_sympd(Prec);
    vec mu_theta = V_theta * mean;

    theta.row(n) = rmvnorm(1, mu_theta, V_theta).t();
  }
}

// -------------------------------------------------------------------
// 2. Update lambda (categorical + continuous likelihood)
// -------------------------------------------------------------------
void update_lambda_mixed(
    mat& lambda,
    const mat& theta,
    const mat& lbs,
    const mat& ubs,
    const vec& b,
    const mat& Omega,
    const mat& Z,
    const mat& Y_cont,
    const ivec& item_type,
    const vec& alpha,
    const vec& sigma2
) {
  using namespace arma;

  const int K = lambda.n_rows;
  const int d = lambda.n_cols;

  mat Omega_inv = inv_sympd(Omega);
  mat lambda_old = lambda;

  for (int k = 0; k < K; k++) {

    mat Prec = Omega_inv;
    vec rhs  = zeros<vec>(d);

    // categorical contribution
    if (item_type(k) == 1 || item_type(k) == 2) {
      vec zk = Z.col(k);
      uvec idx = find_finite(zk);
      if (!idx.is_empty()) {
        mat Theta_sub = theta.rows(idx);
        vec z_sub = zk.elem(idx) + b(k);
        Prec += Theta_sub.t() * Theta_sub;
        rhs  += Theta_sub.t() * z_sub;
      }
    }

    // continuous contribution
    if (item_type(k) == 3) {
      vec yk = Y_cont.col(k);
      uvec idx = find_finite(yk);
      if (!idx.is_empty()) {
        mat Theta_sub = theta.rows(idx);
        vec y_sub = yk.elem(idx) - alpha(k);
        double w = 1.0 / sigma2(k);
        Prec += w * (Theta_sub.t() * Theta_sub);
        rhs  += w * (Theta_sub.t() * y_sub);
      }
    }

    mat V_lambda = inv_sympd(Prec);
    vec mu_lambda = V_lambda * rhs;

    lambda.row(k) = rtmvnorm_gibbs(
      1,
      mu_lambda,
      V_lambda,
      lbs.row(k).t(),
      ubs.row(k).t(),
      mu_lambda
    ).t();
  }

  uvec nf = find_nonfinite(lambda);
  lambda(nf) = lambda_old(nf);
  lambda(find(lbs == ubs)).zeros();
}

// -------------------------------------------------------------------
// 3. Update continuous parameters (alpha, sigma2) with lambda in mean
// -------------------------------------------------------------------
void update_continuous_params(
    vec& alpha,
    vec& sigma2,
    const mat& Y_cont,
    const mat& theta,
    const mat& lambda,
    const ivec& item_type
) {
  using namespace arma;

  const int K = Y_cont.n_cols;

  const double alpha_var = 25.0;
  const double a_sigma   = 2.0;
  const double b_sigma   = 2.0;

  double alpha_prec0 = 1.0 / alpha_var;

  for (int k = 0; k < K; k++) {
    if (item_type(k) != 3) continue;

    vec yk = Y_cont.col(k);
    uvec idx = find_finite(yk);
    if (idx.n_elem == 0) continue;

    vec y_sub = yk.elem(idx);
    mat Theta_sub = theta.rows(idx);
    vec lam_k = lambda.row(k).t();

    vec mu_theta = Theta_sub * lam_k;
    vec y_tilde = y_sub - mu_theta;

    double n = static_cast<double>(idx.n_elem);
    double sig2 = sigma2(k);

    double prec_post = alpha_prec0 + n / sig2;
    double var_post  = 1.0 / prec_post;
    double mean_post = var_post * (sum(y_tilde) / sig2);

    alpha(k) = R::rnorm(mean_post, std::sqrt(var_post));

    vec resid = y_tilde - alpha(k);
    double rss = dot(resid, resid);

    double a_post = a_sigma + 0.5 * n;
    double b_post = b_sigma + 0.5 * rss;

    sigma2(k) = 1.0 / R::rgamma(a_post, 1.0 / b_post);
  }
}



// ---------------------------------------------------------
  // Main mixed sampler
// ---------------------------------------------------------

  // [[Rcpp::export]]
List sample_mixed_irt(const arma::mat& Y_cat,   // N x K (binary + ordered; NA allowed)
                      const arma::mat& Y_cont,  // N x K (continuous; NA allowed)
                      const arma::ivec& item_type, // 1=binary, 2=ordered, 3=continuous
                      const int& d,
                      const int& nu0,
                      const arma::mat& S0,
                      const arma::mat& lbs,
                      const arma::mat& ubs,
                      const arma::uvec& ind,       // 1-based indices of fixed theta
                      const arma::mat& theta_fix,  // same rows as ind
                      const int& nburn,
                      const int& nsamp,
                      const int& thin,
                      const bool& learn_Sigma,
                      const bool& learn_Omega,
                      const bool& display_progress,
                      const Rcpp::List& tau_list   // thresholds for ordered items
) {

  int N = Y_cat.n_rows;
  int K = Y_cat.n_cols;
  int S = nburn + nsamp;

  // Initialize parameters
  mat Z(N, K, fill::zeros);
  vec b(K, fill::zeros);
  mat theta(N, d, fill::zeros);
  mat lambda(K, d, fill::zeros);
  mat Sigma(d, d, fill::eye);
  mat Omega(d, d, fill::eye);
  double sigma_sqr = 25.0;

  // Continuous parameters
  vec alpha(K, fill::zeros);
  mat beta(K, d, fill::zeros);
  vec sigma2(K, fill::ones);

  // Fix points in theta (anchors)
  if (ind.n_elem > 0)
    theta.rows(ind - 1) = theta_fix;

  // Storage
  int n_keep = floor(nsamp / thin);
  cube THETA(N, d, n_keep, fill::zeros);
  cube LAMBDA(K, d, n_keep, fill::zeros);
  mat  B(K, n_keep, fill::zeros);
  cube SIGMA(d, d, n_keep, fill::zeros);
  cube OMEGA(d, d, n_keep, fill::zeros);

  mat  ALPHA(K, n_keep, fill::zeros);
  cube BETA(K, d, n_keep, fill::zeros);
  mat  SIGMA_CONT(K, n_keep, fill::zeros);

  int r = 0;

  Progress pbar(S, display_progress);
  for (int s = 0; s < S; s++) {
    if (Progress::check_abort())
      return List::create();
    pbar.increment();

    // 1) Z for categorical items
update_Z_mixed(Z, N, K, lambda, theta, b, Y_cat, item_type, tau_list);

// 2) b for categorical items
update_b_mixed(b, N, K, sigma_sqr, theta, lambda, Z, item_type);

// 3) lambda for categorical items
update_lambda_mixed(lambda, K, theta, lbs, ubs, b, Omega, Z, item_type);

// 4) continuous item parameters
update_continuous_params(alpha, beta, sigma2, Y_cont, theta, item_type);

// 5) theta
update_theta_mixed(theta, N, d, lambda, b, Z, Sigma,
                   ind - 1, theta_fix, Y_cont, item_type,
                   alpha, beta, sigma2);

// 6) Sigma / Omega
if (learn_Sigma) {
  update_Sigma_mixed(Sigma, N, nu0, theta, S0);
} else if (learn_Omega) {
  update_Omega_mixed(Omega, K, nu0, lambda, S0);
}

// 7) Store after burn-in
if ((s >= nburn) && ((s - nburn) % thin == 0)) {
  THETA.slice(r) = theta;
  LAMBDA.slice(r) = lambda;
  B.col(r) = b;
  SIGMA.slice(r) = Sigma;
  OMEGA.slice(r) = Omega;

  ALPHA.col(r) = alpha;
  BETA.slice(r) = beta;
  SIGMA_CONT.col(r) = sigma2;

  r++;
}
  }

  return List::create(
    Named("theta")      = THETA,
    Named("lambda")     = LAMBDA,
    Named("b")          = B,
    Named("Sigma")      = SIGMA,
    Named("Omega")      = OMEGA,
    Named("alpha")      = ALPHA,
    Named("beta")       = BETA,
    Named("sigma_cont") = SIGMA_CONT
  );
}
