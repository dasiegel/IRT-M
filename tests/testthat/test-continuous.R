test_that("M_constrained_irt_continuous returns correctly shaped output", {
  skip_on_cran()  # keep CRAN checks fast and safe

  set.seed(123)
  N <- 30
  K <- 5
  d <- 2

  # Simple continuous factor model with d=2
  theta_true  <- mvtnorm::rmvnorm(N, mean = rep(0, d), sigma = diag(d))
  lambda_true <- mvtnorm::rmvnorm(K, mean = rep(0, d), sigma = diag(d))
  b_true      <- rnorm(K)

  mu <- theta_true %*% t(lambda_true) - matrix(b_true, N, K, byrow = TRUE)
  Y  <- mu + matrix(rnorm(N * K), N, K)

  fit <- M_constrained_irt_continuous(
    Y = Y, d = d,
    nburn = 40, nsamp = 40, thin = 10,
    learn_Sigma = TRUE, learn_Omega = FALSE,
    display_progress = FALSE
  )

  # Check shapes
  expect_equal(dim(fit$theta),  c(N, d, 4))  # nsamp/thin = 40/10 = 4
  expect_equal(dim(fit$lambda), c(K, d, 4))
  expect_equal(dim(fit$b),      c(K, 4))

  # Basic sanity: finite draws
  expect_false(any(!is.finite(fit$theta)))
  expect_false(any(!is.finite(fit$lambda)))
  expect_false(any(!is.finite(fit$b)))
})

test_that("M_constrained_irt_family dispatches correctly", {
  skip_on_cran()

  set.seed(123)
  N <- 20; K <- 3; d <- 1

  # Binary data
  theta_bin  <- rnorm(N)
  lambda_bin <- rnorm(K)
  b_bin      <- rnorm(K)

  eta <- outer(theta_bin, lambda_bin) - matrix(b_bin, N, K, byrow = TRUE)
  prob <- pnorm(eta)
  Y_bin <- matrix(rbinom(N * K, 1, prob), N, K)

  fit1 <- M_constrained_irt(Y_bin, d = d,
                            nburn = 20, nsamp = 20, thin = 10,
                            learn_Sigma = FALSE, learn_Omega = FALSE,
                            display_progress = FALSE)

  fit2 <- M_constrained_irt_family(Y_bin, d = d, family = "binary",
                                   nburn = 20, nsamp = 20, thin = 10,
                                   learn_Sigma = FALSE, learn_Omega = FALSE,
                                   display_progress = FALSE)

  # Same structure and dimensions
  expect_equal(dim(fit1$theta), dim(fit2$theta))
  expect_equal(dim(fit1$lambda), dim(fit2$lambda))
  expect_equal(dim(fit1$b), dim(fit2$b))
})
