library(testthat)
library(IRTM)


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

# tests/testthat/test-continuous.R
test_that("continuous IRT-M runs without error", {
  set.seed(123)
  Y <- matrix(rnorm(50*10), 50, 10)
  M_mat <- cbind(item = paste0("V", 1:10),
                 dim1 = rep(c(1, -1), 5))
  colnames(Y) <- paste0("V", 1:10)

  expect_no_error(
    irt_m(Y, d = 1, M_matrix = M_mat, family = "continuous",
          nburn = 10, nsamp = 10, thin = 1)
  )
})



test_that("continuous IRT-M with M-matrix generates and removes anchors", {
  set.seed(456)
  N <- 30
  K <- 4
  d <- 2

  Y <- matrix(rnorm(N*K), N, K)
  colnames(Y) <- paste0("V", 1:K)

  M_mat <- data.frame(
    item = paste0("V", 1:K),
    dim1 = c(1, -1, 1, -1),
    dim2 = c(1, 1, -1, -1)
  )

  fit <- irt_m(Y, d = d, M_matrix = M_mat, family = "continuous",
               nburn = 20, nsamp = 20, thin = 1)

  # Check dimensions (anchors should be removed)
  expect_equal(dim(fit$theta), c(N, d, 20))
  expect_equal(dim(fit$lambda), c(K, d, 20))

  # Check no NAs
  expect_false(anyNA(fit$theta))
  expect_false(anyNA(fit$lambda))
  expect_false(anyNA(fit$b))
})

test_that("continuous IRT-M handles heterogeneous scales", {
  set.seed(789)
  N <- 25

  Y <- cbind(
    large_scale = rnorm(N, mean=1e6, sd=2e5),
    small_scale = rnorm(N, mean=0.5, sd=0.2)
  )

  M_mat <- data.frame(
    item = c("large_scale", "small_scale"),
    dim1 = c(1, 1)
  )

  expect_no_error(
    fit <- irt_m(Y, d = 1, M_matrix = M_mat, family = "continuous",
                 nburn = 10, nsamp = 10)
  )

  expect_equal(nrow(fit$theta), N)
})

test_that("continuous auto-detection works", {
  set.seed(101)
  Y <- matrix(rnorm(20*3), 20, 3)
  colnames(Y) <- paste0("X", 1:3)

  M_mat <- data.frame(
    item = paste0("X", 1:3),
    dim1 = c(1, -1, 1)
  )

  # Should auto-detect continuous
  fit <- irt_m(Y, d = 1, M_matrix = M_mat, nburn = 10, nsamp = 10)

  expect_equal(dim(fit$theta)[1], 20)
})

test_that("continuous rejects binary data with error", {
  Y <- matrix(c(0,1,1,0,1,0), 3, 2)
  colnames(Y) <- c("A", "B")

  M_mat <- data.frame(item = c("A", "B"), dim1 = c(1, 1))

  expect_error(
    irt_m(Y, d = 1, M_matrix = M_mat, family = "continuous"),
    "binary.*continuous"
  )
})

test_that("continuous without M-matrix works (no anchors)", {
  set.seed(202)
  Y <- matrix(rnorm(25*4), 25, 4)
  colnames(Y) <- paste0("item", 1:4)

  fit <- irt_m(Y, d = 2, M_matrix = NULL, family = "continuous",
               nburn = 10, nsamp = 10)

  expect_equal(dim(fit$theta)[1], 25)
  expect_false(anyNA(fit$theta))
})

test_that("anchor_quantiles parameter works", {
  set.seed(303)
  Y <- matrix(rnorm(20*3), 20, 3)
  colnames(Y) <- paste0("V", 1:3)

  M_mat <- data.frame(
    item = paste0("V", 1:3),
    dim1 = c(1, -1, 1)
  )

  # Should accept custom quantiles
  expect_no_error(
    fit <- irt_m(Y, d = 1, M_matrix = M_mat, family = "continuous",
                 nburn = 10, nsamp = 10,
                 anchor_quantiles = c(0.01, 0.99))
  )
})

test_that("binary family still works correctly", {
  set.seed(404)
  Y <- matrix(rbinom(30*5, 1, 0.5), 30, 5)
  colnames(Y) <- paste0("Q", 1:5)

  M_mat <- data.frame(
    item = paste0("Q", 1:5),
    dim1 = rep(c(1, -1), length.out=5)
  )

  fit <- irt_m(Y, d = 1, M_matrix = M_mat, family = "binary",
               nburn = 10, nsamp = 10)

  expect_equal(dim(fit$theta)[1], 30)
  expect_false(anyNA(fit$theta))
})
