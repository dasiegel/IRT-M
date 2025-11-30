library(testthat)
library(IRTM)

## Test 1: Continuous data

test_that("continuous IRT-M runs and returns correct shapes", {
  set.seed(112925)
  N <- 25
  K <- 4
  d <- 2

  Y <- matrix(rnorm(N*K), N, K)
  colnames(Y) <- paste0("V", 1:K)

  # Valid M matrix: K rows, (d+1) columns
  M_mat <- data.frame(
    item = paste0("V", 1:K),
    dim1 = c(1, -1, 1, -1),
    dim2 = c(1, 1, -1, -1),
    stringsAsFactors = FALSE
  )

  fit <- irt_m(Y, d = d, M_matrix = M_mat,
               family = "continuous",
               nburn = 20, nsamp = 20, thin = 5)

  expect_type(fit, "list")
  expect_equal(dim(fit$theta),  c(N, d, 4))
  expect_equal(dim(fit$lambda), c(K, d, 4))
  expect_equal(dim(fit$b),      c(K, 4))
})

## Test 2: irtm works with binary data:

test_that("binary IRT-M runs with M_matrix", {
  set.seed(6889)
  N <- 30
  K <- 5
  d <- 1

  # Generate true binary data
  theta <- rnorm(N)
  lambda <- rnorm(K)
  b <- rnorm(K)
  prob <- pnorm(outer(theta, lambda) - b)
  Y <- matrix(rbinom(N*K, 1, prob), N, K)
  colnames(Y) <- paste0("Q", 1:K)

  M_mat <- data.frame(
    item = paste0("Q", 1:K),
    dim1 = rep(1, K)
  )

  fit <- irt_m(Y, d = d, M_matrix = M_mat,
               family = "binary",
               nburn = 10, nsamp = 10, thin = 1)

  expect_equal(dim(fit$theta), c(N, 1, 10))
  expect_equal(dim(fit$lambda), c(K, 1, 10))
})

## Test 3: catch "binary" model with non-binary data:

test_that("binary family rejects non-binary data", {
  Y <- matrix(rnorm(20), 5, 4)
  colnames(Y) <- paste0("V", 1:4)

  expect_error(
    irt_m(Y, d = 1, family = "binary"),
    "requires Y_in values in \\{0, 1\\}"
  )
})

## Test 4: Auto-detects continuous data

test_that("family inference works for continuous data", {
  Y <- matrix(rnorm(20), 5, 4)
  colnames(Y) <- paste0("V", 1:4)

  expect_no_error(
    irt_m(Y, d = 1, nburn = 5, nsamp = 5)
  )
})

## Rejects data with NAs:

test_that("missing data is rejected", {
  Y <- matrix(c(1, 0, NA, 1), 2, 2)

  expect_error(
    irt_m(Y, d = 1),
    "does not support missing values"
  )
})
