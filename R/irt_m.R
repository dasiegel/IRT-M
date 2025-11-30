#' @title irt_m
#' @description
#' A wrapper that simplifies the use of the IRT-M model implemented in
#' `M_constrained_irt_family()`. It checks consistency between the respondent-by-item
#' data and an optional constraint matrix, constructs synthetic anchors when needed,
#' prepares the combined dataset, runs the sampler, removes anchors, and returns
#' posterior samples.
#'
#' @details
#' **Inputs.**
#' - `Y_in` must be an `N × K` matrix or data frame with item identifiers matching
#'   the first column of `M_matrix` (if provided).
#' - `M_matrix` must be a `K × (d+1)` matrix with the first column giving item IDs and
#'   the remaining columns specifying diagonal constraint entries.
#'
#' **Family argument.**
#' The `family` argument selects the observation model or, if given NULL, infers the family from:
#' - `"binary"`
#' - `"continuous"`
#'
#' **Anchors.**
#' When constraints are provided (`M_matrix` not `NULL`), synthetic anchor responses
#' are generated via `pair_gen_anchors()` to fix the latent trait scale. These anchors
#' are included during sampling but removed from the returned posterior samples.
#'
#' **Learning covariance structures.**
#' If `learn_loadings = FALSE` (default), the sampler learns the covariance matrix
#' of latent traits (`Sigma`).
#' If `learn_loadings = TRUE`, the sampler instead learns the covariance matrix
#' of item loadings (`Omega`). These options are mutually exclusive.
#'
#' @param Y_in
#' A numeric `N × K` response matrix or data frame, with rows representing
#' respondents and columns items. Column names must match the
#' first column of `M_matrix` (if supplied).
#'
#' @param d
#' Integer. Number of latent dimensions.
#'
#' @param M_matrix
#' Optional `K × (d+1)` constraint matrix.
#' Column 1: item identifiers matching `colnames(Y_in)`.
#' Columns 2:(d+1): diagonal constraint entries for each dimension.
#' Default: `NULL`.
#'
#' @param family
#' Character string specifying the response family. One of:
#' - `"binary"`
#' - `"continuous"`
#' Default: `"binary"`.
#'
#' @param nburn
#' Number of burn-in MCMC iterations. Default: `1000`.
#'
#' @param nsamp
#' Number of post–burn-in MCMC iterations. Default: `1000`.
#'
#' @param thin
#' Thinning interval for MCMC samples. Default: `1`.
#'
#' @param learn_loadings
#' Logical. If `FALSE`, learn covariance of latent traits (`Sigma`).
#' If `TRUE`, learn covariance of item loadings (`Omega`).
#' These cannot be learned simultaneously.
#'
#' @return
#' A list containing posterior draws:
#' \itemize{
#'   \item{\code{lambda}}{A \((K × d × nsamp/thin)\) array of item discrimination samples.}
#'   \item{\code{b}}{A \((K × nsamp/thin)\) matrix of item difficulty samples.}
#'   \item{\code{theta}}{A \((N × d × nsamp/thin)\) array of respondent latent trait samples (anchors removed).}
#'   \item{\code{Sigma}}{A \((d × d × nsamp/thin)\) array of latent trait covariance matrices
#'       (returned when \code{learn_loadings = FALSE}).}
#'   \item{\code{Omega}}{A \((d × d × nsamp/thin)\) array of item-loading covariance matrices
#'       (returned when \code{learn_loadings = TRUE}).}
#' }
#'
#' @import RcppProgress
#' @importFrom tmvtnorm rtmvnorm
#' @importFrom truncnorm rtruncnorm
#' @importFrom RcppArmadillo armadillo_version
#' @importFrom RcppDist bayeslm
#' @useDynLib IRTM, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @export


irt_m <- function(Y_in, d, M_matrix = NULL,
                  family = NULL,
                  nburn = 1000, nsamp = 1000, thin = 1,
                  learn_loadings = FALSE) {

  ## 1. Coerce and validate Y --------------------------------------------------

  # Coerce to numeric matrix
  Y <- as.matrix(Y_in)
  if (!is.numeric(Y)) {
    stop("Y_in must be numeric or coercible to a numeric matrix.")
  }

  N <- nrow(Y)
  K <- ncol(Y)

  # Column names are required *only* when M_matrix is supplied
  if (!is.null(M_matrix) && is.null(colnames(Y))) {
    stop("When M_matrix is supplied, Y_in must have column names matching M_matrix[,1].")
  }

  # No missing values allowed
  if (anyNA(Y)) {
    stop("IRT-M does not support missing values in Y_in. ",
         "Impute or remove NAs before fitting the model.")
  }

  ## 2. Handle family argument (binary vs continuous) --------------------------

  if (is.null(family)) {
    # Infer: if *all* values are 0/1 treat as binary, else continuous
    if (all(Y %in% c(0, 1))) {
      family <- "binary"
    } else {
      family <- "continuous"
    }
  }
  family <- match.arg(family, choices = c("binary", "continuous"))

  # Consistency checks between data and family
  if (family == "binary" && !all(Y %in% c(0, 1))) {
    stop("Binary IRT-M requires Y_in values in {0, 1}. ",
         "Use family='continuous' for continuous outcomes.")
  }

  if (family == "continuous" && all(Y %in% c(0, 1))) {
    stop("Data appear to be binary (values in {0, 1}) but family='continuous' was specified. ",
         "Use family='binary' for dichotomous outcomes.")
  }

  ## 3. Defaults for constraints / anchors -------------------------------------

  d_which_fix <- NULL
  d_theta_fix <- NULL
  Yfake       <- NULL
  M_array     <- NULL  # 3D constraint array, if used

  ## 4. If M_matrix is supplied, validate and construct M ----------------------

  if (!is.null(M_matrix)) {

    # Coerce to data.frame to avoid factor weirdness
    M_df <- as.data.frame(M_matrix, stringsAsFactors = FALSE)

    # Check dimensions: first col is item ID, next d cols are constraints
    if (ncol(M_df) != (d + 1L)) {
      stop("M_matrix must have d+1 columns (item ID + d constraint columns).")
    }

    item_ids <- as.character(M_df[[1]])

    # Ensure same set of item IDs as Y columns
    if (!setequal(item_ids, colnames(Y))) {
      stop("Item labels do not match between Y_in and M_matrix[,1]. ",
           "They must contain the same set of item identifiers.")
    }

    # Reorder M_matrix rows to align with colnames(Y)
    ord <- match(colnames(Y), item_ids)
    if (anyNA(ord)) {
      stop("Some column names of Y_in are not found in M_matrix[,1]. ",
           "Please check item identifiers.")
    }
    M_df <- M_df[ord, , drop = FALSE]

    # Build 3D M array: d x d x K
    M_array <- array(NA_real_, dim = c(d, d, K))
    if (d == 1L) {
      # Single-dimension: each item has a 1x1 "matrix"
      M_array[1, 1, ] <- as.numeric(M_df[[2]])
    } else {
      codings <- 2:(d + 1L)
      for (j in seq_len(K)) {
        M_array[, , j] <- diag(as.numeric(M_df[j, codings]))
      }
    }

    # Sanity check dimensions
    if (dim(M_array)[3] != K) {
      stop("Internal error: constructed M array does not match number of items.")
    }

    ## 4a. Anchors: only for binary family ------------------------------------

    if (family == "binary") {
      # Generate synthetic anchor responses + fixed thetas
      # The '5' here is the usual extreme value used by IRT-M; arbitrary but conventional
      l2         <- pair_gen_anchors(M_array, 5)
      Yfake      <- l2[[1]]
      theta_fake <- l2[[2]]

      # Ensure Yfake has same number of items and named columns
      if (ncol(Yfake) != K) {
        stop("pair_gen_anchors() returned anchors with incorrect number of items.")
      }
      colnames(Yfake) <- colnames(Y)

      # Stack anchors above real data for the sampler
      Y_all       <- rbind(Yfake, Y)
      d_which_fix <- seq_len(nrow(Yfake))
      d_theta_fix <- theta_fake

    } else {
      # Continuous family: NO anchors
      Y_all       <- Y
      d_which_fix <- NULL
      d_theta_fix <- NULL
    }

  } else {
    # No constraints: no anchors, just pass observed data
    Y_all   <- Y
    M_array <- NULL
  }

  # Final sanity check before passing to C++ sampler
  if (anyNA(Y_all)) {
    stop("Internal error: Y_all contains NA values after processing. ",
         "This should not happen. Please report this bug with a reproducible example.")
  }

  ## 5. Decide what covariance structure to learn

  # Default: learn Sigma (latent trait covariance).
  # If learn_loadings=TRUE, learn Omega instead.
  learn_Sigma <- !learn_loadings
  learn_Omega <-  learn_loadings

  ## 6. Sampler via family-dispatch wrapper --------------------------

  irt <- M_constrained_irt_family(
    Y_all,
    d           = d,
    family      = family,
    M           = M_array,
    theta_fix   = d_theta_fix,
    which_fix   = d_which_fix,
    nburn       = nburn,
    nsamp       = nsamp,
    thin        = thin,
    learn_Sigma = learn_Sigma,
    learn_Omega = learn_Omega
  )

  ## 7. Remove anchors from theta before returning -----------------------------

  if (!is.null(Yfake)) {
    n_anchor <- nrow(Yfake)
    irt$theta <- irt$theta[-seq_len(n_anchor), , , drop = FALSE]
  }

  return(irt)
}
