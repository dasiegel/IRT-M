#' @title irt_m_mixed
#' @description This function is a wrapper to enable easier use of the IRT-M model for mixed data.
#' It takes as input two data frames: a N x K data frame, and a K x (1+d) M-matrix.
#' The first column of the M-matrix should contain item identifiers that match the K column headers in
#' the N x K data frame. If they do not match, the wrapper exits with an error.
#' The wrapper computes anchors, Y_all (merged data and anchors), and a list of diagonal M-Matrices as constraints.
#' It additionally takes as input a vector of data types for each item (1=binary, 2=ordered polytomous, 3=continuous), as well as a list of thresholds
#' for all ordered items. Additional inputs are nburn (Default = 10^3), nsamp (Default = 10^3), thin (Default = 1), and
#' learn_loadings (Default = FALSE). This last one defaults to having the sampler learn factor covariances.
#' If set to true, it will learn loading covariances instead.
#' Finally, the wrapper removes the anchors and returns an irt list.
#' @param Y_in a N x K matrix of responses given by N respondents to K items. Can contain missing values. Column names should match first column in M_matrix. Binary: 0/1; Ordered: 1..K_i; Continuous: numeric.
#' @param d an integer specifying the number of latent dimensions.
#' @param M_matrix a K x (d+1) matrix of theoretical codings used to constrain IRT-M  (default=NULL). First column should match column names in Y_in.
#' @param item_type Integer vector length K: 1=binary, 2=ordered polytomous, 3=continuous. If NULL (default) then classify_with_thresholds helper
#' function is used to infer it.
#' @param tau List length K: thresholds for ordered items (length K_i−1), NULL otherwise. If list is NULL (default) then classify_with_thresholds helper
#' function is used to infer it. Any input here is ignored if item_type is NULL.
#' @param nburn an integer specifying the number of burn-in MCMC iterations.
#' @param nsamp an integer specifying the number of sampling MCMC iterations.
#' @param thin an integer specifying the number of thinning MCMC samples.
#' @param learn_loadings If TRUE, learn Omega (loading covariance); else learn Sigma (trait covariance).
#' @param hyperparameters List with S0 (d x d), O0 (d x d) [optional], nu0 [optional], mu [optional].
#' @param display_progress Show progress bar.
#' @return A list containing the following components:
#'   \item{lambda}{An array of dimension (K x d x nsamp/thin) containing posterior samples of item discrimination parameters.}
#'   \item{b}{A matrix of dimension (K x nsamp/thin) containing posterior samples of item difficulty parameters for categorical items.}
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

#' @return IRT result list with theta, lambda, b (categorical), Sigma/Omega, alpha/beta/sigma_cont (continuous).

irt_m_mixed <- function(Y_in, d, M_matrix = NULL,
                        item_type = NULL, tau = NULL,
                        nburn = 1000, nsamp = 1000, thin = 1,
                        learn_loadings = FALSE, hyperparameters = list(),
                        display_progress=TRUE) {

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


  ## 2. Assign types to each item (binary, polytomous, or continuous) ----------


  # If user doesn't enter item types and thresholds, assign item types to each
  # item (1=binary, 2=ordered polytomous, 3=continuous)
  # Find midpoint thresholds for each polytomous item
  if (is.null(item_type)) { #if user doesn't enter item_type, ignore thresholds
    itm_out <- classify_with_thresholds(Y)
    item_type <- itm_out$item_type
    tau_un <- itm_out$thresholds
  }
  else { #check if item_type and thresholds are both accurate; default conversions w/o thresholds, with recording
    log_file = "irtm_user_input_validation.log"

    log_diag <- function(msg) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      full_msg <- paste0("[", timestamp, "] ", msg, "\n")
      cat(full_msg, file = log_file, append = TRUE)
      warning(msg, call. = FALSE)
    }

    item_names <- colnames(Y)

    # -----------------------------
    # Global structure checks
    # -----------------------------

    if (length(item_type) != K) {
      stop("Length of item_type does not match number of columns in Y_in.")
    }

    if (length(thresholds) != K) {
      stop("Length of thresholds list does not match number of columns in Y_in.")
    }

    # -----------------------------
    # Per‑item validation
    # -----------------------------

    for (j in seq_len(K)) {

      name <- item_names[j]
      col  <- Y[[j]]
      type <- item_type[j]
      thr  <- thresholds[[j]]
      thr_wrong <- FALSE

      col_no_na <- col[!is.na(col)]
      uniq_vals <- sort(unique(col_no_na))
      n_unique  <- length(uniq_vals)

      # Stop and allow user to delete item with only one unique value
      if (n_unique < 2) stop(sprintf("Item %d has fewer than 2 unique values. Please drop this item and start again.", j))

      # -----------------------------
      # Check item_type validity
      # -----------------------------
      if (!(type %in% c(1, 2, 3))) {
        log_diag(sprintf("Item '%s' has invalid item_type %s (must be 1, 2, or 3), converting to continuous type.",
                         name, type))
        item_type[j]<-3
        thresholds[[j]] <- NULL
        next
      }

      # -----------------------------
      # Binary checks
      # -----------------------------
      if (type == 1) {
        if (!is.null(thr)) {
          log_diag(sprintf("Binary item '%s' should not have thresholds; deleting them.", name))
          thresholds[[j]] <- list(NULL)
          thr_wrong <- TRUE
        }
        if (n_unique != 2) {
          log_diag(sprintf("Binary item '%s' does not have exactly 2 unique values, converting to continuous type.", name))
          item_type[j]<-3
          thresholds[[j]] <- list(NULL)
          thr_wrong <- TRUE
        }
        next
      }

      # -----------------------------
      # Polytomous checks
      # -----------------------------
      if (type == 2) {
        if (n_unique < 3) {
          log_diag(sprintf("Polytomous item '%s' has fewer than 3 unique values, converting to binary", name))
          item_type[j] <- 1
          thresholds[[j]] <- list(NULL)
          thr_wrong <- TRUE
          next
        }
        if (is.null(thr)) {
          log_diag(sprintf("Polytomous item '%s' has NULL thresholds, recalculating thresholds", name))
          thr_wrong <- TRUE
          thresholds[[j]] <- list(NULL)
          thr_new <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
          thresholds[[j]] <- list(thr_new)  # wrapped
          next
        } else {
          if (!is.numeric(thr)) {
            log_diag(sprintf("Polytomous item '%s' has non-numeric thresholds, recalculating thresholds", name))
            thr_wrong <- TRUE
            thresholds[[j]] <- list(NULL)
            thr_new <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
            thresholds[[j]] <- list(thr_new)  # wrapped
            next
          }
          if (any(diff(thr) <= 0)) {
            log_diag(sprintf("Polytomous item '%s' has non-increasing thresholds, recalculating thresholds", name))
            thr_wrong <- TRUE
            thresholds[[j]] <- list(NULL)
            thr_new <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
            thresholds[[j]] <- list(thr_new)  # wrapped
            next
          }
          if (length(thr) != (n_unique - 1)) {
            log_diag(sprintf(
              "Polytomous item '%s' has %d thresholds but should have %d, recalculating thresholds",
              name, length(thr), n_unique - 1
            ))
            thr_wrong <- TRUE
            thresholds[[j]] <- list(NULL)
            thr_new <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
            thresholds[[j]] <- list(thr_new)  # wrapped
            next
          }
          if (min(thr) < min(uniq_vals) || max(thr) > max(uniq_vals)) {
            log_diag(sprintf(
              "Polytomous item '%s' has thresholds outside observed data range, recalculating thresholds", name
            ))
            thr_wrong <- TRUE
            thresholds[[j]] <- list(NULL)
            thr_new <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
            thresholds[[j]] <- list(thr_new)  # wrapped
            next
          }
        }
        next
      }

      # -----------------------------
      # Continuous checks
      # -----------------------------
      if (type == 3) {
        if (!is.null(thr)) {
          log_diag(sprintf("Continuous item '%s' should not have thresholds; deleting them.", name))
          thresholds[[j]] <- list(NULL)
        }
        if (sd(col_no_na) < 1e-8) {
          log_diag(sprintf("Continuous item '%s' has extremely small variance", name))
        }
      next
      }
    }

    # unwrap the list-of-lists if thresholds recalculated
    if (thr_wrong==TRUE) thresholds <- lapply(thresholds, function(z) z[[1]])
  }

  ## 3. Normalize items and thresholds to lie within [0,1]----------------------

  #additionally runs diagnostics on items and thresholds as an extra check
  normed_items <- normalize_items(Y_in, item_type, tau_un, log_file = "irtm_item_norm_diagnostics.log")
  tau <- normed_items$thresholds_norm
  Y_norm <- normed_items$Y_norm


  ## 4. Defaults for constraints / anchors -------------------------------------

  M <- array(0, c(d, d, K))
  for (i in 1:K) M[,,i] <- diag(d) * 2
  Yfake <- NULL; theta_fake <- NULL; anchors_count <- 0
  d_which_fix <- integer(0)
  d_theta_fix <- matrix(numeric(0), 0, d)
  Y_all <- Y_norm

  ## 5. If M_matrix is supplied, validate and construct M and anchors-----------
  if (!is.null(M_matrix)) {
    # Coerce to data.frame to avoid factor weirdness
    M_df <- as.data.frame(M_matrix, stringsAsFactors = FALSE)
    item_ids <- as.character(M_df[[1]])
    coln <- colnames(Y_norm)

    #Check to ensure that M has d+1 dimensions if not NULL.
    if (ncol(M_matrix) != (d+1L)) {
      stop("M_matrix must have d+1 columns (item IDs + d constraint columns).")
    }

    #Check to ensure that the K column headers in the input data Y_in
    #match the elements in the first column of the constraint matrix M_matrix
    #exit if not
    if (!setequal(coln, item_ids)) {
      stop("Item labels do not match between Y_in colnames and the first column in M_matrix. Please address the inconsistency before running again.")
    }

    # Reorder M_matrix rows to align with colnames(Y)
    # ensures alignment downstream:
    ord <- match(coln, item_ids)
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
    if (dim(M_array)[3] != as.integer(K)){
      stop("Internal error: constructed M array does not match number of items.")
    }

    # Generate Anchors

    l2 <- pair_gen_anchors(M_array,5)
    Yfake <- l2[[1]]; theta_fake <- l2[[2]]
    colnames(Yfake) <- coln
    # Ensure Yfake has same number of items and named columns
    if (ncol(Yfake) != K) {
      stop("pair_gen_anchors() returned anchors with incorrect number of items.")
    }

    # Stack anchors above real data for the sampler
    Y_all <- rbind(Yfake, Y_norm)
    anchors_count <- nrow(Yfake)
    d_which_fix <- seq_len(anchors_count)
    d_theta_fix <- as.matrix(theta_fake)
    if (ncol(d_theta_fix) != d) stop("theta_fake must have d columns.")
  }

  # One more sanity check before passing to C++ sampler
  if (anyNA(Y_all)) {
    stop("Internal error: Y_all contains NA values after processing. ",
         "This should not happen. Please report this bug with a reproducible example.")
  }

  ## 6. Last setup for mixed sampler with checks--------------------------------


  # Split Y_all into categorical vs continuous for mixed sampler
  Y_cat  <- matrix(NA_real_, nrow(Y_all), K)
  Y_cont <- matrix(NA_real_, nrow(Y_all), K)

  for (k in 1:K) {
    type <- item_type[k]
    if (type == 1) {
      Y_cat[,k] <- Y_all[,k]
    } else if (type == 2) {
      Y_cat[,k] <- Y_all[,k]
      tk <- tau[[k]]
      #last check of thresholds
      if (is.null(tk) || length(tk) < 1) stop(sprintf("tau[[%d]] must be provided for ordered item.", k))
      if (is.unsorted(tk)) stop(sprintf("tau[[%d]] must be strictly increasing.", k))
    } else if (type == 3) {
      Y_cont[,k] <- Y_all[,k]
    } else {
      stop("item_type values must be 1 (binary), 2 (ordered), or 3 (continuous).")
    }
  }

  ## 7. Fix hyperparameters and build lambda constraints------------------------

  # Hyperparameters and defaults
  S0  <- if (is.null(hyperparameters[['S0']])) diag(d) else hyperparameters[['S0']]
  O0  <- if (is.null(hyperparameters[['O0']])) diag(d) * 25 else hyperparameters[['O0']] # same as pscl default
  nu0 <- if (is.null(hyperparameters[['nu0']])) d else hyperparameters[['nu0']]
  mu  <- if (is.null(hyperparameters[['mu']])) rep(0, d) else hyperparameters[['mu']]

  # Build lambda constraints (lbs/ubs) from M (same semantics as your current code)
  lbs <- matrix(NA_real_, K, d)
  ubs <- matrix(NA_real_, K, d)
  for (k in seq_len(K)) {
    for (j in seq_len(d)) {
      val <- M[j,j,k]
      if (is.na(val))         { lbs[k,j] <- -Inf; ubs[k,j] <-  Inf }
      else if (val == 0)      { lbs[k,j] <-  0  ; ubs[k,j] <-   0  }
      else if (val == 1)      { lbs[k,j] <-  0  ; ubs[k,j] <-  Inf }
      else if (val == -1)     { lbs[k,j] <- -Inf; ubs[k,j] <-   0  }
      else                    { lbs[k,j] <- -Inf; ubs[k,j] <-  Inf }
    }
  }

  ## 8. Run constrained_IRT for mixed data--------------------------------------

  # First decide what covariance structure to learn
  # Default: learn Sigma (latent trait covariance).
  # If learn_loadings=TRUE, learn Omega instead.
  # Note: these are mutually exclusive: one can't learn both.
  learnS <- !learn_loadings
  learnO <- learn_loadings

  if(display_progress)
    message('Sampling...')

  # Convert tau (R list) to arma::field<arma::vec> automatically via Rcpp (as.list ok)
  tau_field <- tau

  irt <- sample_mixed_irt(
    Y_cat = Y_cat,
    Y_cont = Y_cont,
    item_type = item_type,
    tau = tau_field,
    d = d,
    nu0 = nu0,
    S0 = S0,
    lbs = lbs,
    ubs = ubs,
    ind = d_which_fix,
    theta_fix = d_theta_fix,
    nburn = nburn,
    nsamp = nsamp,
    thin = thin,
    learn_Sigma = learnS,
    learn_Omega = learnO,
    display_progress
  )

  ## 9. Remove anchors from theta before returning -----------------------------

  if (!is.null(Yfake) && anchors_count > 0) {
    irt$theta <- irt$theta[-seq_len(anchors_count), , , drop=FALSE]
  }

  irt$item_type <- item_type
  irt$tau <- tau
  irt$constraints <- list(lbs = lbs, ubs = ubs)
  irt$hyperparameters <- list(S0=S0, O0=O0, nu0=nu0, mu=mu)

  return(irt)
}
