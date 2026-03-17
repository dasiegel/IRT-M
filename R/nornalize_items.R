#' @title normalize_items
#' @description Reads input data, item_types, thresholds for polytomous data,
#' and a log file name and returns normalized items such that: 1) binary items
#' take value 0 or 1; 2) polytomous items are normalized to lie between 0 and 1,
#' with appropriately normalized thresholds; and 3) continuous items are
#' set to mean 0 and then rescaled to lie in [0,1] (rescaling is first by
#' subtracting the mean and dividing by the sd and then by using the min/max to rescale).
#' @param Y input data (an N x K) matrix of data items in columns.
#' @param item_type a K-dimensional vector of item types (1, 2, 3)
#' @param thresholds a list of threshold vectors, non-NULL only for polytomous
#' items
#' @param log_file output file name for diagnostics (default "irtm_item_norm_diagnostics.log")
#' @return A list containing the following components:
#'   \item{Y_norm}{The normalized input data.}
#'   \item{thresholds_norm}{The normalized thresholds.}
#' @export
#'

normalize_items <- function(Y, item_type, thresholds, log_file = "irtm_item_norm_diagnostics.log") {

  # helper function to log messages
  log_diag <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    full_msg <- paste0("[", timestamp, "] ", msg, "\n")
    cat(full_msg, file = log_file, append = TRUE)
    warning(msg, call. = FALSE)
  }

  Y <- as.data.frame(Y)
  p <- ncol(Y)
  n <- nrow(Y)

  item_names <- colnames(Y)

  Y_norm <- matrix(NA, n, p)
  colnames(Y_norm) <- item_names

  thresholds_norm <- vector("list", p)

  for (j in seq_len(p)) {

    col <- Y[[j]]
    name <- item_names[j]
    type <- item_type[j]
    thr  <- thresholds[[j]]
    col_no_na <- col[!is.na(col)]
    uniq_vals <- sort(unique(col_no_na))
    n_unique  <- length(uniq_vals)

    # -------------------------
    # Diagnostics
    # -------------------------

    if (!is.numeric(col_no_na)) stop(sprintf("Item %s is not numeric. Please convert this item to numeric and start again.", name))
    # 1. Binary diagnostics
    if (type == 1) {
      if (n_unique != 2) {
        log_diag(sprintf("Binary item '%s' does not have exactly 2 unique values", name))
      }
    }

    # 2. Polytomous diagnostics
    if (type == 2) {
      if (n_unique < 3) {
        log_diag(sprintf("Polytomous item '%s' has fewer than 3 unique values", name))
      }
      if (is.null(thr)) {
        log_diag(sprintf("Polytomous item '%s' has NULL thresholds", name))
      } else {
        if (any(diff(thr) <= 0)) {
          log_diag(sprintf("Polytomous item '%s' has non-increasing thresholds", name))
        }
      }
      if (min(col_no_na) == max(col_no_na)) {
        log_diag(sprintf("Polytomous item '%s' has no variation", name))
      }
    }

    # 3. Continuous diagnostics
    if (type == 3) {
      if (n_unique < 2) {
        log_diag(sprintf("Continuous item '%s' has fewer than 2 unique values", name))
      }
      if (sd(col_no_na) < 1e-8) {
        log_diag(sprintf("Continuous item '%s' has extremely small variance", name))
      }
    }

    # -------------------------
    # Normalization
    # -------------------------

    # ---- 1. Binary: force to 0/1 ----
    if (type == 1) {
      uniq <- uniq_vals
      Y_norm[, j] <- ifelse(col == uniq[1], 0,
                            ifelse(col == uniq[2], 1, NA))
      thresholds_norm[[j]] <- NULL
    }

    # ---- 2. Polytomous: min-max scaling ----
    else if (type == 2) {
      col_min <- min(col_no_na)
      col_max <- max(col_no_na)

      Y_norm[, j] <- (col - col_min) / (col_max - col_min)

      if (!is.null(thr)) {
        thresholds_norm[[j]] <- (thr - col_min) / (col_max - col_min)
      } else {
        thresholds_norm[[j]] <- NULL
      }
    }

    # ---- 3. Continuous: mean 0, rescale to [0, 1] ----
    else if (type == 3) {
      mu <- mean(col_no_na)
      sdv <- sd(col_no_na)

      z <- (col - mu) / sdv

      z_min <- min(z, na.rm = TRUE)
      z_max <- max(z, na.rm = TRUE)

      if (z_max == z_min) {
        Y_norm[, j] <- rep(0, n)
      } else {
        Y_norm[, j] <- (z - z_min) / (z_max - z_min)
      }

      thresholds_norm[[j]] <- NULL
    }

    else {
      stop(sprintf("Unknown item type %d at item '%s'", type, name))
    }
  }

  list(
    Y_norm = Y_norm,
    thresholds_norm = thresholds_norm
  )
}
