#' @title classify_with_thresholds
#' @description Reads input data and returns a vector of item types
#' from among binary (1), polytomous (2), and continuous (3) types
#' and a list of vectors of thresholds separating categorical values, only
#' for polytomous items. Uses an adaptive cutoff for distinguishing
#' continuous from polytomous data.
#' @param x input data (an N x K) matrix of data items in columns.
#' @return A list containing the following components:
#'   \item{item_type}{A K-dimensional vector containing the type (in 1, 2, 3)
#'    of each item.}
#'   \item{thresholds}{A list of vectors containing thresholds that separate
#'   different values of each polytomous item.}
#' @export
#'

classify_with_thresholds <- function(x) {

  x <- as.data.frame(x)
  n <- nrow(x)
  p <- ncol(x)

  # adaptive cutoff for continuous vs polytomous
  K_max <- min(10, floor(n / 4))
  if (K_max < 3) K_max <- 3

  item_type  <- integer(p)
  thresholds <- vector("list", p)

  for (j in seq_len(p)) {

    col <- x[[j]]
    col_no_na <- col[!is.na(col)]

    uniq_vals <- sort(unique(col_no_na))
    n_unique  <- length(uniq_vals)
    if (!is.numeric(col_no_na)) stop(sprintf("Item %d is not numeric. Please convert this item to numeric and start again.", j))
    if (n_unique < 2) stop(sprintf("Item %d has fewer than 2 unique values. Please drop this item and start again.", j))

    # ---- 1. Binary ----
    if (n_unique == 2) {
      item_type[j] <- 1
      thresholds[[j]] <- list(NULL)   # wrapped
      next
    }

    # ---- 2. Polytomous ----
    if (n_unique >= 3 && n_unique <= K_max) {
      item_type[j] <- 2

      thr <- (uniq_vals[-1] + uniq_vals[-n_unique]) / 2
      thresholds[[j]] <- list(thr)  # wrapped
      next
    }

    # ---- 3. Continuous ----
    item_type[j] <- 3
    thresholds[[j]] <- list(NULL)     # wrapped
  }

  # unwrap the list-of-lists
  thresholds <- lapply(thresholds, function(z) z[[1]])

  list(
    item_type  = item_type,
    thresholds = thresholds
  )
}



