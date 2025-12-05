#' @title pair_gen_anchors_continuous_columnwise
#'
#' @description
#' This function generates synthetic anchor respondents for continuous IRT-M models.
#' It produces column (item)-specific scaling. It extends
#' \code{\link{pair_gen_anchors}} to handle continuous outcomes by scaling
#' anchor responses to match each item's observed distribution.
#' By making colum-specific scales, it allows the anchors to handle input data
#' produced by heterogeneous processes
#'
#' @details
#' Overview:
#'
#' The pair_gen_anchors set of functions creates anchor respondents with extreme latent trait values
#' (theta). pair_gen_anchors_continuous_columnwise() differs from the base (binary) case in
#' that it generates continuous response values scaled
#' appropriately for each item. These continuous anchors use values that are extreme relative to each item's
#' observed distribution.
#'
#' **Two-step process:**
#'
#' 1. **Generate structural template**: Calls \code{\link{pair_gen_anchors}}
#'    to create the pattern of "high" (1) vs. "low" (0) responses based on
#'    the M-matrix constraints and extreme theta values.
#'
#' 2. **Scale to item distributions**: Transforms each column (item) of the
#'    binary template from {0, 1} to [extreme_low, extreme_high] based on
#'    that item's observed mean and standard deviation.
#'
#' **Scaling formula for each item k:**
#'
#' \deqn{extreme\_low_k = mean(Y[,k]) - extreme\_multiplier \times sd(Y[,k])}
#' \deqn{extreme\_high_k = mean(Y[,k]) + extreme\_multiplier \times sd(Y[,k])}
#' \deqn{Yfake[,k] = Yfake\_binary[,k] \times (extreme\_high_k - extreme\_low_k) + extreme\_low_k}
#'
#' This ensures that:
#' - Binary 0 → extreme low value for item k's scale
#' - Binary 1 → extreme high value for item k's scale
#' - The M-matrix structure (which items should be high/low) is preserved
#'
#' **Why column-specific scaling matters:**
#'
#' When items have vastly different scales (e.g., export volumes in millions,
#' concentration indices in [0,1], percentages in [0,100]), using a single
#' global scale would make anchors too extreme for some items and not extreme
#' enough for others. Column-specific scaling ensures each item's anchors are
#' appropriately extreme relative to that item's distribution.
#'
#' @section Use Cases:
#'
#' Recommended when:
#' - Items have heterogeneous scales (e.g., dollars, percentages, indices)
#' - Ratio of largest to smallest item SD > 10
#' - Items represent fundamentally different quantities
#'
#' Example applications:
#' - Mixed measurement types: counts, continuous scores, and normalized indices
#'
#' @param M
#' A 3-dimensional array of dimension \code{d × d × K} containing K M-matrices.
#' Each M[,,k] is a \code{d × d} diagonal matrix where M[j,j,k] specifies the
#' loading constraint for item k on dimension j:
#' \itemize{
#'   \item 1 = positive loading
#'   \item -1 = negative loading
#'   \item 0 = zero loading (fixed at zero)
#'   \item NA = unconstrained
#' }
#' Must have the same structure as required by \code{\link{pair_gen_anchors}}.
#'
#' @param Y_data
#' A numeric matrix of dimension \code{N × K} containing the observed
#' continuous response data. Used to compute item-specific means and
#' standard deviations for scaling anchors. Must not contain missing values
#' (NAs) in columns that will be used for anchor generation.
#'
#' @param extreme_multiplier
#' Numeric value specifying how many standard deviations from the mean
#' anchor values should be placed. Default is 2.5, which places anchors
#' at approximately the 0.6th and 99.4th percentiles for normally distributed
#' data. Larger values create more extreme anchors; smaller values create
#' anchors closer to the observed data range.
#'
#' @param use_quantiles Logical. If TRUE (default), use empirical quantiles
#' to define extreme values, which automatically respects data bounds.
#' If FALSE, use mean ± extreme_multiplier*SD, which may produce
#' out-of-bounds values for bounded data (e.g., negative values for
#' non-negative items).
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item{\code{Yfake}}{A matrix of dimension \code{(n_anchors × K)} containing
#'     continuous response values for the synthetic anchor respondents, where:
#'     \itemize{
#'       \item n_anchors = 2 if d=1
#'       \item n_anchors = d(d-1)*4 if d>1
#'     }
#'     Each column is scaled to that item's distribution. Values represent
#'     extreme observations relative to each item's mean and standard deviation.
#'   }
#'   \item{\code{theta_fake}}{A list of length n_anchors, where each element is
#'     a numeric vector of length d representing the fixed latent trait values
#'     for each anchor respondent. These are the same extreme theta values
#'     (±A) produced by \code{\link{pair_gen_anchors}}.
#'   }
#' }
#'
#' @section Relationship to Binary Anchors:
#'
#' This function preserves the structural logic of \code{\link{pair_gen_anchors}}
#' while adapting the magnitude of the per-item distribution to continuous data:
#'
#' - Maintains: Which anchors have high vs. low values on which items
#'   (determined by M-matrix and theta signs)
#' - Adapts: The numeric values of "high" and "low" for each item
#'   (scaled to item's distribution)
#'
#' @export
#'
#' @examples
#' # Example with heterogeneous items
#' set.seed(123)
#' N <- 100
#' K <- 3
#' d <- 2
#'
#' # Create data with very different scales
#' Y <- cbind(
#'   exports_millions = rnorm(N, mean = 500, sd = 200),     # [0, 1000]
#'   workforce_pct    = rnorm(N, mean = 5, sd = 2),          # [0, 10]
#'   hhi_index        = rnorm(N, mean = 0.5, sd = 0.2)       # [0, 1]
#' )
#'
#' # M-matrix: exports and workforce load on dim1, HHI loads on dim2
#' M <- array(0, c(2, 2, 3))
#' M[1, 1, ] <- c(1, 1, 0)   # Loadings on dimension 1
#' M[2, 2, ] <- c(0, 0, 1)   # Loadings on dimension 2
#'
#' # Generate scaled anchors
#' anchors <- pair_gen_anchors_continuous_columnwise(M, Y, extreme_multiplier = 2.5)
#'
#' # Examine results
#' print(anchors$theta_fake[[1]])  # e.g., [5, 5]
#' print(anchors$Yfake[1, ])
#' # Might show: [1000, 10, 1.0] - each scaled to item's range
#'
#' # Compare to item distributions
#' colMeans(Y)                     # Original means
#' apply(Y, 2, sd)                 # Original SDs
#' range(anchors$Yfake[, 1])       # Anchor range for item 1
#' range(anchors$Yfake[, 2])       # Anchor range for item 2
#' range(anchors$Yfake[, 3])       # Anchor range for item 3
#'
#' @seealso
#' \code{\link{pair_gen_anchors}} for binary anchor generation
#'
#' \code{\link{irt_m}} for the main user-facing function that automatically
#' calls this function when \code{family = "continuous"} and \code{M_matrix}
#' is provided
#'
#' \code{\link{M_constrained_irt_continuous}} for the underlying sampler that
#' uses these anchors
#'

pair_gen_anchors_continuous_columnwise <- function(
    M, Y_data,
    extreme_multiplier = 2.5,
    use_quantiles = TRUE,
    lower_quantile = 0.001,
    upper_quantile = 0.999
) {

  # Step 1: Use binary anchor logic (defines M structure)
  binary_anchors <- pair_gen_anchors(M, A = 5)
  Yfake_binary <- binary_anchors$Yfake
  theta_fake   <- binary_anchors$theta_fake

  K <- ncol(Y_data)
  n_anchors <- nrow(Yfake_binary)

  Yfake_itemscale <- matrix(NA_real_, n_anchors, K)

  for (k in seq_len(K)) {
    col_data <- Y_data[, k]

    if (use_quantiles) {
      col_low  <- quantile(col_data, probs = lower_quantile, na.rm = TRUE)
      col_high <- quantile(col_data, probs = upper_quantile, na.rm = TRUE)
    } else {
      col_mean <- mean(col_data, na.rm = TRUE)
      col_sd   <- sd(col_data, na.rm = TRUE)

      col_low  <- max(col_mean - extreme_multiplier * col_sd,
                      min(col_data, na.rm = TRUE))
      col_high <- min(col_mean + extreme_multiplier * col_sd,
                      max(col_data, na.rm = TRUE))
    }

    # Map binary Y_fake in {0,1} to {col_low, col_high}
    Yfake_itemscale[, k] <- Yfake_binary[, k] * (col_high - col_low) + col_low

    # Fill NAs with item mean
    # (NAs indicate items that don't load on this anchor's dimensions,
    #  and are thus uninformative;
    #  in the continuous case, mean is the "uninformative" translation)
    na_idx <- is.na(Yfake_itemscale[, k])
    if (any(na_idx)) {
      Yfake_itemscale[na_idx, k] <- mean(col_data, na.rm = TRUE)
    } #end fill NAs with column mean
  }  #end columnwise (K) for-loop

  list(
    Yfake      = Yfake_itemscale,
    theta_fake = theta_fake
  )
}
