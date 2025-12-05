#' @title pair_gen_anchors
#' @description
#' Generates synthetic anchor respondents with extreme latent trait values to
#' identify the orientation and scale of the latent space in IRT-M models.
#'
#' @details
#' The function creates anchor respondents by iterating through all pairs of
#' dimensions. For each pair of dimensions (j, l), the function generates four anchor respondents
#' such that the synthetic respondents exhibit extreme theta values on those two dimensions while holding
#' other dimensions at zero.
#'
#' **Number of anchors:**
#' - For \code{d = 1}: 2 anchors
#' - For \code{d > 1}: \code{d(d-1)*4} anchors
#'
#' **Anchor construction for d > 1:**
#'
#' For each pair of dimensions (j, l) where j ≠ l, four anchors are created:
#'
#' 1. **Anchor (+,+)**: theta has value +A on dimensions j and l, zero elsewhere
#' 2. **Anchor (-,+)**: theta has value -A on dimension j, +A on dimension l, zero elsewhere
#' 3. **Anchor (+,-)**: theta has value +A on dimension j, -A on dimension l, zero elsewhere
#' 4. **Anchor (-,-)**: theta has value -A on dimensions j and l, zero elsewhere
#'
#' **Response generation:**
#'
#' For each anchor respondent and each item k, the binary response (0 or 1)
#' is determined by the item's loadings on dimensions j and l as specified in
#' the M-matrix:
#'
#' - If the item loads positively on the dimension(s) where theta is positive,
#'   or loads negatively where theta is negative, the response tends toward 1
#' - If the item loads positively where theta is negative, or negatively where
#'   theta is positive, the response tends toward 0
#' - Items that load on neither dimension j nor l (M[j,j,k]=0 and M[l,l,k]=0)
#'   receive NA
#'
#' @section Examples:
#'
#' **For d=2:** Creates 8 anchors (matrix entries) total
#' - 4 anchors for the (dim1, dim2) pair:
#'   - theta = [10, 10, 0, ...]
#'   - theta = [-10, 10, 0, ...]
#'   - theta = [10, -10, 0, ...]
#'   - theta = [-10, -10, 0, ...]
#'
#' **For d=3:** Creates 24 anchors (matrix entries) total
#' - 4 anchors (in two synthetic rows) for pair (dim1, dim2): theta varies on dims 1,2; dim 3 = 0
#' - 4 anchors for pair (dim1, dim3): theta varies on dims 1,3; dim 2 = 0
#' - 4 anchors for pair (dim2, dim1): theta varies on dims 2,1; dim 3 = 0
#' - 4 anchors for pair (dim2, dim3): theta varies on dims 2,3; dim 1 = 0
#' - 4 anchors for pair (dim3, dim1): theta varies on dims 3,1; dim 2 = 0
#' - 4 anchors for pair (dim3, dim2): theta varies on dims 3,2; dim 1 = 0
#'
#' Note: Pairs (j,l) and (l,j) create different anchors because the logic
#' prioritizes dimension j in determining responses.
#'
#' **For d=1:** Creates 2 anchors
#' - Anchor 1: theta = [A]
#' - Anchor 2: theta = [-A]
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
#'
#' @param A
#' Numeric value specifying the magnitude of extreme theta values.
#' Anchor theta values will be ±A. Default recommended value is 5 for
#' identification purposes.
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item{\code{Yfake}}{A matrix of dimension \code{(n_anchors × K)} containing
#'     binary responses (0, 1, or NA) for the synthetic anchor respondents, where:
#'     \itemize{
#'       \item n_anchors = 2 if d=1
#'       \item n_anchors = d(d-1)*4 if d>1
#'     }
#'     Values are 0 (disagree/absent) or 1 (agree/present), with NA for items
#'     that do not load on the anchor's active dimensions.
#'   }
#'   \item{\code{theta_fake}}{A list of length n_anchors, where each element is
#'     a numeric vector of length d representing the fixed latent trait values
#'     for each anchor respondent. Each vector contains values in {-A, 0, +A}.
#'   }
#' }
#'
#' @export
#'
#' @examples
#' # Example with d=2, K=4
#' M <- array(0, c(2, 2, 4))
#' M[1,1,] <- c(1, 1, -1, -1)  # Item loadings on dim 1
#' M[2,2,] <- c(1, -1, 1, -1)  # Item loadings on dim 2
#'
#' anchors <- pair_gen_anchors(M, A = 5)
#'
#' # Check number of anchors created
#' nrow(anchors$Yfake)  # Should be 8 for d=2
#'
#' # Examine first anchor (theta = [5, 5])
#' anchors$theta_fake[[1]]  # [5, 5]
#' anchors$Yfake[1, ]       # Binary responses based on M-matrix


pair_gen_anchors = function(M,A){
  K = dim(M)[3]
  d = ncol(M)
  M[is.na(M)]=9
  if (d>1) {
    Yfake = matrix (NA, d*(d-1)*4, K)
    theta_fake = list()
    i = 1
    for (j in 1:d){
      for (l in 1:d){
        if(j==l)
          next
        for(k in 1:K){
          #YY
          ## logic: if all positive and not negative loading
          ## make 1; if not add an NA
          Yfake[i, k] = ifelse((M[j, j, k] == 1 && M[l, l, k] != -1) ||
                                 (M[l, l, k] == 1 && M[j, j, k] != -1), 1, NA)

          ## If all negative and not positive, fill with 0
          ## otherwise: NA
          if(is.na(Yfake[i, k]))
            Yfake[i, k] = ifelse((M[j, j, k] == -1 && M[l, l, k] != 1) ||
                                   (M[l, l, k] == -1 && M[j, j, k] != 1), 0, NA)

          #NY
          Yfake[i+1, k] = ifelse((M[j, j, k] == -1 && M[l, l, k] != 1) ||
                                   (M[l, l, k] == 1 && M[j, j, k] != -1), 1, NA)
          if(is.na(Yfake[i+1, k]))
            Yfake[i+1, k] = ifelse((M[j, j, k] == 1 && M[l, l, k] != -1) ||
                                     (M[l, l, k] == -1 && M[j, j, k] != 1), 0, NA)

          #YN
          Yfake[i+2, k] = ifelse((M[j, j, k] == 1 && M[l, l, k] != -1) ||
                                   (M[l, l, k] == -1 && M[j, j, k] != 1), 1, NA)
          if(is.na(Yfake[i+2, k]))
            Yfake[i+2, k] = ifelse((M[j, j, k] == -1 && M[l, l, k] != 1) ||
                                     (M[l, l, k] == 1 && M[j, j, k] != -1), 0, NA)

          #NN
          Yfake[i+3, k] = ifelse((M[j, j, k] == -1 && M[l, l, k] != 1) ||
                                   (M[l, l, k] == -1 && M[j, j, k] != 1), 1, NA)
          if(is.na(Yfake[i+3, k]))
            Yfake[i+3, k] = ifelse((M[j, j, k] == 1 && M[l, l, k] != -1) ||
                                     (M[l, l, k] == 1 && M[j, j, k] != -1), 0, NA)

        }
        theta_fake[[i]] = rep(0, d)
        theta_fake[[i]][j] = A
        theta_fake[[i]][l] = A
        theta_fake[[i+1]] = rep(0, d)
        theta_fake[[i+1]][j] = -A
        theta_fake[[i+1]][l] = A
        theta_fake[[i+2]] = rep(0, d)
        theta_fake[[i+2]][j] = A
        theta_fake[[i+2]][l] = -A
        theta_fake[[i+3]] = rep(0, d)
        theta_fake[[i+3]][j] = -A
        theta_fake[[i+3]][l] = -A
        i = i+4
      }
    }
  }else if (d==1) {
    Yfake = matrix (NA, 2, K)
    theta_fake = list()
    for(k in 1:K){
      #Y
      if(M[1, 1, k] == 1) {
        Yfake[1, k]=1
      }else if(M[1, 1, k] == -1) {
        Yfake[1, k]=0
      }
      #N
      if(M[1, 1, k] == -1) {
        Yfake[2, k]=1
      }else if(M[1, 1, k] == 1) {
        Yfake[2, k]=0
      }
    }
    theta_fake[[1]] = rep(0, d)
    theta_fake[[1]][1] = A
    theta_fake[[2]] = rep(0, d)
    theta_fake[[2]][1] = -A
  }else {
    stop("The number of dimensions is less than 1.")
  }
  fakeList<- list("Yfake" = Yfake, "theta_fake" = theta_fake)
  return(fakeList)
}
