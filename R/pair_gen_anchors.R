#' @title pair_gen_anchors
#' @description This function generates anchor points from the M matrices.
#' It creates d(d-1)*4 fake respondents such that, for every pair of dimensions:
#' The first respondent has an extremely positive value of both dimensions of the pair,
#' The second has an extremely positive value for dim 1 and an extremely negative value for dim 2 of each pair,
#' The third has an extremely negative value for dim 1 and an extremely positive value for dim 2 of each pair,
#' The fourth has an extremely negative value of both dimensions of the pair.
#' These respondents' answers are imputed according to the directions of loadings specified by the M-matrices, i.e.,
#' if question k loads positively on dim 1 and positively on dim 2, the first respondent in the dim 1/dim 2 pair will have
#' yes for question k. If question k+1 loads  positively on dim 1 and negatively on dim 2 then the second respondent in
#' the dim 1/dim 2 pair will have yes on question k+1 and so on.
#' @param M a list containing K dxd M-matrices
#' @param A What value should be considered extreme for the latent dimensions.
#' @return A list with two elements:
#'   \item{Yfake}{A matrix of dimension (d(d-1)*4 x K) containing the imputed answers of the fake anchor respondents, where d is the number of dimensions and K is the number of questions. Values are 0 (no) or 1 (yes).}
#'   \item{theta_fake}{A list of d(d-1)*4 vectors, each of length d, representing the latent trait values for the fake anchor respondents. Each vector contains mostly zeros, with extreme values (A or -A) at the positions corresponding to the pair of dimensions being considered.}
#' @export
#' @export

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
          Yfake[i, k] = ifelse((M[j, j, k] == 1 && M[l, l, k] != -1) ||
                                 (M[l, l, k] == 1 && M[j, j, k] != -1), 1, NA)
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
