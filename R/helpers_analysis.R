#' @title Average Thetas
#' @description Compute matrix of theta means over posterior distributions
#' @param theta_array An array of dimension (N x d x nsamp/thin) containing posterior samples of respondent latent trait values
#' @return theta_av N x d matrix of average thetas
#' @export
#'

theta_av <- function(theta_array) {

  N <- dim(theta_array)[1]
  d <- dim(theta_array)[2]
  z <- dim(theta_array)[3]

  # Reshape the array to 2D: (N * d) x nsamp/thin
  reshaped_array <- matrix(theta_array, nrow = N * d, ncol = z)

  # Compute the mean for each element and reshape back to N x d
  tav <- matrix(rowMeans(reshaped_array), nrow = N, ncol = d)
  rm(reshaped_array)
  rm(N)
  rm(d)
  rm(z)

  return(tav)
}

#' @title irt_vis
#' @description Takes as input the number of latent dimensions (d),
#' an N x (d+z) data frame with average thetas in the first d columns and variables not included in the
#' calculation of the thetas in the rest (T_out), and, optionally, a variable name (sub_name)
#' taken from T_out, and an output file name (out_file),
#' and returns either unconditional theta distributions or distributions subset by that variable
#' @param d The number of latent dimensions
#' @param T_out N x (d+z) data frame with average latent dimensions in first d columns
#' @param sub_name The name of a variable in T_out used for levels in the plot (Default = NULL)
#' @param out_file Output file name for plot (Default = NULL)
#' @return A ggplot2 object containing density ridge plots of the latent dimensions. When sub_name is NULL, the plot shows the distribution of each theta dimension. When sub_name is provided, the plot shows distributions faceted by theta dimension and grouped by the specified variable.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom ggridges geom_density_ridges
#' @importFrom rlang sym
#' @export
#'

irt_vis <- function(d, T_out, sub_name=NULL, out_file=NULL) {

  #initialize plot output
  irtv <- NULL
  theta_names <- colnames(T_out[1:d])
  Value<-NULL
  Theta<-NULL
    if (is.null(sub_name)) {   #If desired output is aggregated
    basedata <- melt(T_out[,1:d])
    if (d==1) { #When there is only one latent dimension
      colnames(basedata) <- c("Value")
      irtv <- ggplot(mutate(basedata,Theta = theta_names[1]),  # Replace Theta with a constant value
                     aes(x = Value,
                         y = Theta,
                         fill = Theta)) +
        geom_density_ridges() +
        scale_y_discrete(limits = rev(levels(factor(theta_names[1])))) +
        ggtitle('Posterior Distribution of Group Mean',
                subtitle = theta_names[1]) +
        scale_fill_brewer(palette = 'Spectral') +
        theme_bw()
    } else if (d>1) { #If there is more than one latent dimension
      colnames(basedata) <- c("Theta", "Value")
      irtv <- ggplot(basedata,
                         aes(x=Value,
                             y=Theta,
                             fill=Theta))+
        geom_density_ridges() +
        scale_y_discrete(limits = rev(levels(basedata$Theta))) +
        ggtitle('Posterior Distribution of Group Mean',
                subtitle= "Thetas") +
        scale_fill_brewer(palette='Spectral') +
        theme_bw()
    } else {
      stop("You have less than one latent dimension specified.")
    }
  } else { #Output is disaggregated by variable sub_name, which should be a factor

      mt_cols <- c(theta_names,sub_name)
      dat2 <- melt(T_out[,mt_cols])
      colnames(dat2) <- c(sub_name, "Theta", "Value")
      irtv <- ggplot(dat2,
                          aes(x=Value,
                              y=!!sym(sub_name),
                              fill=!!sym(sub_name)))+
        geom_density_ridges(alpha=.5) +
        ggtitle('Posterior Distribution of Group Mean',
                subtitle= "Thetas") +
        labs(y=sub_name,
             x= "Theta Posterior Estimates")+
        scale_y_discrete(limits=rev(levels(dat2$sub_name)))+
        facet_wrap(~Theta, ncol=2)+
        scale_fill_brewer(palette='Spectral') +
        theme_bw()
    }

  #Print plot to file if desired by user
  if (!is.null(out_file)) {
    ggsave(irtv, filename=out_file, width=5, height=5, units ="in", dpi=300)
  }
  return(irtv)
}

#' @title get_lambdas
#' @description Takes as input the array of lambdas from the irt list,
#' a vector of item names (can be taken from either Y_in or M_matrix), a vector of dimension names,
#' and, optionally, a vector comprising elaborations about each item.
#' Returns a list containing a data frame with the mean lambdas for each item-dimension pair,
#' possibly attaching elaborations to each item's string,
#' and a data frame with the items with the highest mean values of lambda for each dimension in order
#' @param lambda_array An array of dimension (K x d x nsamp/thin) containing posterior samples of item discrimination parameters.
#' @param item_names Vector of item names.
#' @param dim_names Vector of dimension names.
#' @param item_elab A vector comprising elaborations about each item (Default = NULL).
#' @return A list containing the following components:
#'   \item{av_lams}{A data frame of dimension (K x (1+d)) containing averages of item discrimination parameters.}
#'   \item{high_lams}{A data frame of dimension (K x d) containing an ordered list of the items with the highest mean values of lambda for each dimension.}
#' @export
#'

get_lambdas <- function(lambda_array, item_names, dim_names, item_elab=NULL) {

  #initialize output list
  lambdas <- vector("list", length=2)

  #Find average lambdas
  K <- dim(lambda_array)[1]
  d <- dim(lambda_array)[2]
  z <- dim(lambda_array)[3]
  # Reshape the array to 2D: (K * d) x nsamp/thin
  reshaped_array <- matrix(lambda_array, nrow = K * d, ncol = z)
  # Compute the mean for each element and reshape back to N x d
  lav <- matrix(rowMeans(reshaped_array), nrow = K, ncol = d)
  rm(reshaped_array)
  rm(K)
  rm(d)
  rm(z)

  #Combines elaborations into item strings if user supplied
  inames <-item_names
  if (!is.null(item_elab)) inames <- paste(item_names, item_elab, sep=": ")

  #Creates data frame for output of average lambdas
  av_lambs <- as.data.frame(cbind(inames, lav))
  colnames(av_lambs)<-c("QMap",dim_names)
  av_lambs[dim_names]<-lapply(av_lambs[dim_names], as.numeric)
  lambdas[[1]]<-av_lambs

  # Create data matrix with descending order
  # Extract the first column from av_lambs
  values <- av_lambs[[1]]
  # Extract the ordering columns (last d columns) and take their absolute value
  order_columns <- av_lambs[ , -1, drop=FALSE]
  order_columns <- as.data.frame(lapply(order_columns, abs))

  # Number of ordering columns (d)
  d <- ncol(order_columns)
  # Initialize an empty list to store reordered columns
  reordered_columns <- vector("list", d)

  # Loop through each ordering column and reorder `values` in descending order
  for (i in seq_len(d)) {
    reordered_columns[[i]] <- values[order(-order_columns[[i]])]
  }

  # Combine reordered columns into a data frame
  high_lambs <- as.data.frame(reordered_columns)
  # Set column names for B
  colnames(high_lambs) <- dim_names

  lambdas[[2]]<-high_lambs

  return(lambdas)
}

#' @title learned correlations
#' @description Takes as input either the Sigma covariance matrix, if the user has learned the factor covariance,
#' or the Omega covariance matrix, if the user has learned the loading covariance, as well as a vector of
#' dimension names.
#' Returns a correlation matrix with correlations between the dimensions.
#' @param cov_array An array of dimension (d x d x nsamp/thin) containing posterior samples of the relevant covariance matrix.
#' @param dim_names Vector of dimension names.
#' @return A data frame containing the correlation matrix derived from t input covariance array, with rows and columns labeled according to dim_names (if provided). Each cell represents the correlation between the corresponding dimensions.
#' @export
#'

dim_corr <- function(cov_array, dim_names = NULL) {

  d <- dim(cov_array)[1]
  z <- dim(cov_array)[3]

  # Reshape the array to 2D: (d * d) x nsamp/thin
  reshaped_array <- matrix(cov_array, nrow = d * d, ncol = z)

  # Compute the mean for each element and reshape back to d x d
  covav <- matrix(rowMeans(reshaped_array), nrow = d, ncol = d)
  rm(reshaped_array)
  rm(z)

  #compute correlation matrix
  # Compute the standard deviations (sqrt of diagonal elements)
  std_devs <- sqrt(diag(covav))

  # Create the correlation matrix
  corav <- covav / (std_devs %*% t(std_devs))

  corav<-as.data.frame(corav)

  if (!is.null(dim_names)) {
    if (length(dim_names)==d) {
      colnames(corav)<-dim_names
      rownames(corav)<-dim_names
    }
  }

  return(corav)
}
