#' Generate RNA-seq data from an underlying network
#' 
#' The expression data are generated based on the gene-gene associations of an
#' underlying network. An association structure is imposed by first generating 
#' data from a multivariate Gaussian distribution. Those data are then used to
#' sample from the empirical distribution of gene expression profiles in the 
#' reference dataset using the inverse transform method.
#' @param n The number of samples to generate.
#' @param network A 'network' object or list of 'network' objects.
#' @param reference A data.frame containing reference gene expression data. Rows
#' should correspond to samples and columns to genes. If \code{NULL}, then the 
#' \code{\link{reference}} dataset is used.
#' @param verbose Boolean indicator for message output.
#' @return A list containing the simulated expression data and the reference 
#' dataset. If a list of networks were provided, then the results for
#' each network are returned as a list.
#' @references 
#' \insertRef{grimes21}{SeqNet}
#' @export 
#' @examples 
#' nw <- random_network(10) # Create a random network with 10 nodes.
#' nw <- gen_partial_correlations(nw) # Add weights to connections in the network.
#' # If no reference is provided, the internal RNA-seq reference dataset is used.
#' x <- gen_rnaseq(20, nw)$x # Simulate 20 observations from the network.
gen_rnaseq <- function(n, 
                       network,
                       reference = NULL,
                       verbose = TRUE) {
  if(n <= 0) {
    stop("Argument 'n' must be positive.")
  }
  
  single_network <- TRUE
  if(!is(network, "network")) {
    if(is.list(network) && all(sapply(network, function(nw) is(nw, "network")))) {
      p <- network[[1]]$p
      if(length(network) > 1 && !all(sapply(network[-1], function(nw) nw$p == p))) {
        stop(paste0("'", deparse(substitute(network)), 
                    "' is a list but does not contain networks of the same size."))
      }
      single_network <- FALSE
    } else {
      stop(paste0("'", deparse(substitute(network)), 
                  "' is not a 'network' object or list of 'network' objects."))
    }
  } else {
    p <- network$p
  }
  
  if(is.null(reference)) {
    if(verbose) {
      cat("Using breast cancer TCGA data as reference dataset.\n")
    }
    data("reference", envir = environment())
    df_ref <- reference$rnaseq
    df_ref <- sample_reference_data(df_ref, p)
    rm(reference)
  } else {
    df_ref <- sample_reference_data(reference, p)
  }
  
  if(!single_network) {
    return(lapply(network, function(nw) {
      gen_rnaseq(n, nw, df_ref, verbose = verbose)
    }))
  }
  
  x <- gen_gaussian(n, network)$x
  x <- pnorm(x) # Obtain n by p matrix of percentiles.
  for(i in 1:p) {
    # Setting type = 1 gives inverse of empirical distribution function.
    x[, i] <- quantile(df_ref[, i], x[, i], type = 1)
  }
  
  return(list(x = x,
              reference = df_ref))
}