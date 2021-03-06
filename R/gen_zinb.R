#' The Zero-Inflated Negative Binomial Distribution
#' 
#' @param x A vector of quantities.
#' @param size The dispersion paramater used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param mu The mean parameter used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param rho The zero-inflation parameter.
#' @param log Logical; if \code{TRUE}, then log(d) is returned.
#' @return The value(s) of the density function evaluated at \code{x}.
#' @references 
#' \insertRef{grimes21}{SeqNet}
#' @export 
#' @examples 
#' x <- rzinb(10, 1, 10, 0.1)
#' p <- pzinb(x, 1, 10, 0.1)
#' y <- qzinb(p, 1, 10, 0.1)
#' all(x == y)
#' # Compute P(0 < X < 5) for X ~ ZINB(1, 10, 0.1)
#' sum(dzinb(0:5, 1, 10, 0.1))
dzinb <- function(x, size, mu, rho = 0, log = FALSE) {
  d <- rho * (x == 0) + (1 - rho) * dnbinom(x, size, mu = mu)
  if(log) {
    d <- log(d)
  }
  return(d)
}

#' The Zero-Inflated Negative Binomial Distribution
#'
#' @param p A vector of probabilities
#' @param size The dispersion paramater used in \code{\link[stats:NegBinomial]{dnbinom}}
#' @param mu The mean parameter used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param rho The zero-inflation parameter.
#' @param lower.tail Logical; if \code{TRUE}, then probabilities are P(X <= x).
#' Otherwise, P(X > x).
#' @param log.p Logical; if \code{TRUE}, then exp(p) is used.
#' @return The quantiles for the given probabilities.
#' @export 
#' @examples 
#' x <- rzinb(10, 1, 10, 0.1)
#' p <- pzinb(x, 1, 10, 0.1)
#' y <- qzinb(p, 1, 10, 0.1)
#' all(x == y)
#' # Compute P(0 < X < 5) for X ~ ZINB(1, 10, 0.1)
#' sum(dzinb(0:5, 1, 10, 0.1))
qzinb <- function (p, size, mu, rho, lower.tail = TRUE, log.p = FALSE) {
  if (!lower.tail) {
    p <- 1 - p
  }
  if (log.p) {
    p <- exp(p)
  }
  x <- qnbinom(pmax(0, (p - rho) / (1 - rho)), size, mu = mu)
  
  return(x)
}

#' The Zero-Inflated Negative Binomial Distribution
#'
#' @param q A vector of quantities.
#' @param size The dispersion paramater used in \code{\link[stats:NegBinomial]{dnbinom}}
#' @param mu The mean parameter used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param rho The zero-inflation parameter.
#' @param lower.tail Logical; if \code{TRUE}, then probabilities are P(X <= x).
#' Otherwise, P(X > x).
#' @param log.p Logical; if \code{TRUE}, then log(p) is returned.
#' @return The probabilities for the given \code{q} values.
#' @export 
#' @examples 
#' x <- rzinb(10, 1, 10, 0.1)
#' p <- pzinb(x, 1, 10, 0.1)
#' y <- qzinb(p, 1, 10, 0.1)
#' all(x == y)
#' # Compute P(0 < X < 5) for X ~ ZINB(1, 10, 0.1)
#' sum(dzinb(0:5, 1, 10, 0.1))
pzinb <- function (q, size, mu, rho, lower.tail = TRUE, log.p = FALSE) {
  p <- rho + (1 - rho) * pnbinom(q, size, mu = mu)
  if (!lower.tail) {
    p <- 1 - p
  }
  if (log.p) {
    p <- log(p)
  }
  
  return(p)
}

#' The Zero-Inflated Negative Binomial Distribution
#'
#' @param n The number of random values to return.
#' @param size The dispersion paramater used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param mu The mean parameter used in \code{\link[stats:NegBinomial]{dnbinom}}.
#' @param rho The zero-inflation parameter.
#' @return The randomly generated values from the distribution.
#' @export 
#' @examples 
#' x <- rzinb(10, 1, 10, 0.1)
#' p <- pzinb(x, 1, 10, 0.1)
#' y <- qzinb(p, 1, 10, 0.1)
#' all(x == y)
#' # Compute P(0 < X < 5) for X ~ ZINB(1, 10, 0.1)
#' sum(dzinb(0:5, 1, 10, 0.1))
rzinb <- function (n, size, mu, rho) {
  x <- ifelse(rbinom(n, 1, rho), 0, rnbinom(n, size, mu = mu))
  
  return(x)
}

#' Generate ZINB counts from an underlying network
#' 
#' The count data are generated based on the gene-gene associations of an
#' udnerlying network. An association structure is imposed by first generating 
#' data from a multivariate Gaussian distribution, and counts are then obtained
#' through the inverse tranformation method. To generate realistic counts, either 
#' a reference dataset or parameters for the ZINB model (size, mu, rho) can be 
#' provided. Parameters can be estimated from a reference using the
#' \code{\link{est_params_from_reference}} function.
#' @param n The number of samples to generate.
#' @param network A 'network' object or list of 'network' objects.
#' @param reference Either a vector or data.frame of counts from a reference
#' gene expression profile. If a data.frame is provided, each column should
#' correspond to a gene. If both \code{reference} and \code{params} are 
#' \code{NULL}, then parameters are estimated from the 
#' \code{\link{reference}} dataset.
#' @param params A matrix of ZINB parameter values; each column should contain 
#' the size, mu, and rho parameters for a gene.
#' @param library_sizes A vector of library sizes. Used only if \code{reference} is 
#' \code{NULL}.
#' @param adjust_library_size A boolean value. If \code{TRUE}, the library size of 
#' generated counts are adjusted based on the reference library sizes. If both 
#' \code{reference} and \code{library_sizes} is \code{NULL}, then no adjustment is made. 
#' By default, this adjustment is made if the necessary information is provided.
#' @param verbose Boolean indicator for message output.
#' @return A list containing the generated counts and the ZINB parameters used
#' to create them. If a list of networks were provided, then the results for
#' each network are returned as a list.
#' @export 
#' @examples
#' nw <- random_network(10) # Create a random network with 10 nodes.
#' nw <- gen_partial_correlations(nw) # Add weights to connections in the network.
#' # If no reference is provided, ZINB data are generated using an internal reference.
#' x <- gen_zinb(20, nw)$x # Simulate 20 ZINB observations from the network.
gen_zinb <- function(n, 
                     network,
                     reference = NULL,
                     params = NULL,
                     library_sizes = NULL,
                     adjust_library_size = NULL,
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
  
  if(is.null(reference) && is.null(params)) {
    if(verbose) {
      cat("Using breast cancer TCGA data as reference dataset.\n")
    }
    data("reference", envir = environment())
    params <- reference$params
    reference <- reference$rnaseq
  }
  
  # Estimate model paramters from reference dataset
  if(is.null(params)) {
    index <- 1:p # Default: use all p columns in the reference dataset.
    if(p > ncol(reference)) {
      # There are too few columns, sample with replacement.
      if(verbose) {
        cat("reference contains fewer columns than nodes in network.",
            "Sampling genes with replacement.\n")
      }
      index <- sample(1:ncol(reference), p, replace = TRUE)
    } else if (p < ncol(reference)) {
      # There are too many columns, take random subset.
      if(verbose) {
        cat("reference contains more columns than nodes in network.",
            "Sampling a subset of genes.\n")
      }
      index <- sample(1:ncol(reference), p)
    }
    # Subset the columns of the reference.
    reference <- reference[, index]
    model <- est_params_from_reference(
      reference,
      verbose = verbose)
    params <- model$params
    colnames(params) <- colnames(reference)
    
  } else {
    # TODO: add argument checks for 'params' (i.e. that it is a data.frame 
    # containing 3 rows, that values are positive, rho between 0 and 1, etc.)
    # params are provided; sample columns if necessary.
    index <- 1:p # Default: use all p columns of the params.
    if(p > ncol(params)) {
      # There are too few columns, sample with replacement.
      if(verbose) {
        cat("params contains fewer columns than nodes in network.",
            "Sampling genes with replacement.\n")
      }
      index <- sample(1:ncol(params), p, replace = TRUE)
    } else if (p < ncol(params)) {
      # There are too many columns, take random subset.
      if(verbose) {
        cat("params contains more columns than nodes in network.",
            "Sampling a subset of genes.\n")
      }
      index <- sample(1:ncol(params), p)
    }
    params <- params[, index]
  } 
  
  if(!single_network) {
    return(lapply(network, function(nw) {
      gen_zinb(n, nw, params = params, verbose = verbose)
    }))
  }
  
  x <- gen_gaussian(n, network)$x
  x <- pnorm(x) # Obtain n by p matrix of quantiles.
  
  # Convert quantiles to counts for each gene.
  for(i in 1:p) {
    x[, i] <- qzinb(x[, i], 
                    size = params[1, i], 
                    mu = params[2, i],
                    rho = params[3, i])
  }
  
  # Adjust library size by default (arg is null) or if requested (arg is TRUE).
  if(is.null(adjust_library_size) || adjust_library_size) {
    # Check if reference library sizes are provided.
    if(is.null(library_sizes)) {
      if(is.null(reference)) {
        # Give warning if adjustment was requested.
        if(!is.null(adjust_library_size) && adjust_library_size) 
          warning("Cannot adjust library size; must provide 'reference' or 'library_sizes'.")
      } else {
        library_sizes <- rowSums(reference)
      }
    }
    # Adjust x if library sizes are available.
    if(!is.null(library_sizes)) {
      # Adjust library sizes
      library_sizes_x <- rowSums(x)
      factors <- quantile(library_sizes, runif(n), type = 1) / 
        library_sizes_x
      x <- apply(x, 2, function(vals) floor(vals * factors))
      rownames(x) <- NULL
    }
  }
  
  return(list(x = x,
              params = params))
}



#' Estimate ZINB parameters from reference data
#' 
#' The observations in the reference dataset should be as homogeneous as possible.
#' For example, we should not expect differential expression or differential 
#' connectivity of genes within the sample. If the data are heterogeneous, the
#' estimation of the parameters may be unreliable.
#' @param reference Either a vector or data.frame of counts from a reference
#' gene expression profile. If a data.frame is provided, each column should
#' correspond to a gene.
#' @param verbose Boolean indicator for message output.
#' @return Returns a list containing a matrix of parameter estimates 'size', 
#' 'mu', and 'rho' for each gene in the reference, and the reference dataset
#' used. The parameter matrix can be used in \code{\link{gen_zinb}}.
#' @export
#' @examples
#' # The internal reference dataset already contains ZINB parameter estimates,
#' # so running est_params_from_reference() is not necessary. To simulate 
#' # ZINB data from a different RNA-seq reference dataset, the data can
#' # be passed into gen_zinb() directly using the 'reference' argument, and 
#' # est_params_from_reference() will be used automatically (i.e. the user
#' # does not need to call this function directly).
#' \donttest{
#' # An example using the reference dataset
#' data(reference) 
#' # The RNA-seq dataset should have samples as rows and genes as columns:
#' rnaseq <- reference$rnaseq
#' # Estimate ZINB params for first ten genes.
#' params <- est_params_from_reference(rnaseq[, 1:10])$params 
#' # However, the previous call is not needed for simulated ZINB data.
#' # The RNA-seq dataset can be passed directly to `gen_zinb()`.
#' nw <- random_network(10)
#' x <- gen_zinb(20, nw, reference = rnaseq[, 1:10])$x # Pass in 'rnaseq' directly.
#' }
est_params_from_reference <- function(reference,
                                      verbose = TRUE) {
  get_nb_params <- function(x) {
    fit <- fitdistrplus::fitdist(x, "nbinom",
                                 lower = 0)$estimate
    params <- list(size = unname(fit)[1], 
                   mu = unname(fit)[2],
                   rho = 0,
                   BIC = NA)
    L <- dnbinom(x, size = params$size, mu = params$mu)
    bic_nb <- -2 * sum(log(L)) + log(length(x)) * 2 # Two parameters in model.
    params$BIC <- bic_nb
    
    return(params)
  }
  
  get_zinb_params <- function(x) {
    # Use estimated size and mu from nb model to start zinb.
    fitzi <- NULL
    perc_zero <- mean(x == 0)
    if(perc_zero > 0) {
      # If x contains any zero values, try fitting a ZI model.
      fitzi <- tryCatch({
          m <- mean(x[x != 0]) # Used to calculate initial value.
          fit <- fitdistrplus::fitdist(x, dzinb, 
                                       start = list(size = m^2 / (var(x) - m), 
                                                    mu = m, 
                                                    rho = perc_zero / 2),
                                       lower = 0)$estimate
          if(fit[3] <= 0) {
            # If estimate for rho (ZI parameter) is not positive, return NULL.
            fit <- NULL
          } 
          fit
        }, error = function(e) {
          warning(paste("Estimation of ZINB parameters failed:", e))
          return(NULL)
        })
    }
    if(is.null(fitzi)) {
      # If estimation of ZINB failed or was not performed, return with BIC at Inf.
      params <- list(size = NA, mu = NA, rho = NA, BIC = Inf)
    } else {
      params <- list(size = unname(fitzi[1]), 
                     mu = unname(fitzi[2]),
                     rho = unname(fitzi[3]),
                     BIC = NA)
      L <- dzinb(x, params$size, params$mu, params$rho)
      params$BIC <- -2 * sum(log(L)) + log(length(x)) * 3 # Three parameters in model.
    }
    
    return(params)
  }
  
  get_params <- function(x) {
    # First fit NB model.
    fit <- get_nb_params(x)
    if(sum(x == 0) > 0) {
      # If there are zeroes in data, compare NB to ZINB using BIC criterion.
      fit_zinb <- get_zinb_params(x)
      if((fit_zinb$BIC <= fit$BIC) && (fit_zinb$rho > 0)) {
        # If ZINB is better fit and estimated rho is positive, use ZINB model.
        fit <- fit_zinb
      }
    }
    
    params <- c(size = fit$size,
                mu = fit$mu,
                rho = fit$rho)
    
    return(params)
  }
  
  if(is.vector(reference)) {
    reference <- matrix(reference, ncol = 1)
  } else if(!is.data.frame(reference) && !is.matrix(reference)) {
    stop("Argument 'reference' should be a vector or data.frame.")
  }
  
  p <- ncol(reference)
  params <- matrix(0, nrow = 3, ncol = p)
  colnames(params) <- colnames(reference)
  rownames(params) <- c("size", "mu", "rho")
  
  for(i in 1:p) {
    x <- reference[, i]
    params[, i] <- get_params(x)
  }
  
  return(list(params = params,
              reference = reference))
}