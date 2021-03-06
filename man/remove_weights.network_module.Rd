% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{remove_weights.network_module}
\alias{remove_weights.network_module}
\title{Removes the weights of all connections}
\usage{
\method{remove_weights}{network_module}(x, ...)
}
\arguments{
\item{x}{Either a 'network', 'network_module', or 'matrix' object.}

\item{...}{Additional arguments.}
}
\value{
The modified object.
}
\description{
Removes the weights of all connections
}
\examples{
# Create a random network with 10 nodes and add random edge weights.
nw <- random_network(10)
nw <- gen_partial_correlations(nw)
is_weighted(nw)
# Remove the edge weights from the network.
nw <- remove_weights(nw)
is_weighted(nw)
}
