% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{get_node_names}
\alias{get_node_names}
\title{Get node names}
\usage{
get_node_names(x, ...)
}
\arguments{
\item{x}{Either a 'network', 'network_module', or 'matrix' object.}

\item{...}{Additional arguments.}
}
\value{
A vector containing the node names or node indices.
}
\description{
Get node names
}
\note{
Modules do not retain the names of each node, so the node indicies are
returned instead. These can be used to index into the vector of node
names obtained from the network.
}
\examples{
# Create a random network with 10 nodes. 
nw <- random_network(10)
get_node_names(nw) # Default names are 1, 2, ..., 10.
nw <- set_node_names(nw, paste("node", 1:10, sep = "_"))
get_node_names(nw) # Print out updated node names.
# Modules only contain the indicies to nodes, not the node names
module <- nw$modules[[1]]
get_node_names(module)
# When converting the network to a matrix, node names appear as column names.
adj_matrix <- get_adjacency_matrix(nw)
colnames(adj_matrix) 
}
