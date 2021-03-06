% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modules.R
\name{connect_module_structure}
\alias{connect_module_structure}
\title{Connect disconnected components in an adjacency matrix}
\usage{
connect_module_structure(
  adj,
  weights = NULL,
  alpha = 100,
  beta = 1,
  epsilon = 10^-5
)
}
\arguments{
\item{adj}{An adjacency matrix to modify.}

\item{weights}{(Optional) weights used for sampling nodes.}

\item{alpha}{A positive value used to parameterize the Beta distribution.}

\item{beta}{A positive value used to parameterize the Beta distribution.}

\item{epsilon}{A small constant added to the sampling probability of each node.}
}
\value{
A modified adjacency matrix
}
\description{
Connect disconnected components in an adjacency matrix
}
\note{
This function is used in \code{\link{random_module_structure}} to
reconnect any disconnected components after edge removal and rewiring.
When connecting two components, a node is sampled from each component
with probability that is dependent on node degree; those two nodes are then
connected, which connects the components.
}
\examples{
# This function is used in `random_module_structure()` to reconnect any
# disconnected components. To demonstrate, we'll create a random structure,
# remove connections to one of the nodes (that node will then be a disconnected
# component), and use `connect_module_structure()` to reconnect it back to
# the main component.
adj <- random_module_structure(10)
adj <- remove_connections_to_node(adj, 1, prob_remove = 1)
# Note that there are now two components in the network:
components_in_adjacency(adj) 
g <- plot_network(adj)
# After connecting, the network contains one component.
adj <- connect_module_structure(adj)
components_in_adjacency(adj) 
plot_network(adj, g)
}
