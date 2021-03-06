% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networks.R
\name{perturb_network}
\alias{perturb_network}
\title{Perturbs the connections in a network}
\usage{
perturb_network(
  network,
  n_hubs = 1,
  n_nodes = 0,
  rewire_hub_prob = 0.5,
  rewire_other_prob = 0.5,
  ...
)
}
\arguments{
\item{network}{The network to modify.}

\item{n_hubs}{The number of hub nodes to turn off.}

\item{n_nodes}{The number of non-hub nodes to rewire. When rewiring, the
degree of the node is unchanged.}

\item{rewire_hub_prob}{The probability that a connection is removed from
a hub that is selected to be turned off. If \code{rewire_hub_prob = 1}, then
all of the connections to the hub are removed.}

\item{rewire_other_prob}{The probability that a connection is rewired from
a non-hub that is selected for rewiring. If \code{rewire_other_prob = 1}, then
all of the connections to the hub are rewired; however, this does not mean
that all connections will be changed, as some connections may be removed
but later rewired back.}

\item{...}{Additional arguments passed to
\code{\link{rewire_connections_to_node}} and
\code{\link{remove_connections_to_node}}.}
}
\value{
The modified network.
}
\description{
The network is perturbed by removing connections from hubs and/or rewiring
other nodes in the network. By default, one hub is turned off (i.e. its
connections are removed each with probability \code{rewire_hub_prob = 0.5}), and
no other nodes are changed. Hub nodes are defined as those having degree
above three standard deviations from the average degree, and nodes are
sampled from these to be turned off; if there are no hub nodes, then
those with the largest degree are turned off.
}
\examples{
# Create a random network, perturb the network, then plot the differential network.
set.seed(12345)
nw <- random_network(100)
# Rewire 2 random hub genes and 10 other random genes:
nw_diff <- perturb_network(nw, n_hubs = 2, n_nodes = 10)
plot_network_diff(nw, nw_diff) 
}
