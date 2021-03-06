% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networks.R
\name{create_empty_network}
\alias{create_empty_network}
\title{Create a network object.}
\usage{
create_empty_network(p)
}
\arguments{
\item{p}{The number of nodes in the network}
}
\value{
A network object.
}
\description{
Creates a 'network' object containing no modules.
}
\examples{
nw <- create_empty_network(10)
plot(nw) # A network with no edges.
}
