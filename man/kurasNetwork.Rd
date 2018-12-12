\name{kurasNetwork}
\docType{data}
\alias{kurasNetwork}
\title{Example data describing a network of connected links}
\description{
Example data describing a network of connected links. Taken from KURAS project 
(provided by Victor Philippon)
}
\usage{kurasNetwork}
\format{data frame with 10040 observations of two variables. The variables
\emph{us_node_id} (upstream node ID) and \emph{ds_node_id} (downstream node ID)
define the connections between links. They are needed if the list of connected
links upstream of each node is to be calculated by means of 
\code{\link{getConnectedLinks}}.
}
\keyword{kurasNetwork}
