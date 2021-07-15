# exampleNetwork ---------------------------------------------------------------

#' Example Network
#' 
#' Example data describing a network of connected links
#' 
#' @param n_links number of links that the network to be returned shall contain. 
#'   If there is no network with the given number of links, a network that is 
#'   slightly smaller or bigger is returned. Giving -1L here returns the biggest
#'   possible network that is stored in this package. 
#' @param index integer number to switch between different sub nets if more 
#'   than one subset of the required size is available. 
#' @return data frame with (roughly) \code{n_links} observations of two
#'   variables. The variables \code{us_node_id} (upstream node ID) and
#'   \code{ds_node_id} (downstream node ID) define the connections between
#'   links. They are needed if the list of connected links upstream of each node
#'   is to be calculated by means of \code{\link{getConnectedLinks}}.
#' 
#' @export
exampleNetwork <- function(n_links = 30L, index = 1L)
{
  # Full network
  x <- utils::read.csv(
    system.file("extdata/exampleNetwork.csv", package = "kwb.graph"), 
    stringsAsFactors = FALSE
  )
  
  if (n_links == -1L) {
    return(x)
  }
  
  connected <- kwb.graph::getConnectedLinks(x)
  
  # Deviations of actual subnet sizes from required size (number of links)
  diffs <- abs(lengths(connected) - n_links)
  
  # Indices of subnets that are matching the size requirement best
  indices <- which(diffs == min(diffs))
  
  # Function to select an element at index i out of x. Take care of indices that
  # exceed the length of x by use of modulo function
  get_at <- function(x, i) x[((i - 1L) %% length(x)) + 1L]
  
  # One subnet of best matching size  
  x[connected[[get_at(indices, index)]], ]
}
