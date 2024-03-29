# getConnectedLinks ------------------------------------------------------------

#' Get Connected Links
#'
#' @param x data frame with each row representing a link of the network. Required
#' columns: \emph{us_node_id}, \emph{ds_node_id} 
#' @param upstream if TRUE (upstream), if FALSE (downstream), default: TRUE
#' @param version 1: R-implementation, 2: C-implementation, (default: 1)
#' @param dbg default: FALSE
#' @param ... additional arguments passed to  \code{\link{getConnectedLinks.C}}
#'
#' @return get connected links
#' @export
#' @useDynLib kwb.graph C_getDirectLinks C_getConnectedLinks
#' @examples 
#' network <- kwb.graph::exampleNetwork(n_links = -1L)
#' 
#' runtime.R <- vector()
#' runtime.C1 <- vector()  
#' runtime.C2 <- vector()  
#' 
#' n <- 1
#' resultSize <- 2054851
#' queueSize <- 100*1024
#' 
#' elapsed <- function(exp) system.time(exp)["elapsed"]
#' 
#' for (i in 1:n) {
#'   cat("run", i, "/", n, "\n")
#'   runtime.R[i] <- elapsed(x1 <- getConnectedLinks(
#'     network
#'   ))
#'   runtime.C1[i] <- elapsed(x2 <- getConnectedLinks(
#'     network, version = 2, resultSize = resultSize, queueSize = queueSize
#'   ))
#'   runtime.C2[i] <- elapsed(x3 <- getConnectedLinks(
#'     network, version = 3, resultSize = resultSize, queueSize = queueSize
#'   ))
#' }
#' 
#' cat("mean runtime with R-functions:", mean(runtime.R), "\n")
#' cat("mean runtime with C-functions(1):", mean(runtime.C1), "\n")  
#' cat("mean runtime with C-functions(2):", mean(runtime.C2), "\n")  
#' 
#' runtimeData <- data.frame(
#'   version = 1:3, 
#'   runtime = c(runtime.R), runtime.C1, runtime.C2
#' )
#' 
#' boxplot(runtime ~ version, data = runtimeData)
#' 
getConnectedLinks <- function(x, upstream = TRUE, version = 1, dbg = FALSE, ...)
{
  n <- nrow(x)
  
  # get the row indices of all upstream (or downstream) connected links
  if (version == 1) {
    direct.us <- getDirectLinks.R(x = x, upstream = upstream)
    connectedLinks <- getConnectedLinks.R(direct.us, dbg = dbg)
  }
  else {
    if (dbg) cat("getting direct links... ")
    direct.us <- getDirectLinks.C(x = x)
    if (dbg) cat("ok.\ngetting connected links...\n")
    connectedLinks <- getConnectedLinks.C(direct.us, version = version-1, dbg = dbg, ...)
    if (dbg) cat("getting connected links ok.\n")
  }
  #else {
  #  stop("version must be 1 (R-implementation) or 2 (C-implementation)!")
  #}
  connectedLinks
}

# getDirectLinks.R -------------------------------------------------------------

#' Get Direct Links (R Implementation)
#'
#' @param x data frame with each row representing a link of the network. Required
#' columns: \emph{us_node_id}, \emph{ds_node_id} 
#' @param upstream if TRUE (upstream), if FALSE (downstream), default: TRUE
#'
#' @return get direct links with R implementation
#' @export

getDirectLinks.R <- function
(
  x,
  upstream = TRUE
) 
{  
  # initialise boolean matrix where each row represents a link and the columns
  # holding TRUE represent the directly connected upstream links
  linked <- list()
  
  # set direct.us to TRUE at crossings between connected links
  for (i in seq_len(nrow(x))) {
    if (upstream) {
      linked[[i]] <- which(x[i, "us_node_id"] == x[, "ds_node_id"])
    }
    else {
      linked[[i]] <- which(x[i, "ds_node_id"] == x[, "us_node_id"])
    }
  }
  
  linked
}

# getDirectLinks.C -------------------------------------------------------------
#' Get Direct Links (C Implementation)
#'
#' @param x data frame with each row representing a link of the network. Required
#' columns: \emph{us_node_id}, \emph{ds_node_id} 
#' @param MAX_DIRECT_CONNECTIONS default: 5
#' @param dbg default: FALSE
#'
#' @return get direct links with C implementation
#' @export
#'

getDirectLinks.C <- function
(
  x, MAX_DIRECT_CONNECTIONS = 5, dbg = FALSE
)
{
  n <- nrow(x)  
  
  numberOfFound <- rep(as.integer(0), n)
  indicesOfFound <- as.integer(rep(0, MAX_DIRECT_CONNECTIONS*n))
  
  result <- .C(
    "C_getDirectLinks", 
    length = as.integer(n), 
    upstreamNodes = x$us_node_id,
    downstreamNodes = x$ds_node_id,
    numberOfFound = numberOfFound,
    indicesOfFound = indicesOfFound,
    maxNumber = as.integer(MAX_DIRECT_CONNECTIONS),
    dbg = as.integer(dbg)
    #,    PACKAGE = "kwb.graph"    
  )
  
  if (max(result$numberOfFound) > MAX_DIRECT_CONNECTIONS) {
    stop("It seems that you need to increase MAX_DIRECT_CONNECTIONS!")
  }
  
  indexMatrix <- matrix(result$indicesOfFound, ncol = MAX_DIRECT_CONNECTIONS)
  
  lapply(seq_len(n), function(i) {
    indexMatrix[i, seq_len(result$numberOfFound[i])]
  })
}

# getConnectedLinks.R ----------------------------------------------------------
#' Get Connected Links (R Implementation)
#'
#' @param directly.connected directly.connected
#' @param dbg default: FALSE
#'
#' @return get connected links with R implementation
#' @export
#' @importFrom kwb.utils catIf
getConnectedLinks.R <- function
(
  directly.connected, 
  dbg = FALSE
)
{
  # initialise result list
  all.connected <- list()
  
  n <- length(directly.connected)
  
  blocksize <- max(1, n %/% 80)
  
  # loop through list of direct links
  for (i in seq_len(n)) {
    
    kwb.utils::catIf(dbg && i %% blocksize == 0, ".")
    
    # initialise vector of indices of connected links
    all.connected[[i]] <- integer(0)
    
    # initialise boolean vector holding TRUE at indices representing the links
    # that are connected directly or indirectly to the current link i
    is.connected <- logical(length = n)
    
    # start queue of indices of connected links with indices of directly
    # connected links
    queue <- directly.connected[[i]] 
    
    while (length(queue) > 0) {
      
      # get and remove next index from queue
      link.index <- queue[1]
      queue <- queue[-1]        
      
      is.connected[link.index] <- TRUE
      
      if (link.index < i) {
        # if link with index link.index was already considered (before the
        # current link i) save full information on links that are connected to
        # the link with index link.index in the logical vector is.connected.
        is.connected[all.connected[[link.index]]] <- TRUE
      } 
      else {
        # add indices of directly connected links to the queue
        queue <- c(queue, directly.connected[[link.index]])
      }
    } # of while
    
    # save indices of connected links
    all.connected[[i]] <- which(is.connected)
  }
  
  kwb.utils::catIf(dbg, "<\n")
  
  all.connected
}

# getConnectedLinks.C ----------------------------------------------------------
#' #' Get Connected Links (C Implementation)
#'
#' @param directLinks directLinks 
#' @param resultSize default: 60000
#' @param queueSize default: 1024*1024
#' @param version version of C implementation: 1,2 or 3 (default: 1)
#' @param dbg default: FALSE
#'
#' @return get connected links with C implementation
#' @export
#'

getConnectedLinks.C <- function # getConnectedLinks.C
### getConnectedLinks.C
(
  directLinks, resultSize = 60000, queueSize = 1024*1024, version = 1,
  dbg = FALSE
)
{
  # Prepare the input data
  n <- length(directLinks)
  
  # vector containing the number of directly connected links for each link
  numberOfLinks <- sapply(directLinks, length)
  
  # matrix with each row representing a link. Column i contains the ID of the 
  # i-th directly connected link. If there is no such link the matrix element is 
  # - 1.
  maxDirectLinks = max(numberOfLinks)
  
  indicesOfDirectLinks <- matrix(
    rep(as.integer(-1), n * maxDirectLinks), ncol = maxDirectLinks
  )
  
  FUN <- function(i) {
    n.links <- numberOfLinks[[i]]
    onerow <- indicesOfDirectLinks[i, ]
    if (n.links > 0) {
      onerow[seq_len(n.links)] <- as.integer(directLinks[[i]])
    }
    onerow
  }
  
  links.matrix <- t(sapply(seq_len(n), FUN = FUN))    
  
  # in C, a one-dimensional array is used, created by putting one column after 
  # the other
  links.vector <- as.vector(links.matrix)

  if (dbg) {
    cat("\ngetConnectedLinks.C:\n",
        sprintf("Calling C_getConnectedLinks(%d, queueSize = %d, dbg = %d, version = %d)\n", 
                n, queueSize, dbg, version))    
  }
  
  result <- .C(
    "C_getConnectedLinks", 
    in_n = as.integer(n), 
    in_numberOfDirectLinks = as.integer(numberOfLinks),
    in_indicesOfDirectLinks = as.integer(links.vector),
    out_numberOfTotalLinks = as.integer(rep(0, n)),
    inout_result_size = as.integer(resultSize),
    out_result_origin = as.integer(rep(0, resultSize)),
    out_result_linked = as.integer(rep(0, resultSize)),
    in_queueSize = as.integer(queueSize),
    in_dbg = as.integer(dbg),
    in_version = as.integer(version)
    #,    PACKAGE = "kwb.graph"
  )

  if (dbg) {
    cat("\nBack in R from C_getConnectedLinks.\n")
  }
  
  valid <- seq_len(result$inout_result_size)
  
  linked <- data.frame(
    origin = result$out_result_origin[valid],
    linked = result$out_result_linked[valid])
  
  structure(linked, numberOfConnected = result$out_numberOfTotalLinks)
}
