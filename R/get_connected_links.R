# getConnectedLinks ------------------------------------------------------------

#' Get Connected Links
#'
#' @param x data frame with each row representing a link of the network.
#'   Required columns: \emph{us_node_id}, \emph{ds_node_id}
#' @param upstream if TRUE (upstream), if FALSE (downstream), default: TRUE
#' @param version 1: R-implementation, 2: C-implementation, (default: 1)
#' @param dbg default: FALSE
#' @param ... additional arguments passed to \code{\link{getConnectedLinks.C}}
#' @return get connected links
#' @importFrom kwb.utils catAndRun
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
getConnectedLinks <- function(x, upstream = TRUE, version = 1, dbg = FALSE, ...)
{
  if (version == 1) {
    return(getConnectedLinks.R(
      directly.connected = getDirectLinks.R(x, upstream), 
      dbg = dbg
    ))
  } 

  # Helper function to print a debug message and run a piece of code
  run <- function(msg, expr) kwb.utils::catAndRun(msg, expr, dbg = dbg)
  
  run("Getting connected links", getConnectedLinks.C(
    directLinks = run("Getting direct links", getDirectLinks.C(x)),
    version = version - 1, 
    dbg = dbg, 
    ...
  ))
}

# getDirectLinks.R -------------------------------------------------------------

#' Get Direct Links (R Implementation)
#'
#' @param x data frame with each row representing a link of the network. Required
#' columns: \emph{us_node_id}, \emph{ds_node_id} 
#' @param upstream if TRUE (upstream), if FALSE (downstream), default: TRUE
#' @return list with as many elements as there are rows in \code{x}
#' @export
getDirectLinks.R <- function(x, upstream = TRUE) 
{  
  # For each link (each row in x), find the row indices of directly (upstream
  # or downstream) connected links
  us <- "us_node_id"
  ds <- "ds_node_id"
  
  lapply(seq_len(nrow(x)), function(i) which(`==`(
    x[i, ifelse(upstream, us, ds)],
    x[ , ifelse(upstream, ds, us)]
  )))
}

# getDirectLinks.C -------------------------------------------------------------

#' Get Direct Links (C Implementation)
#'
#' @param x data frame with each row representing a link of the network.
#'   Required columns: \emph{us_node_id}, \emph{ds_node_id}
#' @param MAX_DIRECT_CONNECTIONS default: 5
#' @param dbg default: FALSE
#' @return get direct links with C implementation
#' @importFrom kwb.utils createAccessor
#' @export
getDirectLinks.C <- function(x, MAX_DIRECT_CONNECTIONS = 5, dbg = FALSE)
{
  n <- nrow(x)

  # Function to access columns of x
  get <- kwb.utils::createAccessor(x)
  
  # Function to access elements of list returned by "C_getDirectLinks"
  get <- kwb.utils::createAccessor(.C(
    "C_getDirectLinks", 
    length = n, 
    upstreamNodes = get("us_node_id"),
    downstreamNodes = get("ds_node_id"),
    numberOfFound = integer(n),
    indicesOfFound = integer(MAX_DIRECT_CONNECTIONS * n),
    maxNumber = as.integer(MAX_DIRECT_CONNECTIONS),
    dbg = as.integer(dbg)
    #,    PACKAGE = "kwb.graph"    
  ))
  
  n_found <- get("numberOfFound")

  if (max(n_found) > MAX_DIRECT_CONNECTIONS) {
    stop("It seems that you need to increase MAX_DIRECT_CONNECTIONS!")
  }
  
  M <- matrix(get("indicesOfFound"), ncol = MAX_DIRECT_CONNECTIONS)
  
  lapply(seq_len(n), function(i) M[i, seq_len(n_found[i])])
}

# getConnectedLinks.R ----------------------------------------------------------

#' Get Connected Links (R Implementation)
#'
#' @param directly.connected directly.connected
#' @param dbg default: FALSE
#' @return get connected links with R implementation
#' @export
#' @importFrom kwb.utils catIf
getConnectedLinks.R <- function(directly.connected, dbg = FALSE)
{
  # Initialise result list
  all.connected <- list()
  
  n <- length(directly.connected)
  
  blocksize <- max(1, n %/% 80)
  
  # Loop through list of direct links
  for (i in seq_len(n)) {
    
    kwb.utils::catIf(dbg && i %% blocksize == 0, ".")
    
    # Initialise vector of indices of connected links
    all.connected[[i]] <- integer(0)
    
    # Initialise boolean vector holding TRUE at indices representing the links
    # that are connected directly or indirectly to the current link i
    is.connected <- logical(length = n)
    
    # Start queue of indices of connected links with indices of directly
    # connected links
    queue <- directly.connected[[i]] 
    
    while (length(queue) > 0) {
      
      # Get and remove next index from queue
      link.index <- queue[1L]
      queue <- queue[-1L]
      
      is.connected[link.index] <- TRUE
      
      if (link.index < i) {
        
        # If link with index link.index was already considered (before the
        # current link i) save full information on links that are connected to
        # the link with index link.index in the logical vector is.connected.
        is.connected[all.connected[[link.index]]] <- TRUE
        
      } else {
        
        # Add indices of directly connected links to the queue
        queue <- c(queue, directly.connected[[link.index]])
      }
      
    } # End of while
    
    # Save indices of connected links
    all.connected[[i]] <- which(is.connected)
  }
  
  kwb.utils::catIf(dbg, "<\n")
  
  all.connected
}

# getConnectedLinks.C ----------------------------------------------------------

#' Get Connected Links (C Implementation)
#'
#' @param directLinks directLinks 
#' @param resultSize default: 60000
#' @param queueSize default: 1024*1024
#' @param version version of C implementation: 1,2 or 3 (default: 1)
#' @param dbg default: FALSE
#' @return data.frame with columns \code{origin}, \code{linked} and attribute
#'   \code{numberOfConnected}
#' @importFrom kwb.utils catIf
#' @export
getConnectedLinks.C <- function(
  directLinks, resultSize = 60000, queueSize = 1024*1024, version = 1,
  dbg = FALSE
)
{
  n <- length(directLinks)
  
  # Number of directly connected links for each link
  n_links <- lengths(directLinks)
  
  # Matrix with each row representing a link. Column i contains the ID of the 
  # i-th directly connected link. If there is no such link the matrix element is 
  # -1.
  i_direct <- matrix(-1L, nrow = n, ncol = max(n_links))
  
  links.matrix <- t(sapply(seq_len(n), FUN = function(i) {
    n.links <- n_links[[i]]
    onerow <- i_direct[i, ]
    if (n.links > 0L) {
      onerow[seq_len(n.links)] <- as.integer(directLinks[[i]])
    }
    onerow
  }))    
  
  kwb.utils::catIf(dbg, "\ngetConnectedLinks.C:\n", sprintf(
    "Calling C_getConnectedLinks(%d, queueSize = %d, dbg = %d, version = %d)\n", 
    n, queueSize, dbg, version
  ))
  
  fetch <- kwb.utils::createAccessor(.C(
    "C_getConnectedLinks", 
    in_n = as.integer(n), 
    in_numberOfDirectLinks = as.integer(n_links),
    # In C, a one-dimensional array is used, created by putting the columns one
    # after another
    in_indicesOfDirectLinks = c(links.matrix),
    out_numberOfTotalLinks = as.integer(rep(0, n)),
    inout_result_size = as.integer(resultSize),
    out_result_origin = as.integer(rep(0, resultSize)),
    out_result_linked = as.integer(rep(0, resultSize)),
    in_queueSize = as.integer(queueSize),
    in_dbg = as.integer(dbg),
    in_version = as.integer(version)
    #, PACKAGE = "kwb.graph"
  ))

  kwb.utils::catIf(dbg, "\nBack in R from C_getConnectedLinks.\n")

  valid <- seq_len(fetch("inout_result_size"))
  
  structure(
    data.frame(
      origin = fetch("out_result_origin")[valid],
      linked = fetch("out_result_linked")[valid]
    ), 
    numberOfConnected = fetch("out_numberOfTotalLinks")
  )
}
