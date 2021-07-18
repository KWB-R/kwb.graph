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
      directLinks = getDirectLinks.R(x, upstream), 
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
#' @param directLinks direct links
#' @param dbg default: FALSE
#' @return get connected links with R implementation
#' @export
#' @importFrom kwb.utils catIf
getConnectedLinks.R <- function(directLinks, dbg = FALSE)
{
  # Initialise result list
  result <- list()
  
  n <- length(directLinks)
  
  blocksize <- max(1, n %/% 80)
  
  # Loop through list of direct links
  for (i in seq_len(n)) {
    
    kwb.utils::catIf(dbg && i %% blocksize == 0, ".")
    
    # Initialise vector of indices of connected links
    result[[i]] <- integer(0L)
    
    # Initialise boolean vector holding TRUE at indices representing the links
    # that are connected directly or indirectly to the current link i
    is_linked <- logical(n)
    
    # Start queue of indices of connected links with indices of directly
    # connected links
    queue <- directLinks[[i]] 
    
    while (length(queue)) {
      
      # Get and remove next index from queue
      index <- queue[ 1L]
      queue <- queue[-1L]
      
      is_linked[index] <- TRUE
      
      # If link "index" was already considered (before the current link "i") 
      if (index < i) {
        
        # Save information on links that are connected to link "index" in
        # logical vector "is_linked".
        is_linked[result[[index]]] <- TRUE
        
      } else {
        
        # Add indices of directly connected links to the queue
        queue <- c(queue, directLinks[[index]])
      }
      
    } # End of while
    
    # Save indices of connected links
    result[[i]] <- which(is_linked)
  }
  
  kwb.utils::catIf(dbg, "<\n")
  
  result
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
  n_direct <- length(directLinks)
  
  # Number of directly connected links for each link
  n_links <- lengths(directLinks)
  
  # Matrix with each row representing a link. Column i contains the ID of the 
  # i-th directly connected link. If there is no such link the matrix element is 
  # -1.
  i_direct <- matrix(-1L, nrow = n_direct, ncol = max(n_links))
  
  link_matrix <- t(sapply(seq_len(n_direct), function(i) {
    y <- i_direct[i, ]
    if (n <- n_links[[i]]) {
      y[seq_len(n)] <- as.integer(directLinks[[i]])
    }
    y
  }))
  
  kwb.utils::catIf(dbg, "\ngetConnectedLinks.C:\n", sprintf(
    "Calling C_getConnectedLinks(%d, queueSize = %d, dbg = %d, version = %d)\n", 
    n_direct, queueSize, dbg, version
  ))
  
  fetch <- kwb.utils::createAccessor(.C(
    "C_getConnectedLinks", 
    in_n = n_direct, 
    in_numberOfDirectLinks = n_links,
    # In C, a one-dimensional array is used, created by putting the columns one
    # after another
    in_indicesOfDirectLinks = c(link_matrix),
    out_numberOfTotalLinks = integer(n_direct),
    inout_result_size = as.integer(resultSize),
    out_result_origin = integer(resultSize),
    out_result_linked = integer(resultSize),
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
