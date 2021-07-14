load("~/github-repos/kwb.graph/data/kurasNetwork.RData")
x0 <- stats::setNames(kurasNetwork, c("upstream", "downstream"))
node_names <- unique(c(x0$upstream, x0$downstream))
node_ids <- kwb.utils::createIdAlong(node_names, base_name = "node")
node_ids <- gsub("^node_", "n", node_ids)

x <- x0
x$us_node_id <- node_ids[match(x$upstream, node_names)]
x$ds_node_id <- node_ids[match(x$downstream, node_names)]

exampleData <- x[, -(1:2)]
head(exampleData)

write.csv(
  exampleData, 
  file = "~/github-repos/kwb.graph/inst/extdata/exampleNetwork.csv",
  row.names = FALSE,
  quote = FALSE
)
