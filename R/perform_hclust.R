perform_hclust <- function(data, plot = FALSE) {
  
  tree <- hclust(
    as.dist(data), 
    method = "average"
  )
  
  if (plot) {
    # cat("Plotting the resulting clustering tree (dendrogram)")
    plot(
      tree, 
      xlab="", 
      sub="", 
      main = "Hierarchical clustering (method = 'average')\non TOM-based dissimilarity (dissTOM)",
      labels = FALSE, 
      hang = 0.04
    )
  }
  
  tree
}