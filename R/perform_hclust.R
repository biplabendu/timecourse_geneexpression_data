perform_hclust <- function(data) {
  
  tree <- hclust(as.dist(data), method = "average")
  
  writeLines("Plotting the resulting clustering tree (dendrogram)")
  plot(
    tree, 
    xlab="", 
    sub="", 
    main = "Gene clustering\non TOM-based dissimilarity (dissTOM)",
    labels = FALSE, 
    hang = 0.04
  )
  
  tree
}