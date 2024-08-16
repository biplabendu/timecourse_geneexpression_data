create_modules <- function(tree, 
                           dissTOM, 
                           data = NULL,
                           min_module_size = 50,
                           merge_cutoff_similarity = NULL) {
  modules <- dynamicTreeCut::cutreeDynamic(
    dendro = tree,
    distM = dissTOM,
    method = "hybrid",
    verbose = 4,
    deepSplit = 3, 
    # see WGCNA for more info on tuning parameters
    pamRespectsDendro = FALSE,
    minClusterSize = min_module_size
  ) |> 
    WGCNA::labels2colors()
  
  writeLines("How many genes are there in each of the initial modules (clusters) detected?")
  table(modules) |> print()
  
  # Calculate eigengenes
  MEList = WGCNA::moduleEigengenes(
    datExpr, 
    colors = modules
  )
  MEs = MEList$eigengenes
  
  # Calculate dissimilarity of module eigengenes
  MEDiss = 1-cor(MEs, method = "kendall");
  
  # Cluster module eigengenes
  METree = hclust(as.dist(MEDiss), method = "average");
  # Plot the result
  plot(
    METree, 
    main = "Clustering of module eigengenes",
    xlab = "", 
    sub = "MEDiss = 1-cor(MEs, method = 'kendall')"
  )
  abline(
    h = 1 - merge_cutoff_similarity, 
    lwd = 4,
    col = "red", 
    lty = 2
  )
  writeLines("Next steps: Select a cutoff to merge similar modules.")
  
  if (!is.null(merge_cutoff_similarity) & !is.null(data)) {
    cat(
      paste(
        "Merging modules that have a correlation ≥", 
        merge_cutoff_similarity,
        "..."
      )
    )
    # Call an automatic merging function
    merged_modules <- WGCNA::mergeCloseModules(
      data, 
      modules, 
      cutHeight = 1 - merge_cutoff_similarity, 
      verbose = 0
    )
    cat("Done.\n")
    
    # Visualize the merge
      # The merged module colors
    mergedColors = merged_modules$colors;
    # # Eigengenes of the new merged modules:
    cat("Plotting the identified clusters (denoted with colors) before and after merging.")
    WGCNA::plotDendroAndColors(
      geneTree,
      cbind(modules, mergedColors),
      c("Dynamic Tree Cut", "Merged dynamic"),
      dendroLabels = FALSE, 
      hang = 0.03,
      addGuide = TRUE, 
      guideHang = 0.05
    )
    
    # return the merged modules
    merged_modules
    
  } else {
    modules
  }
  
}