create_modules <- function(tree, 
                           dissTOM, 
                           min_module_size,
                           verbose = 0,
                           data,
                           merge_cutoff_similarity = 0.9) {
  modules <- dynamicTreeCut::cutreeDynamic(
    dendro = tree,
    distM = dissTOM,
    method = "hybrid",
    verbose = verbose,
    deepSplit = 3, 
    # see WGCNA for more info on tuning parameters
    pamRespectsDendro = FALSE,
    minClusterSize = min_module_size
  ) |> 
    WGCNA::labels2colors()
  
  # Calculate eigengenes
  MEList = WGCNA::moduleEigengenes(
    data, 
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
    main = "Initial classification into modules",
    xlab = glue::glue(
      "Red horizontal line shows modules that are ≥ 
        {merge_cutoff_similarity} similar."
    ),
    ylab = "Dissimilarity (height)",
    ylim = c(0, 0.9),
    sub = "MEDiss = 1-cor(MEs, method = 'kendall')"
  )
  abline(
    h = 1 - merge_cutoff_similarity, 
    lwd = 4,
    col = "red", 
    lty = 2
  )
  
  if (!is.null(merge_cutoff_similarity)) {
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
    cat(
      "[ NOTE, FIGURE ] Plotting identified clusters before and after merging."
    )
    cat("\n\n")
    WGCNA::plotDendroAndColors(
      geneTree,
      cbind(modules, mergedColors),
      main = "Cluster Dendogram (Dynamic Tree Cut)",
      c("Inital classification", "Merged dynamic"),
      dendroLabels = FALSE, 
      hang = 0.03,
      addGuide = TRUE, 
      guideHang = 0.05
    )
    
    writeLines("Module (cluster) size:")
    table(mergedColors) |> print()
    
    # return the merged modules
    mergedColors
    
  } else {
    writeLines("Initial classification into modules (clusters):")
    table(modules) |> print()
    
    modules
  }
  
}