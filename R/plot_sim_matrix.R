plot_sim_matrix <- function(matrix, n=200) {
  ## Let's display a chunk of the matrix (code from Hughitt 2016; 
  # github)
  heatmap_indices <- sample(nrow(matrix), n)
  
  writeLines(paste0("Plotting a chunk of the gene-gene similarity matrix with ", length(heatmap_indices), " genes."))
  gplots::heatmap.2(
    t(matrix[heatmap_indices, heatmap_indices]),
    col=viridis::inferno(100),
    labRow=NA, 
    labCol=NA,
    trace='none', 
    dendrogram='row',
    xlab='Gene', 
    ylab='Gene',
    main= paste0(
      "Similarity matrix \nmethod = 'kendall' \n (", 
      length(heatmap_indices), 
      " random genes)"
    ),
    density.info='none', 
    revC=TRUE
  )
}