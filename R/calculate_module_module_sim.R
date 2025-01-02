calculate_module_module_sim <- function(merged_modules) {
  
  cat("Calculating module-module similarity based 
  on module-eigengene-expression...")
  # Calculate similarity of the eigen-genes
  sim_matrix_ME <- cor(
    merged_modules$newMEs, 
    method = "kendall"
  )
  # calculate adj_matrix
  adj_matrix_ME <- WGCNA::adjacency.fromSimilarity(
    sim_matrix_ME,
    power=1,        # DO NOT power transform
    type='signed'
  ) |> 
    as.matrix()
  cat("Done.\n")
  
  cat("Tidying module names...")
  ## CHANGE THE NAMES OF THE MODULES;
  module_ids <- data.frame(
    old_labels = rownames(adj_matrix_ME) %>% 
      stringr::str_split("ME", 2) %>% 
      sapply("[", 2) %>% 
      as.character(),
    new_labels = paste0(
      "C", 
      1:nrow(adj_matrix_ME)
    )
  )
  rownames(adj_matrix_ME) <- module_ids$new_labels
  colnames(adj_matrix_ME) <- module_ids$new_labels
  cat("Done.\n")
  
  if (plot) {
    cat("Plotting adjacency matrix for module-module similarity...")
    gplots::heatmap.2(
      t(adj_matrix_ME),
      col = viridis::inferno(100),
      # labRow = NA, 
      # labCol = NA,
      trace = 'none', 
      dendrogram = 'row',
      xlab = '', 
      ylab = '',
      main = 'Adjacency matrix - MEs \n(module-module similarity)',
      density.info = 'none', 
      revC = TRUE
    )
    cat("Done.\n")
  }
  
  list(
    "ME" = adj_matrix_ME,
    "mapping_tbl" = module_ids
  )
  
}