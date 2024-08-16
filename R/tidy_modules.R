tidy_modules <- function(merged_modules, mapping_tbl, data) {
  
  # Make a list that returns gene names for a given cluster
  module_genes <- list()
  
  # modules.to.exclude <- c(paste0("C",c(2,5,6,7,10:17,19)))
  modules.to.exclude <- c("")
  which.modules <- mapping_tbl %>% 
    filter(!new_labels %in% modules.to.exclude) %>%
    pull(old_labels)
  which.labels <- mapping_tbl %>% 
    filter(!new_labels %in% modules.to.exclude) %>%
    pull(new_labels)
  
  # Get the genes from each of the modules
  for (i in 1:length(which.modules)) {
    # which color
    mod.color = as.character(which.modules[[i]])
    # subset
    module_genes[[i]] <- names(data)[
      which(merged_modules == mod.color)
    ]
    names(module_genes)[[i]] <- as.character(
      which.labels[[i]]
    )
  }
  
  # # check the result | works
  # names(module_genes)
  # module_genes['C22']
  
  # [13 Dec 2021]
  # Save a csv with the module identity information for all genes used in building the GCN
  # make a dataframe with gene_name and module_identity
  for (i in 1:length(module_genes)){
    if (i == 1){
      dat_module_gene <- data.frame(
        gene_name = module_genes[[i]],
        module_identity = as.character(names(module_genes)[i])
      )
    }
    else{
      foo <- data.frame(
        gene_name = module_genes[[i]],
        module_identity = as.character(names(module_genes)[i])
      )
      dat_module_gene <- rbind(dat_module_gene, foo) 
    }
    
  }
  
  dat_module_gene |> 
    as_tibble() |> 
    left_join(
      mapping_tbl,
      join_by(module_identity == new_labels)
    )
}