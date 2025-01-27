calculate_gene_gene_sim <- function(data, 
                                    method = "kendall", 
                                    name = NULL, 
                                    overwrite = FALSE,
                                    cache = FALSE,
                                    ...) {
  chk::chk_data(data)
  chk::chk_logical(overwrite)
  chk::chk_logical(cache)
  
  if(cache == FALSE) {
    cat("Running gene-gene similarity...")
    sim_matrix <- cor(
      datExpr, 
      method = method,
      ...
    ) 
    cat("Done.")
    cat("\n")
  } else {
    chk::chk_scalar(name)
    
    # set up tmp workspace
    path = ".databases/.tmp/"
    sim_matrix_path = glue::glue(
      "{path}/01_sim_matrix_{name}.RDS"
    )
    
    if (!dir.exists(here::here(path))) {
      cat("Creating '.databases/.tmp/' for saving cache...")
      dir.create(here::here(path), recursive = TRUE)
      # this step takes time
      cat("Done.")
      cat("\n")
    }
    
    if (!file.exists(sim_matrix_path) | overwrite == TRUE) {
      cat("Running gene-gene similarity...")
      sim_matrix <- cor(
        datExpr, 
        method = method,
        ...
      ) 
      cat("Done.")
      cat("\n")
      
      cat("Saving gene-gene similarity matrix...")
      saveRDS(
        sim_matrix,
        file = here::here(
          sim_matrix_path
        )
      )
      cat("Done.")
      cat("\n")
    } else {
      cat("Loading gene-gene similarity matrix from cache...")
      sim_matrix <- readRDS(
        file = here::here(
          sim_matrix_path
        )
      )
      cat("Done.")
    }
  }
  
  
  
  sim_matrix
}
