load_data <- function(sample_names) {
  # check inputs
  chk::chk_character(sample_names)
    
  # Step 1: split sample names
  samples <- purrr::map(
    sample_names,
    function(x) {
      c(
        strsplit(x, "-")[[1]][1],
        strsplit(x, "-")[[1]][2]
      )
    }
  )
  
  # step 2: make paths
  paths <- list()
  for (i in 1:length(samples)) {
    paths[[i]] <- glue::glue(
      "tissue_data/{samples[[i]][1]}/{samples[[i]][2]}/{samples[[i]][2]}.txt"
    ) 
  }
  
  # step 3: load and tidy data
  data.db <- list()
  for (i in 1:length(paths)) {
    data.db[[i]] <- read.csv(
      here::here(paths[[i]]),
      sep = "\t",
      header = TRUE,
      na.strings = c("", " ", "na", "NA", "Na")
    ) |> 
      as_tibble() |> 
      rename(
        gene_name = ID
      )
  }
  
  # step 4: set names
  data.db |> setNames(sample.names)
  
}
