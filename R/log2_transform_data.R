log2_transform_data <- function(data_list, sample) {
  
  # Print the selected dataset
  writeLines(paste0("Selected Dataset: ", sample))
  
  # Extract and process the data
  dat <- data_list %>%
    purrr::pluck(sample) %>%
    mutate(
      across(
        !gene_name,
        ~ log2(.x + 1)
      )
    ) %>%
    select(gene_name, everything()) %>%
    collect()
  
  
  # writeLines(
  #   "What are the dimensions of the original dataset?
  #    [Rows = #genes, Cols = #samples]"
  # )
  # 
  # dim(dat[-1]) %>% print()
  
  dat
}
