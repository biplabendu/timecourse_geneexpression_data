subset_data <- function(data, 
                        min_expression, 
                        min_timepoints) {
  
  chk::chk_data(data)
  chk::chk_number(min_expression)
  chk::chk_number(min_timepoints)
  chk::chk_lte(
    min_timepoints,
    ncol(data) - 1,
  )
  
  data |> 
    mutate(
      n_samples = rowSums(
        across(
          !gene_name,
          ~ .x >= min_expression
        ),
        na.rm = TRUE
      )
    ) |> 
      filter(
        n_samples >= min_timepoints
      ) |> 
      select(
        - n_samples
      ) |> 
      # TEMP FIX ----
    # Multiple rows per gene
    # fix: only taking the first row
    # might not be the best way
    group_by(gene_name) |> 
      slice(1)
}


