#' Subset dataframe based on expression thresholds
#'
#' This function filters a data frame to include rows where the expression 
#' level exceeds a specified threshold (`min_expression`) in at least a given 
#' number of timepoints (`min_timepoints`). It allows specifying the column 
#' representing unique IDs (default is `gene_name`).
#'
#' @param data A data frame where one of the columns represents unique IDs 
#'   (e.g., genes), and other columns represent expression levels across 
#'   timepoints.
#' @param id_column A character string specifying the name of the column 
#'   containing unique IDs (default is "gene_name").
#' @param min_expression A numeric value specifying the minimum expression 
#'   level required for a timepoint to be counted.
#' @param min_timepoints A numeric value specifying the minimum number of 
#'   timepoints where the expression level must meet or exceed `min_expression`.
#'
#' @return A data frame with rows filtered based on the expression threshold 
#'   and timepoint count, retaining only the first occurrence of each unique ID.
#'
#' @importFrom dplyr mutate rowSums across filter select
#' @importFrom chk chk_data chk_number chk_lte
#' @export
subset_data <- function(data, 
                        id_column = "gene_name",
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
          !all_of(id_column),
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
    )
}


