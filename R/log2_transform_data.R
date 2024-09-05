#' @description
#' Log-2 transform the data
#' 
#' @param data a single dataframe
#' @param sample name of the dataframe to log-transform
#' 
#' @return a single dataframe with log2-transformed values.
#' 
#' @export
log2_transform_data <- function(data, id_column = "gene_name") {
  chk::chk_data(data)
  chk::chk_character(id_column)
  chk::check_names(
    data,
    id_column
  )
  
  data %>%
    mutate(
      across(
        !all_of(id_column),
        ~ log2(.x + 1)
      )
    )
}
