#' @description
#' Log-2 transform the data
#' 
#' @param data a single dataframe
#' @param sample name of the dataframe to log-transform
#' 
#' @return a single dataframe with log2-transformed values.
#' 
#' @export
log2_transform_data <- function(data, 
                                id_column,
                                log2 = TRUE) {
  chk::chk_data(data)
  chk::chk_logical(log2)
  
  if (log2) {
    cat("Applying log2-transformation...")
    chk::chk_character(id_column)
    chk::check_names(
      data,
      id_column
    )
    
    out <- data |> 
      mutate(
        across(
          !all_of(id_column),
          ~ log2(.x + 1)
        )
      )
    cat("Done.")
    cat("\n")
  }
  
  out
}
