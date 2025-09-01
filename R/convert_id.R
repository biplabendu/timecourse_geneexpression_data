#' @internal
#' 
convert_id <- function(data, 
                       id_col = "gene_name", 
                       from, 
                       to) {
  dat <- read.csv(
    here::here(
      "./result/combined_oma_v2.csv"
    ),
    na.strings = c("", " ", "NA")
  ) |> 
    select(
      - human_name
    ) |> 
    tidyr::separate_longer_delim(
      cflo_ID,
      ", "
    ) |> 
    as_tibble()
  
  out <- dat |> 
    select(
      matches(
        paste(
          c(from, to),
          collapse = "|"
        )
      )
    ) |> 
    select(
      !!id_col := .data[[glue::glue("{from}_ID")]],
      everything()
    )
  
  # cat(
  #   "number of", 
  #   toupper(from), 
  #   "genes not found in", 
  #   toupper(to),
  #   ":",
  #   out |> 
  #     filter(
  #       if_all(matches(to), ~ is.na(.x))
  #     ) |> 
  #     select(matches(from)) |> 
  #     pull(1) |> 
  #     unique() |> 
  #     length()
  # )
  
  data |> 
    left_join(
      out,
      join_by(!!id_col)
    )
}
