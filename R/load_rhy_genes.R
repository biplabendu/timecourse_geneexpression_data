#' @keywords internal
load_rhy_genes <- function(sample) {
  # check inputs
  chk::chk_scalar(sample)
  chk::chk_character(sample)
  
  # Step 1: split sample names
  samples <- purrr::map(
    sample,
    function(x) {
      c(
        strsplit(x, "-")[[1]][1],
        strsplit(x, "-")[[1]][2]
      )
    }
  ) |> 
    unlist()
  
  filenames <- c(
    "ARS",
    "empJTK",
    "GeneCycle",
    "JTK",
    "LS",
    "meta2d",
    "RAIN"
  )
  
  # step 2: make paths
  paths <- glue::glue(
    "tissue_data/{samples[1]}/{samples[2]}/{filenames}.txt"
  )
  
  # step 3: load and tidy data
  rhy.db <- list()
  for (i in 1:length(paths)) {
    rhy.db[[i]] <- read.csv(
      here::here(paths[[i]]),
      sep = "\t",
      header = TRUE,
      na.strings = c("", " ", "na", "NA", "Na")
    ) |> 
      as_tibble()
  }
  
  # step 4: set names
  rhy.db |> setNames(filenames)
  
}