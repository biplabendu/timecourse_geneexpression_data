source("renv/activate.R")

go <- function() {
  library(dplyr)
  library(dbplyr)
  library(ggplot2)
  for (i in list.files(here::here("R"), full.names = TRUE)) {
    source(i)
  }
}
