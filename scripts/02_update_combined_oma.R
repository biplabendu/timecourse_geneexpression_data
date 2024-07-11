library(dplyr)
library(dbplyr)


# read file ---------------------------------------------------------------

dat <- read.csv(
  "./result/combined_oma_v1.csv",
  na.strings = c("", " ")
) |> 
  as_tibble()


# load orthology database for ants ----------------------------------------

db <- RSQLite::dbConnect(
  RSQLite::SQLite(), 
  "./data/databases/blast_data.db"
)

# extract the necessary data
human_cflo <-
  db %>%
  tbl(., "human") %>%
  collect() |> 
  select(
    cflo_ID = cflo_gene,
    human_ID = human_gene,
    human_name
  ) |> 
  group_by(
    human_ID
  ) |> 
  reframe(
    cflo_ID = paste(cflo_ID |> unique(), collapse = ", "),
    human_name = paste(human_name |> unique(), collapse = ", ")
  ) |> 
  distinct()



# Concat with combined oma file -------------------------------------------

dat |> 
  left_join(
    human_cflo,
    join_by(
      human_ID
    )
  ) |> 
  relocate(
    human_name,
    human_ID,
    .before = 1
  ) |> 
  write.csv(
    "./result/combined_oma_v2.csv",
    row.names = FALSE
  )
