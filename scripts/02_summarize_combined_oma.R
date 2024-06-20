library(dplyr)


# read file ---------------------------------------------------------------

dat <- read.csv(
  "./result/combined_oma_v1.csv",
  na.strings = c("", " ")
) |> 
  as_tibble()


# summary 1: different combinations ---------------------------------------

dat |> 
  # mutate_all(as.factor) |> summary()
  mutate(
    across(
      everything(),
      ~ if_else(
        !is.na(.),
        "present",
        "x"
      )
    )
  ) |> 
  group_by(
    across(
      everything()
    )
  ) |> 
  tally(
    name = "n_genes"
  ) |> 
  arrange(
    desc(n_genes)
  ) |> 
  # filter(
  #   n_genes > 100
  # ) |> 
  View()


# summary 2: present in all -----------------------------------------------

sub <- dat |> 
  filter(
    if_all(
      everything(),
      ~ !is.na(.)
    )
  )

# For how many mouse genes, there are ≥ 2 genes in at least one more species?
sub |> 
  group_by(
    mouse_ID
  ) |> 
  tally() |> 
  mutate(
    n = if_else(
      n == 1,
      "1:1",
      "1:n possible"
    )
  ) |> 
  group_by(
    n
  ) |> 
  tally() |> 
  rename(
    orthology = n,
    n_genes = nn
  ) |> 
  View()
