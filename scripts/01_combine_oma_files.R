library(dplyr)

files <- list.files(
  "./ORTHOLOGY/oma/",
  full.names = FALSE
)

for (file in files) {
  # file <- "aedes_anopheles_oma.txt"
  # file <- "mouse_drosophila_oma.txt"
  # getting the name of the comparison
  species <- strsplit(file, "_oma")[[1]][1]
  # read the file
  foo <- read.csv(
    paste0(
      "./ORTHOLOGY/oma/",
      file
    ),
    sep = "\t"
  )
  
  # change the column names for orthotype and omagroup
  if (dim(foo)[2] == 4) {
    colnames(foo)[3] <- paste0(
      colnames(foo)[3],
      "_",
      species
    )
    colnames(foo)[4] <- paste0(
      "omagroup_",
      species
    )
  }
  
  
  # any specific transformation / clean up 
  if (species == "mouse_drosophila") {
    foo <- foo |> 
      rowwise() |> 
      mutate(
        mouse_ID = strsplit(mouse_ID, "\\.")[[1]][1]
      ) |> 
      ungroup()
  }
  
  # saving the file 
  write.csv(
    foo,
    paste0(
      "./ORTHOLOGY/oma_csv/",
      species,
      "_oma.csv"
    ),
    row.names = FALSE
  )
}


# Combine files -----------------------------------------------------------

# list all the CSVs
files_new <- list.files(
  "./ORTHOLOGY/oma_csv/"
)




