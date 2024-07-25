make_database_expression <- function (data, 
                                      path_to_csv = NULL, 
                                      header = TRUE, timepoints, 
                                      expressed_timepoints_min = 1, cycle = "LD", db_name = "database") 
{
  library(dplyr)
  if (!dir.exists(here::here(".databases/"))) {
    dir.create(here::here(".databases/"))
    cat("Created .databases/ in project root.")
  }
  db_path <- glue::glue(".databases/{db_name}.db")
  my.db <- RSQLite::dbConnect(RSQLite::SQLite(), here::here(db_path))
  tbl_names <- paste(db_name, c("expression", "expressed_genes", 
                                "log2expression", "zscores"), sep = "_")
  if (length(RSQLite::dbListTables(my.db)) == 0) {
    if (!is.null(path_to_csv)) {
      data <- as_tibble(read.csv(path_to_csv, header = header, 
                                 stringsAsFactors = FALSE, na.strings = c(NA, 
                                                                          "", " ")))
    }
    RSQLite::dbWriteTable(my.db, tbl_names[1], data)
    cat("Table (", tbl_names[1], ") has been added to", 
        db_path, "\n")
    expressed <- select(mutate(mutate(data, n = rowSums(across(matches("^ZT|^CT"), 
                                                               ~.x > 1), na.rm = TRUE), across(matches("^ZT|^CT"), 
                                                                                               ~round(.x, 1))), expressed = if_else(n >= expressed_timepoints_min, 
                                                                                                                                    "yes", "no")), gene_name, expressed)
    RSQLite::dbWriteTable(my.db, tbl_names[2], expressed)
    cat("Table (", tbl_names[2], ") has been added to", 
        db_path, "\n")
    log2.data <- mutate(data, across(matches("^ZT|^CT"), 
                                     ~log2(.x + 1)))
    RSQLite::dbWriteTable(my.db, tbl_names[3], log2.data)
    cat("Table (", tbl_names[3], ") has been added to", 
        db_path, "\n")
    zscores.data <- tidyr::pivot_wider(ungroup(mutate(group_by(tidyr::pivot_longer(log2.data, 
                                                                                   cols = matches("^ZT|^CT"), names_to = "time", values_to = "exp"), 
                                                               gene_name), exp = scale(exp)[, 1])), names_from = "time", 
                                       values_from = "exp")
    RSQLite::dbWriteTable(my.db, tbl_names[4], zscores.data)
    cat("Table (", tbl_names[4], ") has been added to", 
        db_path, "\n")
  }
  else {
    cat("Database exists. Returning link to the database.")
  }
  my.db
}
