plot_sample_expression <- function(data) {
  
  data = datExpr
  
  # save the number of genes and samples
  # that will be used to create the circadian GCN
  nGenes = ncol(data)
  nSamples = nrow(data)
  
  writeLines("Visualizing the log-transformed data")
  
  data |> 
    tibble::rownames_to_column(
      "sample"
    ) |> 
    as_tibble() |> 
    tidyr::pivot_longer(
      cols = !sample,
      names_to = "gene_id",
      values_to = "log2_fpkm"
    ) |> 
    mutate(
      sample = stringr::str_remove(sample, "ZT")
    ) |> 
    ggplot(
      aes(
        x = log2_fpkm, 
        # color = sample,
        fill = sample
      )
    ) + 
    geom_density(
      position = "stack"
    ) + 
    theme_bw(20) +
    scale_fill_manual(
      values = viridis::viridis(nSamples)
    ) +
    labs(
      x = "Expression (log2)"
    ) +
    theme(
      legend.position = "bottom",
      legend.justification = "right"
    ) +
    guides(
      fill = guide_legend(
        nrow = 3,
        byrow=TRUE
      )
    )
}