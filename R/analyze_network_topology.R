#' @title 
#' Perform network topology analysis on the data
#' 
#' @param data a single dataframe where columns are gene names(feature) and rows
#' are time points
#' 
#' @return a list and a plot. a list of two components: 1.$powerEstimate (an 
#' integer that is a recommended power) 2. $fitIndices (a dataframe that shows
#' scale free topology model fit). The plot visualizes the Scale Independence 
#' and Mean connectivity from the $fitIndices element of the list.
#' 
#'
#' 
analyze_network_topology <- function(data) {
  
  cat("Performing network topology analysis to pick 
  soft-thresholding power...\n")
  
  # Choose a set of soft-thresholding powers
  powers = c(c(1:10), seq(from = 12, to=30, by=2))
  # # Call the network topology analysis function
  sft = WGCNA::pickSoftThreshold(
    data, 
    powerVector = powers, 
    verbose = 0
  )
  
  cat("Plotting the resutls from the network topology analysis...")
  plot_network_topology(
    data = sft,
    powers = powers,
    height = 0.90
  )
  cat("Done.\n")
  
  sft
}