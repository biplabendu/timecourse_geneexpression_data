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