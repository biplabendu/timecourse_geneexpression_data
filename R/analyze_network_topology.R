analyze_network_topology <- function(data, 
                                     max_power = 21, 
                                     height = 0.9,
                                     plot = TRUE) {
  cat("Performing network topology analysis to pick 
  soft-thresholding power...\n")
  
  # Choose a set of soft-thresholding powers
  powers = c(
    c(1:10), 
    seq(
      from = 12, 
      to = if_else(max_power < 12, 12, max_power), 
      by = 3
    )
  )
  # # Call the network topology analysis function
  sft = WGCNA::pickSoftThreshold(
    data, 
    powerVector = powers, 
    verbose = 0
  )
  
  if (plot) {
    cat("\n")
    cat("Plotting resutls from the network topology analysis...")
    plot_network_topology(
      data = sft,
      powers = powers,
      height = height
    )
  }
  
  cat("Done.\n")
  cat(
    glue::glue(
      "[ NOTE, FIGURE ]: Red horizontal line indicates a signed R^2 of {height}"
    )
  )
  cat("\n\n")
  sft
}
