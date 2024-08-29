check_sample_quality <- function(data) {
  # USE THE FOLLOWING CODE TO CHECK IF YOU HAVE ANY BAD SAMPLES #
  gsg = WGCNA::goodSamplesGenes(data, verbose = 3);
  if(gsg$allOK == TRUE) {
    cat("All okay!")
  }
}