plot_network_topology <- function(data,
                                  powers,
                                  height) {
  chk::chk_range(height, c(0.5, 1))
  
  sft = data
  par(mfrow = c(1,2));
  cex1 = 1.1;
  # Scale-free topology fit index as a function of the soft-thresholding power
  plot(
    sft$fitIndices[,1], 
    -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
    xlab="Soft Threshold (power)",
    ylab="Scale Free Topology\nModel Fit, (signed R^2)",
    type="n",
    main = paste("Scale independence")
  );
  # this line corresponds to using an R^2 cut-off of h
  abline(
    h = height,
    col="pink",
    lwd = 12,
    lty = 1
  );
  text(
    sft$fitIndices[,1], 
    -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
    labels=powers,
    cex=cex1,
    col="purple"
  );
  # Mean connectivity as a function of the soft-thresholding power
  plot(
    sft$fitIndices[,1], 
    sft$fitIndices[,5],
    xlab="Soft Threshold (power)",
    ylab="Mean Connectivity", 
    type="n",
    main = paste("Mean connectivity")
  )
  text(
    sft$fitIndices[,1], 
    sft$fitIndices[,5], 
    labels=powers, 
    cex=cex1,
    col="purple"
  )
  par(mfrow = c(1,1))
}