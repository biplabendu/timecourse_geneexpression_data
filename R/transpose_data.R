transpose_data <- function(data) {
  
  datExpr <- as.data.frame(t(data[-1]))
  names(datExpr) <- data$gene_name
  rownames(datExpr) <- names(data)[-1]
  
  datExpr
}