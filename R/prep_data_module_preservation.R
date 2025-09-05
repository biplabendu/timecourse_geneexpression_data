#' @keywords internal
prep_data_module_preservation <- function (data, ref, test) {
  # Form multi-set expression data: columns starting from 9 contain actual expression data.
  multiExpr = vector(mode = "list", length = nSets)
  
  multiExpr[[1]] = list(data = as.data.frame(t(log2(data[[ref]][-c(1)]+1))))
  names(multiExpr[[1]]$data) = data[[ref]]$gene_name;
  rownames(multiExpr[[1]]$data) = names(data[[ref]])[-c(1)];
  
  multiExpr[[2]] = list(data = as.data.frame(t(log2(data[[test]][-c(1)]+1))));
  names(multiExpr[[2]]$data) = data[[test]]$gene_name;
  rownames(multiExpr[[2]]$data) = names(data[[test]])[-c(1)];
  
  multiExpr |> 
    setNames(c(ref, test))
  
}
