plot_adj_as_network <- function(matrix, 
                                min_edge = 0.4, 
                                node_size = 45, 
                                node_fill_col = "yellow",
                                node_frame_col = NULL,
                                node_label_size = 1,
                                edge_size = 2,
                                layout = igraph::layout_in_circle,
                                ...) {
  set.seed(420)
  # get rid of low correlations 
  matrix[matrix < min_edge] <- 0
  
  # build_network
  network <- igraph::graph_from_adjacency_matrix(
    matrix,
    mode = "upper",
    weighted = T,
    diag = F
  )
  
  igraph::V(network)$color <- node_fill_col
  
  igraph::V(network)$size <- node_size
  # igraph::V(network)$size <- igraph::degree(network, mode = "total")*node_size
  
  if (!is.null(node_label_size) & node_label_size > 0) {
    igraph::V(network)$label.color <- "black"
    igraph::V(network)$label.cex <- node_label_size
  }
  
  if (!is.null(node_frame_col)) {
    igraph::V(network)$frame.color <- node_frame_col 
  }
  
  igraph::E(network)$width <- igraph::E(network)$weight*edge_size
  igraph::E(network)$color <- "black"
  
  writeLines("Visualizing a simplified representation of the circadian GCN")
  par(mfrow = c(1,1))
  plot(
    network,
    # size=100,
    # layout = igraph::layout.kamada.kawai,
    # layout = igraph::layout.fruchterman.reingold,
    # layout = igraph::layout.graphopt,
    layout = layout,
    # vertex.label=NA
    # vertex.shape="none"
    ...
  )
  
}