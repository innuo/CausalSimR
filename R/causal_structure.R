#' @export
CausalStructure <- R6::R6Class("CausalStructure", list(
  dataset = NULL,
  options = NULL,
  causal.graph = NULL,
  vars.topo.sorted = NULL,
  parents = NULL,

  initialize = function(dataset, options=NULL) {
    self$dataset <- dataset
    self$options <- options
  },

  learn_structure = function(){
    bn = bnlearn::tabu(self$dataset$data)
    self$make_structure(bn$arcs)
  },

  make_structure = function(edges){
    g <- igraph::make_empty_graph(n = self$dataset$ncols, directed=TRUE)
    g <- igraph::set_vertex_attr(g, "name", index=igraph::V(g), self$dataset$col.names.to.model)
    g <- igraph::add_edges(g, array(t(as.matrix(edges))))
    self$causal.graph <- g

    self$vars.topo.sorted <- names(igraph::topo_sort(self$causal.graph))
    self$parents <- list()
    for(v in self$dataset$col.names.to.model){
      self$parents[[v]] <- names(igraph::neighbors(self$causal.graph, v, mode = "in"))
    }
  },

  to_list = function(){
    graph.list <- list()
    graph.list$nodes <- names(igraph::V(self$causal.graph))
    edges <- data.frame(igraph::as_edgelist(self$causal.graph))
    names(edges) <- c("from", "to")
    graph.list$edges <- edges
    graph.list
  },

  from_list = function(graph.list){
    edges <- graph.list$edges
    self$make_structure(edges)
  }



  )
)
