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
    bn = bnlearn::mmhc(self$dataset$data)
    g <- igraph::make_empty_graph(n = self$dataset$ncols, directed=TRUE)
    g <- igraph::set_vertex_attr(g, "name", index=V(g), self$dataset$col.names.to.model)
    g <- igraph::add_edges(g, array(t(bn$arcs)))
    self$causal.graph <- g
    self$vars.topo.sorted <- names(topo_sort(g))
    self$parents <- list()
    for(v in self$dataset$col.names.to.model){
       self$parents[[v]] <- names(neighbors(g, v, mode = "in"))
    }
  }

  )
)
