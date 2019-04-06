CausalSimModel <- R6::R6Class("CausalSimModel", list(
  options = NULL,
  dataset = NULL,
  structure = NULL,
  conditional.samplers = list(),
  initialize = function(dataset, options=NULL) {
    self$dataset <- dataset
    self$options <- options
    self$structure <- CausalStructure$new(dataset, options)
    self$structure$learn_structure()
    for(v in self$dataset$col.names.to.model){
      self$conditional.samplers[[v]] <- ConditionalSampler$new(dataset,
                                                               y.var = v,
                                                               x.vars=self$structure$parents[[v]],
                                                               options=options)
    }
  },

  learn = function(){
    self$structure$learn_structure()
    for(v in self$dataset$col.names.to.mode){
      self$conditional.samplers[[v]]$learn()
    }
  },

  sample = function(n){
    sample.df <- data.frame(matrix(NA, nrow=n, ncol=self$dataset$ncols))
    names(sample.df) <- self$dataset$col.names.to.model
    for(v in self$structure$vars.topo.sorted){
      sample.df[[v]] <- self$conditional.samplers[[v]]$draw(sample.df)
    }
    sample.df
  },

  plot = function(){
    plot(self$structure$causal.graph)
  }


  )
)
