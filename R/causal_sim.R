CausalSimModel <- R6::R6Class("CausalSimModel", list(
  options = NULL,
  dataset = NULL,
  structure = NULL,
  conditional.samplers = list(),
  initialize = function(dataset, options=NULL) {
    self$dataset <- dataset
    self$options <- options
    self$structure <- CausalStructure$new(dataset, options)
   },

  structure_to_json = function(){
    str = toJSON(self$structure$to_list(), pretty = TRUE)
  },

  structure_from_json = function(json.string){
    self$structure$from_list(fromJSON(json.string))
  },

  learn_structure = function(){
    self$structure$learn_structure()
  },

  learn_samplers = function(){
    for(v in self$dataset$col.names.to.model){
      self$conditional.samplers[[v]] <- ConditionalSampler$new(self$dataset,
                                                               y.var = v,
                                                               x.vars=self$structure$parents[[v]],
                                                               options=options)
    }


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
