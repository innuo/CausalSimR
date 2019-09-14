#' @export
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

  structure_to_json_string = function(){
    str = jsonlite::toJSON(self$structure$to_list(), pretty = TRUE)
  },

  structure_from_json_file = function(json.file){
    str = readChar(json.file, file.info(json.file)$size)
    self$structure_from_json_string(str)
  },

  structure_from_json_string = function(json.string){
    self$structure$from_list(jsonlite::fromJSON(json.string))
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
    for(v in self$dataset$col.names.to.model){
      #print(v)
      self$conditional.samplers[[v]]$learn()
    }
  },

  sample = function(n, do = list()){
    sample.df <- data.frame(matrix(NA, nrow=n, ncol=self$dataset$ncols))
    names(sample.df) <- self$dataset$col.names.to.model
    for(v in self$structure$vars.topo.sorted){
      if(is.null(do[[v]]))
        sample.df[[v]] <- self$conditional.samplers[[v]]$draw(sample.df)
      else{
        sample.df[[v]] <- self$dataset$make_column(rep(do[[v]], n), v)
      }
    }
    sample.df
  },

  fill = function(){ #fill using gibbs sampling on learned model

  },

  plot = function(){
    plot(self$structure$causal.graph)
  }


  )
)
