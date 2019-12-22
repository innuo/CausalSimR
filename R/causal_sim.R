#' @export
CausalSimModel <- R6::R6Class("CausalSimModel", list(
  options = NULL,
  dataset = NULL,
  structure = NULL,
  conditional.samplers = list(),
  markov.blanket.samplers = list(),

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

  learn_sampler = function(v, options){
    self$conditional.samplers[[v]] <- ConditionalSampler$new(self$dataset,
                                                             y.var = v,
                                                             x.vars=self$structure$parents[[v]],
                                                             options=options)
    self$markov.blanket.samplers[[v]] <- ConditionalSampler$new(self$dataset,
                                                                y.var = v,
                                                                x.vars=self$structure$markov.blanket[[v]],
                                                                options=options)

    self$conditional.samplers[[v]]$learn()
    self$markov.blanket.samplers[[v]]$learn()
  },

  learn_samplers = function(options=NULL){
    for(v in self$dataset$col.names.to.model){
      self$learn_sampler(v, options)
    }
  },

  sample = function(n, do = list()){
    sample.df <- data.frame(matrix(NA, nrow=n, ncol=self$dataset$ncols))
    names(sample.df) <- self$dataset$col.names.to.model

    for(v in self$structure$vars.topo.sorted){
      if(is.null(do[[v]])){
        sample.df[[v]] <- self$dataset$make_column(self$conditional.samplers[[v]]$draw(sample.df), v)
      }
      else{
        sample.df[[v]] <- self$dataset$make_column(rep(do[[v]], n), v)
      }
    }
    sample.df
  },

  fill_gibbs = function(df.missing, num.iter=20){
    nc <- ncol(df.missing)
    cnames <- names(df.missing)
    pred.mat <- matrix(0, nrow=nc, ncol=nc)
    for (i in 1:nc){
      v <- cnames[i]
      jinds <- match(self$structure$markov.blanket[[v]], cnames)
      if(!is.null(self$structure$markov.blanket[[v]])){
        pred.mat[i,jinds] <- 1
      }
      else{
        pred.mat[i,jinds] <- 1
      }

      pred.mat[i,i] <- 0 #zero diagonal
    }

    if(any(is.na(df.missing))){
      tmp <- mice::mice(df.missing,m=2,maxit=num.iter,meth="cart",seed=1, printFlag = F)
      df.filled <- mice::complete(tmp)
    }
    else df.filled <- df.missing
    for(i in 1:ncol(df.filled)) if(class(df.filled[,i]) != "factor") df.filled[,i] <- as.numeric(df.filled[,i])
    df.filled
  },

  fill_gibbs_old = function(df.missing, num.iter = 10){ #fill using gibbs sampling on learned model
    non.missing.inds <- !is.na(df.missing)

    tmp.data <- DataSet$new(df.missing)
    tmp.data$fill_missing()
    filled.df <- tmp.data$data #initial random draw

    for(i in 1:ncol(filled.df)) filled.df[non.missing.inds[,i], i] <- df.missing[non.missing.inds[,i], i]

    for(i in 1:num.iter){
      cols <- sample(1:ncol(filled.df))
      for(j in cols){
          v <- names(filled.df)[j]
          filled.df[, j] <- self$markov.blanket.samplers[[v]]$draw(filled.df)
          filled.df[non.missing.inds[,j], j] <- df.missing[non.missing.inds[,j], j]
      }
    }
    filled.df
  },

  plot = function(){
    plot(self$structure$causal.graph)
  }


  )
)
