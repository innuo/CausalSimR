ConditionalSampler <- R6Class("ConditionalSampler", list(
  dataset = NULL,
  x.vars = NULL,
  y.var = NULL,
  options = NULL,
  model = NULL,

  initialize = function(dataset, y.var, x.vars = NULL, options = NULL) {
    self$dataset <- dataset
    self$y.var <- y.var
    self$x.vars <- x.vars
    self$options <- options
    self$.learn()
  },

  .learn = function(){


  }

)
)

quantile.sampler = function(v, options=NULL){
  model <- list(vec=v)
  class(model) <- "Quantile"
  model
}

factor.sampler = function(v, options=NULL){
  model <- list()
  model$table <- as.data.frame(table(a))
  class(model) <- "Factor"
  model
}


draw.Factor = function(model, n){
  y <- sample(model$table$a, size = n, replace=TRUE, prob=amodel$table$Freq)
  y
}

draw.Quantile = function(model, n){
  y <- quantile(model$vec, probs=runif(n), na.rm = TRUE)
  y
}
