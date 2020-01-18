#' @export
ConditionalSampler <- R6::R6Class("ConditionalSampler", list(
  dataset = NULL,
  x.vars = NULL,
  y.var = NULL,
  y.type = NULL,
  options = NULL,
  model = NULL,

  initialize = function(dataset, y.var, x.vars = NULL, options = NULL) {
    self$dataset <- dataset
    self$y.var <- y.var
    self$y.type <- self$dataset$col.types[[y.var]]
    self$x.vars <- x.vars
    if(is.null(options)){
      self$options <- list(mean.degree = 2,
                     var.degree = 2)
    }
    else self$options <- options
  },

  options_from_list = function(options){
    self$options <- options
  },

  learn = function(){
    learn.method <- self$.polyreg_sampler
    if(length(self$x.vars) == 0){
      if(self$y.type == "numeric")
        learn.method <-  self$.quantile_sampler
      else
        learn.method <- self$.factor_sampler
    }
    self$model <- learn.method()

  },

  predict = function(parent.data){
    if(class(self$model) == "Factor" || class(self$model) == "Quantile")
      y <- rep(NA, nrow(parent.data))
    else
      y <- predict(self$model$model, parent.data)
    y
  },

  draw = function(...){
    draw.method <- self$.draw.generic
    if(class(self$model) == "Factor")
      draw.method <- self$.draw.Factor
    else if(class(self$model) == "Quantile")
      draw.method <- self$.draw.Quantile

    y <- draw.method(...)
    y
  },

  .quantile_sampler = function(){
    data <- self$dataset$dataset_from_vars(c(self$y.var, self$x.vars))

    model <- list(vec=data[[self$y.var]])
    class(model) <- "Quantile"
    model
  },

  .factor_sampler = function(){
    data <- self$dataset$dataset_from_vars(c(self$y.var, self$x.vars))

    model <- list()
    model$table <- as.data.frame(table(data[[self$y.var]]))
    class(model) <- "Factor"
    model
  },

  .draw.Quantile = function(parent.data){
    n <- nrow(parent.data)
    y <- quantile(self$model$vec, probs=runif(n), na.rm = TRUE)
    y
  },

  .draw.Factor = function(parent.data){
    n <- nrow(parent.data)
    y <- .sample.vec(self$model$table[,1], size = n, replace=TRUE, prob=self$model$table$Freq)
    y
  },

  .glm_sampler = function(){
    model <- list()
    data <- self$dataset$dataset_from_vars(c(self$y.var, self$x.vars))

    model$model = dglm_sampler(self$y.var, self$x.vars, self$options, data)
    class(model) <- "DGLM"
    model
  },

  .polyreg_sampler = function(){
    model <- list()
    data <- self$dataset$dataset_from_vars(c(self$y.var, self$x.vars))

    model$model = polyreg_sampler(self$y.var, self$x.vars, self$options, data)
    class(model) <- "PolyReg"
    model
  },

  .draw.generic = function(parent.data){
    y <- draw(self$model$model, parent.data)
    y
  }

)
)

.sample.vec = function(x, ...){
  x[sample(length(x), ...)]
}

draw <- function(object, ...){
  UseMethod("draw")
}
