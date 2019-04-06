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
    self$y.type <- self$dataset$col.types[y.var]
    self$x.vars <- x.vars
    self$options <- options
    self$learn()
  },

  learn = function(){
    learn.method <- self$.rf_sampler
    if(length(self$x.vars) == 0){
      if(self$y.type == "numeric")
        learn.method <-  self$.quantile_sampler
      else
        learn.method <- self$.factor_sampler
    }
    self$model <- learn.method()

  },

  draw = function(...){
    draw.method <- self$.draw.RF
    if(class(self$model) == "Factor")
      draw.method <- self$.draw.Factor
    else if(class(self$model) == "Quantile")
      draw.method <- self$.draw.Quantile

    y <- draw.method(...)
    y
  },

  .quantile_sampler = function(){
    model <- list(vec=self$dataset$data[[self$y.var]])
    class(model) <- "Quantile"
    model
  },

  .factor_sampler = function(){
    model <- list()
    model$table <- as.data.frame(table(self$dataset$data[[self$y.var]]))
    class(model) <- "Factor"
    model
  },

  .draw.Quantile = function(parent.data){
    n <- nrow(parent.data)
    y <- quantile(self$model$vec, probs=runif(n), na.rm = TRUE)
    y
  },

  .draw.Factor = function(parent.data){
    #browser()
    n <- nrow(parent.data)
    y <- sample(self$model$table[,1], size = n, replace=TRUE, prob=self$model$table$Freq)
    y
  },

  #TODO: read from options
  .rf_sampler = function(){
    model <- list()
    formula.string <- paste0(self$y.var, " ~ ", paste0(self$x.vars, collapse="+"))
    model$rfm <- ranger(as.formula(formula.string), data=self$dataset$data,
                  sample.fraction = min(2000/self$dataset$nrows, 1),
                  num.trees =100,
                  min.node.size = 1,
                  mtry=min(3, length(self$x.vars)))
    class(model) <- "RF"
    model

  },

  .draw.RF = function(parent.data){
    preds <- predict(self$model$rfm, parent.data, predict.all=TRUE)
    browser()
    tree.num <- sample(1:ncol(preds$predictions), 1)
    y <- preds$predictions[,tree.num]
    if(self$y.type == "factor")
      y <- levels(self$dataset$data[[self$y.var]])[y]
    y
  }

)
)

