ConditionalSampler <- R6Class("ConditionalSampler", list(
  dataset = NULL,
  options = NULL,
  initialize = function(dataset, options) {
    self$dataset <- dataset
    self$options <- options
  }

)
)
