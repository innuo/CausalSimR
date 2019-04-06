CausalSimModel <- R6Class("CausalSimModel", list(
  dataset = NULL,
  initialize = function(dataset) {
    self$dataset <- dataset
  }
  )
)
