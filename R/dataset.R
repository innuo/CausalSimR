DataSet <- R6Class("DataSet", list(
  data = NULL,
  col.names.to.model = NULL,
  col.types = NULL,
  nrows = NULL,
  ncols = NULL,

  initialize = function(data) {
    types <- do.call(rbind, lapply(data, class))
    col.ids.to.model <- which(types[,1] %in% c("numeric", "factor"))
    self$col.names.to.model <- row.names(types)[col.ids.to.model]
    self$col.types <- types[col.ids.to.model]
    self$data <- subset(data, select=self$col.names.to.model)
    self$nrows <- nrow(data)
    self$ncols <- length(self$col.names.to.model)
  }


  )
)
