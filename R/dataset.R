#' @export
DataSet <- R6::R6Class("DataSet", list(
  raw.data = NULL,
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
    names(self$col.types) <- self$col.names.to.model
    self$data <- subset(data, select=self$col.names.to.model)
    self$nrows <- nrow(data)
    self$ncols <- length(self$col.names.to.model)
    self$raw.data <- self$data
  },

  make_column = function(vec, col.name){
    if(self$col.types[[col.name]] == "factor")
      v = factor(vec, levels(self$data[[col.name]]))
    else
      v = as.numeric(vec)
    v
  },

  drop_missing = function(){
    self$data <- na.omit(self$data)
  },

  fill_missing = function(method="cart", iter=10){
    if(any(is.na(self$data))){
      tmp <- mice::mice(self$data,m=2,maxit=iter,meth=method,seed=1, printFlag = F)
      self$data <- mice::complete(tmp)
    }
    for(i in 1:ncol(self$data)) if(class(self$data[,i]) != "factor") self$data[,i] <- as.numeric(self$data[,i])
  }

  )
)
