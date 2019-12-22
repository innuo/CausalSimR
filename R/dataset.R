#' @export
DataSet <- R6::R6Class("DataSet", list(
  raw.data = list(),
  data = NULL,
  col.types = c(),
  missings.filled = FALSE,

  initialize = function(data) {
    self$attach_data(data)
  },

  ncols = function(){
    length(self$col.types)
  },

  col.names.to.model = function(){
    names(self$col.types)
  },

  attach_data = function(data){
    types <- do.call(rbind, lapply(data, class))
    col.ids.to.model <- which(types[,1] %in% c("numeric", "factor"))

    col.names.to.model = c()
    for(i in col.ids.to.model){
      name <- names(data)[i]
      self$col.types[[name]] <- types[i]
      col.names.to.model <- c(col.names.to.model, name)
    }

    tmp.data <- subset(data, select=col.names.to.model)
    self$data <- plyr::rbind.fill(self$data, tmp.data)
    self$raw.data[[length(self$raw.data)+1]] <- tmp.data
  },

  dataset_from_ids = function(ids){
    if (length(ids) == 0) {
      if(!self$missings.filled) {
        self$fill_missing()
      }
      return(self$data)
    }

    df <- NULL
    for(id in ids){
      if(is.null(df)) df <- self$raw.data[[id]]
      else df <- rbind(df, self$raw.data[[id]])
    }
    return (df)
  },

  matching_dataset_ids = function(vars){
    ids <- c()
    for(i in 1:length(self$raw.data)){
      if(!any(is.na(match(vars, names(self$raw.data[[i]])))))
        ids <- c(ids, i)
    }
    return(ids)
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
    self$missings.filled <- TRUE
  }

  )
)
