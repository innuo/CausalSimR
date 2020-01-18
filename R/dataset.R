#' @export
DataSet <- R6::R6Class("DataSet", list(
  raw.data = list(),
  data = NULL,
  filled.data = NULL,
  col.types = c(),
  options = NULL,

  initialize = function(data, options=NULL) {
    self$attach_data(data)
    if(is.null(options))
      self$options = list(minimum_dataset_size_to_learn = 1000,
                          imputed_dataset_size = 5000)
    else
      self$options = options
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

    self$data <- if(is.null(self$data)) tmp.data
                 else factor_safe_bind_rows(self$data, tmp.data)

    self$raw.data[[length(self$raw.data)+1]] <- tmp.data
    self$fill_missing()
  },

  dataset_from_vars = function(vars, impose.minimum.size=TRUE){
    ids <- self$matching_dataset_ids(vars, impose.minimum.size)
    if (length(ids) == 0) {
      return(self$filled.data)
    }

    df <- NULL
    for(id in ids){
      tmp.df <- self$raw.data[[id]]
      non.missing.ids <- complete.cases(subset(tmp.df, select=vars))
      if(is.null(df)) {
        df <- tmp.df[non.missing.ids,]
      }
      else df <- factor_safe_bind_rows(df, tmp.df[non.missing.ids,])
    }
    return (df)
  },

  matching_dataset_ids = function(vars, impose.minimum.size){
    ids <- numeric(0)
    for(i in 1:length(self$raw.data)){
      if(!any(is.na(match(vars, names(self$raw.data[[i]]))))){
        #check if raw dataset has enough samples
         if(!impose.minimum.size ||
            sum(complete.cases(subset(self$raw.data[[i]], select=vars))) >=
                           self$options$minimum_dataset_size_to_learn)
            ids <- c(ids, i)

      }
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

  estimate_fit = function(df){
    n <- nrow(df)
    fit.scores <- rep(NA, length(self$raw.data))
    for (i in 1:length(self$raw.data)){
      rd <- na.omit(self$raw.data[[i]])
      if (nrow(rd) < 100) next

      d1 <- rd[sample(1:nrow(rd), n, replace = T),]
      d2 <- rd[sample(1:nrow(rd), n, replace = T),]

      ds <- subset(df, select=names(rd))

      base.score <- kl_est(d1, d2) + kl_est(d2, d1)
      cross.score <- kl_est(ds, d1) + kl_est(d2, ds)

      fit.scores[i] <- min(1/cross.score^(1/ncol(rd)), 1)
    }
    fit.scores
  },

  drop_missing = function(){
    self$data <- na.omit(self$data)
  },

  fill_missing = function(method="cart", iter=5){
    nr <- nrow(self$data)
    self$filled.data <- fill_using_mice(self$data[sample(1:nr,
                                 min(self$options$imputed_dataset_size, nr)),],
                                 method, iter)

  }

  )
)

fill_using_mice <- function(df, method, num.iter){
   if(any(is.na(df))){
    tmp <- mice::mice(df,m=2,maxit=num.iter,remove_collinear = FALSE,
                      meth=method,seed=1, printFlag = F)
    df<- mice::complete(tmp)
    print(summary(df))
  }
  return(df)
  #for(i in 1:ncol(self$data))
  #  if(class(self$filled.data[,i]) != "factor") self$filled.data[,i] <- as.numeric(self$filled.data[,i])

}

factor_safe_bind_rows <- function(...){
  factor.cols <- c()
  for (d in list(...)){
    is.f <- unlist(lapply(d, is.factor))
    factor.cols <- c(factor.cols, names(d)[is.f])
  }

  df <- suppressWarnings(dplyr::bind_rows(...))
  for(f in unique(factor.cols))
    df[[f]] <- as.factor(df[[f]])

  df
}

kl_est <- function(X1, X2,
                   samp.size.max=min(1000, nrow(X1)),
                   mtry=ceiling(sqrt(ncol(X1))),
                   min.node.size=ceiling(log(nrow(X1))),
                   num.trees=100, ratio.trunc.val=100){
  print(colnames(X1))
  print(colnames(X2))
  X <- rbind(X1, X2)
  y <- c(rep(0, nrow(X1)), rep(1, nrow(X2)))
  ret <- ranger(y ~., data = cbind.data.frame(y=y, X=X),
                classification=TRUE, probability = TRUE,
                sample.fraction=samp.size.max/nrow(X),
                mtry=mtry, min.node.size = min.node.size, num.trees=num.trees)

  # P(x|0) / P(x|1) = P(0|x) P(1) / P(1|x) P(0)
  ratio <- (ret$predictions[,1] / ret$predictions[,2]) * (nrow(X2)/nrow(X1))
  ratio[ratio > ratio.trunc.val] <- ratio.trunc.val
  ratio[ratio < 1/ratio.trunc.val] <- 1/ratio.trunc.val
  log.ratio <- log(ratio)

  divergence <- max(0.5 * (mean(log.ratio[1:nrow(X1)]) -
                              mean(log.ratio[(nrow(X1)+1):nrow(X)])), 0)
  #divergence <- max(mean(log.ratio[1:nrow(X1)]), 0)
  divergence

}
