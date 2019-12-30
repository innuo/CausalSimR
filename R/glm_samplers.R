
dglm_sampler = function(y.var, x.vars, options, data){
  print("-----------")
  print(y.var)
  print(x.vars)

  basic.formula <- as.formula(paste0(y.var, " ~ ",
                                     paste0(x.vars, collapse="+"), "-1"))
  X <- model.matrix(basic.formula, data)
  colnames(X) <- str_replace_all(colnames(X), " ", ".")
  y <- model.extract(model.frame(basic.formula, data), "response")

  predictor.string <- paste0(colnames(X), collapse=",")
  mean.model.formula <- as.formula(sprintf("%s ~ polym(%s, degree=%d, raw=T)",
                                y.var, predictor.string, options$mean.degree))
  var.model.formula <- as.formula(sprintf("~ polym(%s, degree=%d, raw=T)",
                               predictor.string, options$var.degree))

  model <- list(basic.formula = basic.formula, y.var = y.var)

  data.for.fit <- cbind.data.frame(y, X)
  names(data.for.fit)[1] <- y.var

  if(class(y) == "factor"){
    model$type <- "classification"
    model$levels <- levels(y)
    fit <- multinom(mean.model.formula,
                    data = data.for.fit)
    model$fit <- strip_glm(fit)
  }
  else{
    model$type <- "regression"
    #browser()
    fit <- dglm(mean.model.formula, dformula = var.model.formula,
                family = options$mean.model.family,
                dlink = "log", data= data.for.fit,
                ykeep=FALSE, model=FALSE) #TODO: "log" shit

    model$fit <- strip_glm(fit)
    model$fit$dispersion.fit <- strip_glm(fit$dispersion.fit)
  }


  class(model) <- "DGLMSampler"
  model

}


predict.DGLMSampler = function(model, data){
  options(na.action='na.pass') #often some of the cols are NA
  X <- model.matrix(model$basic.formula, data)
  colnames(X) <- str_replace_all(colnames(X), " ", ".")

  if(model$type == "classification"){
    probs <- predict(model$fit, as.data.frame(X), type="prob")
    if(is.null(dim(probs))){
      probs <- cbind(1-probs, probs)
      colnames(probs) <- model$levels
    }
    y <- do.call(c, lapply(1:nrow(data), function(i) sample(colnames(probs), 1, prob=probs[i,])))
    y <- factor(y, levels=model$levels)
  }
  else{
    y.mean <- predict(model$fit, as.data.frame(X), type="response")
    y.vars <- predict(model$fit$dispersion.fit, as.data.frame(X), type="response")
    y <- rnorm(length(y.mean)) * sqrt(y.vars) + y.mean
  }
  y
}

strip_glm = function(cm) {
  cm$y = c()
  cm$model = c()

  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()

  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  cm
}
