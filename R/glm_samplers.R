
processed.model.matrix <- function(data, basic.model = NULL,
                                   y.var = NULL, x.vars =NULL){
  if(is.null(basic.model)){
    basic.formula <- as.formula(paste0(y.var, " ~ ",
                                       paste0(x.vars, collapse="+")))

    X <- model.matrix(basic.formula, data=data)
    X <- X[, colnames(X) != "(Intercept)", drop=FALSE]
    colnames(X) <- str_replace_all(colnames(X), " ", ".")

    X <- as.data.frame(X)
    X <- X[!duplicated(as.list(X))]

    X <- scale(X)

    basic.model <- list(basic.formula=basic.formula, colnames = colnames(X),
                        X.center=attr(X, "scaled:center"), X.scale=attr(X, "scaled:scale"))

    X <- as.data.frame(X)
  }
  else{
    X <- model.matrix(basic.model$basic.formula, data=data)
    colnames(X) <- str_replace_all(colnames(X), " ", ".")
    X <- subset(as.data.frame(X), select=basic.model$colnames)

    X <- scale(X, center = basic.model$X.center, scale = basic.model$X.scale)
    X <- as.data.frame(X)
  }
  return (list(basic.model=basic.model, X=X))
}


dglm_sampler = function(y.var, x.vars, options, data){
  # basic.formula <- as.formula(paste0(y.var, " ~ ",
  #                                    paste0(x.vars, collapse="+"), "-1"))

  ret <- processed.model.matrix(data, y.var = y.var, x.vars =x.vars)
  y <- model.extract(model.frame(ret$basic.model$basic.formula, data), "response")
  X <- ret$X

  predictor.string <- paste0(colnames(X), collapse=",")
  mean.model.formula <- as.formula(sprintf("%s ~ polym(%s, degree=%d, raw=T)",
                                           y.var, predictor.string, options$mean.degree))
  var.model.formula <- as.formula(sprintf("~ polym(%s, degree=%d, raw=T)",
                                          predictor.string, options$var.degree))
  # mean.model.formula <- as.formula(sprintf("%s ~ polym(%s, degree=%d, raw=T)",
  #                                          y.var, predictor.string, 1))
  # var.model.formula <- as.formula(sprintf("~ polym(%s, degree=%d, raw=T)",
  #                                         predictor.string, 1))

  model <- list(basic.model = ret$basic.model, y.var = y.var)

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

    mm.mean <- model.matrix(mean.model.formula, data.for.fit)
    mm.mean <- mm.mean[, colnames(mm.mean) != "(Intercept)", drop=FALSE]

    mm.sigma <- model.matrix(var.model.formula, data.for.fit)
    mm.sigma <- mm.sigma[, colnames(mm.sigma) != "(Intercept)", drop=FALSE]

    fit <- lmvar(y, X_mu=mm.mean, X_sigma=mm.sigma)
    model$mean.model.formula <- mean.model.formula
    model$var.model.formula <- var.model.formula
    model$fit <- strip_lmvar(fit)

    #### DLGM fails with divergence issues. Switching to lmvar for now
    # fit <- do.call(dglm, list(formula=mean.model.formula,
    #                           dformula = var.model.formula,
    #                           family = get(options$mean.model.family),
    #                           dlink = options$var.model.link,
    #                           data= data.for.fit,
    #                           ykeep=FALSE, model=FALSE, singular.ok=TRUE))
    #
    # model$fit <- strip_glm(fit)
    # model$fit$dispersion.fit <- strip_glm(fit$dispersion.fit)
  }


  class(model) <- "DGLMSampler"
  model

}


predict.DGLMSampler = function(model, data){
  options(na.action='na.pass') #often some of the cols are NA

  X <- processed.model.matrix(data, model$basic.model)$X

  if(model$type == "classification"){
    probs <- predict(model$fit, X, type="prob")
    if(is.null(dim(probs))){
      probs <- cbind(1-probs, probs)
      colnames(probs) <- model$levels
    }
    y <- do.call(c, lapply(1:nrow(data), function(i) sample(colnames(probs), 1, prob=probs[i,])))
    y <- factor(y, levels=model$levels)
  }
  else{
    mf <- as.formula(paste("~", labels(terms(model$mean.model.formula))))
    mm.mean <- model.matrix(mf, X)
    mm.mean <- mm.mean[, colnames(mm.mean) != "(Intercept)", drop=FALSE]

    mm.sigma <- model.matrix(model$var.model.formula, X)
    mm.sigma <- mm.sigma[, colnames(mm.sigma) != "(Intercept)", drop=FALSE]

    preds <- predict(model$fit, X_mu=mm.mean, X_sigma=mm.sigma)
    y <- rnorm(nrow(preds)) * preds[,2] + preds[,1]

    #### Replacing DGLM with LMVAR
    # y.mean <- predict(model$fit, X, type="response")
    # y.vars <- predict(model$fit$dispersion.fit, X, type="response")
    # y <- rnorm(length(y.mean)) * sqrt(y.vars) + y.mean
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

strip_lmvar <- function(fit){
  fit$X_mu = c()
  fit$X_sigma = c()
  fit$y = c()
  return(fit)
}
