

kernelized_sampler = function(formula, data, parameters=list()){
  X <- scale(model.matrix(formula, data))
  y <- model.extract(model.frame(formula, data), "response")

  model <- list(formula = formula, parameters=parameters,
                X.center=attr(X, "scaled:center"), X.scale=attr(X, "scaled:scale"))


  num.prototypes <- ifelse(is.null(parameters$num_prototypes), ceiling(sqrt(nrow(data))), parameters$num_prototypes)
  model$prototypes <- X[sample(1:nrow(data), num.prototypes),, drop=FALSE]
  #model$prototypes <- scale(lhs::maximinLHS(num.prototypes, dim(X)[2]))

  Xk = make_kernel_matrix(X, model$prototypes, parameters)
  if(class(y) == "factor"){
    model$type <- "classification"
    model$levels <- levels(y)
    model$classifier <- LiblineaR::LiblineaR(Xk, y, type=6)
  }
  else{
    model$type <- "regression"
    model$y.center <- mean(y)
    model$y.scale <- sd(y)
    y.scaled <- scale(y)

    model$mean.regressor <- LiblineaR::LiblineaR(Xk, y.scaled , type=11, svr_eps=0.01)
    pred.error.squares <- (predict(model$mean.regressor, Xk)$predictions - y.scaled)^2
    model$var.regressor <-  LiblineaR::LiblineaR(Xk, pred.error.squares , type=11, svr_eps=0.001)

  }
  class(model) <- "KernelizedSampler"
  model

}


predict.KernelizedSampler = function(model, data){
  options(na.action='na.pass')
  X <- scale(model.matrix(model$formula, data), center = model$X.center, scale = model$X.scale)
  Xk = make_kernel_matrix(X, model$prototypes, model$parameters)
  if(model$type == "classification"){
    probs <- predict(model$classifier, Xk, proba=TRUE)$probabilities
    y <- do.call(c, lapply(1:nrow(data), function(i) sample(model$levels, 1, prob=probs[i,])))
    y <- factor(y, levels=model$levels)
  }
  else{
    y.hat.scaled <- predict(model$mean.regressor, Xk)$predictions
    y.vars <-  pmax(predict(model$var.regressor, Xk)$predictions, rep(0, length(y.hat.scaled))) + 0.01
    y.scaled <- y.hat.scaled + rnorm(nrow(data), sd = sqrt(y.vars))
    y <- y.scaled * model$y.scale + model$y.center
  }
  y
}

make_kernel_matrix = function(X, prototypes, parameters){
  if(is.null(parameters$kernel))
    #kernel.fun <- kernlab::rbfdot()
    kernel.fun <- kernlab::polydot(degree=3)
  else
    kernel.fun <- do.call(get(paste0("kernlab::",parameters$kernel)), parameters$kernel_args)

  Xk = kernlab::kernelMatrix(kernel.fun, X, prototypes)
  Xk
}

