polyreg_sampler = function(y.var, x.vars, options, data){

  y <-  data[[y.var]]
  model <- list(y.var = y.var, x.vars=x.vars)
  data.for.fit <- data.frame(const=rnorm(nrow(data)), data[, x.vars], y) #rnorm instead of 1, because of singularity in learning
  names(data.for.fit) <- c("const", x.vars, y.var)

  if(class(y) == "factor"){
    model$type <- "classification"
    model$levels <- levels(y)
    fit <- polyreg::polyFit(data.for.fit, deg = options$mean.degree, use="mvrlm")
    fit$fit <- strip_glm(fit$fit)
    model$fit <- fit
    class(model$fit) <- "MyPolyFit" #so I can get probs out
  }
  else{
    model$type <- "regression"
    mean.fit <- polyreg::polyFit(data.for.fit, deg = options$mean.degree)
    residuals <- predict(mean.fit, data.for.fit) - data.for.fit[[y.var]]

    rq <- quantile(residuals, probs = c(0.1, 0.9))
    residuals[residuals < rq[1]] <- rq[1]
    residuals[residuals > rq[2]] <- rq[2]

    mean.fit <- strip_polyreg(mean.fit)
    model$mean.fit <- mean.fit

    data.for.fit <- subset(data.for.fit, select=c("const", x.vars))
    data.for.fit$var <- log(abs(residuals)+0.001*sd(y))
    var.fit <- polyreg::polyFit(data.for.fit, deg = options$var.degree)
    var.fit <- strip_polyreg(var.fit)
    model$var.fit <- var.fit
    class(model$mean.fit) <- "MyPolyFit"
    class(model$var.fit) <- "MyPolyFit"

  }

  class(model) <- "PolyRegSampler"
  model
}


predict.PolyRegSampler = function(model, data){
  X <- data.frame(const=0, data)
  eps <- 1e-6
  options(na.action='na.pass') #often some of the cols are NA
  if(model$type == "classification"){
    probs <- predict(model$fit, X)
    probs[probs > 1-eps] <- 1-eps
    probs[probs < eps] <- eps
    preds <- do.call(c, lapply(1:nrow(data), function(i) sample(1:ncol(probs), 1, prob=probs[i,])))
    y <- model$levels[preds]
  }
  else{
    preds <- predict(model$mean.fit, X)
    vars  <- exp(predict(model$var.fit, X))
    y <- rnorm(length(preds)) * pmax(vars, rep(0, length(preds))) +  preds
  }
  y
}



predict.MyPolyFit <- function (object, newdata, ...)
{
  use <- object$use
  doPCA <- !is.null(object$PCA)
  if (!doPCA) {
    plm.newdata <- getPoly(newdata, object$degree, object$maxInteractDeg,
                           modelFormula = object$XtestFormula, retainedNames = object$retainedNames,
                           ...)$xdata
  }
  else if (object$PCA == "prcomp") {
    message("Beginning PCA\n\n", timestamp())
    if (object$pcaLocation == "front") {
      new_data <- predict(object$pca.xy, newdata)[, 1:object$pcaCol]
      plm.newdata <- getPoly(new_data, object$degree, object$maxInteractDeg,
                             ...)$xdata
    }
    else if (object$pcaLocation == "back") {
      new_data <- getPoly(newdata, object$degree, object$maxInteractDeg,
                          ...)$xdata
      plm.newdata <- predict(object$pca.xy, new_data)[,
                                                      1:object$pcaCol]
      plm.newdata <- as.data.frame(plm.newdata)
    }
    else stop("invalid pcaLocation. Should be \"front\" or \"back\".")
  }
  else if (object$PCA == "RSpectra") {
    if (object$pcaLocation == "front") {
      xy.eig <- object$pca.xy
      new_data <- as.matrix(newdata) %*% xy.eig$vectors
      plm.newdata <- getPoly(new_data, object$degree, object$maxInteractDeg)$xdata
    }
    else if (object$pcaLocation == "back") {
      new_data <- getPoly(newdata, object$degree, object$maxInteractDeg,
                          ...)$xdata
      xy.eig <- object$pca.xy
      plm.newdata <- as.matrix(new_data) %*% xy.eig$vectors[,
                                                            1:object$pcaCol]
      plm.newdata <- as.data.frame(plm.newdata)
    }
    message("Finished with PCA and model matrix construction.\n\n",
            timestamp())
  }
  if (object$use == "lm") {
    pred <- predict(object$fit, plm.newdata)
    return(pred)
  }
  if (object$use == "mvrlm") {
    pre <- predict(object$fit, plm.newdata)
    pred <- apply(pre, 1, which.max)
    #return(pred)
    return(pre) #probabily matrix
  }
  if (is.null(object$glmMethod)) {
    pre <- predict(object$fit, plm.newdata, type = "response")
    pred <- ifelse(pre > 0.5, object$classes[1], object$classes[2])
  }
  else {
    len <- length(object$classes)
    if (object$glmMethod == "multlog") {
      pr <- predict(object$fit, plm.newdata, type = "probs")
      idx <- apply(pr, 1, which.max)
      col.name <- colnames(pr)
      lc <- length(col.name)
      tempM <- matrix(rep(col.name, length(idx)), ncol = lc,
                      byrow = TRUE)
      pred <- NULL
      for (r in 1:nrow(tempM)) {
        pred[r] <- tempM[r, idx[r]]
      }
      return(pred)
    }
    else if (object$glmMethod == "all") {
      votes <- matrix(0, nrow = nrow(plm.newdata), ncol = len)
      for (i in 1:len) {
        for (j in 1:len) {
          if (i == j)
            next
          pre <- predict(object$fit[[i]][[j]], plm.newdata,
                         type = "response")
          votes[, i] <- votes[, i] + ifelse(pre > 0.5,
                                            1, 0)
        }
      }
      winner <- apply(votes, 1, which.max)
    }
    else if (object$glmMethod == "one") {
      prob <- matrix(0, nrow = nrow(plm.newdata), ncol = len)
      for (i in 1:len) {
        prob[, i] <- predict(object$fit[[i]], plm.newdata,
                             type = "response")
      }
      winner <- apply(prob, 1, which.max)
    }
    pred <- NULL
    for (k in 1:nrow(plm.newdata)) {
      pred[k] <- object$classes[winner[k]]
    }
  }
  return(pred)
}
