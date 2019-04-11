#' @importFrom magrittr %>%
simple_nn_sampler = function(formula, data, parameters=list()){
  X <- scale(model.matrix(formula, data))
  y <- model.extract(model.frame(formula, data), "response")

  model <- list(formula = formula, parameters=parameters,
                X.center=attr(X, "scaled:center"), X.scale=attr(X, "scaled:scale"))

  h <- 10
  m <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = h, input_shape = ncol(X), activation = 'relu') %>%
    #keras::layer_dropout(0.5) %>%
    keras::layer_dense(units = h, activation = 'relu')
    #keras::layer_dropout(0.5)

  if(class(y) == "factor"){
    model$type <- "classification"
    model$levels <- levels(y)
    m <-  m %>% layer_dense(units = length(levels(y)), activation = 'softmax')
    m %>% keras::compile(
      optimizer = 'adam',
      loss = 'sparse_categorical_crossentropy',
      metrics = c('accuracy')
    )
    m %>% keras::fit(X, as.numeric(y)-1, epochs=50, verbose=0)
    model$model <- m
  }
  else{
    model$type <- "regression"
    m.mean <-  m %>% keras::layer_dense(units=1, input_shape=h)
    m.var <- keras::clone_model(m.mean)

    m.mean %>% keras::compile(
      optimizer = 'adam',
      loss = 'mse',
      metrics = c('mean_absolute_error')
    )
    m.var %>% keras::compile(
      optimizer = 'adam',
      loss = 'mse',
      metrics = c('mean_absolute_error')
    )

    m.mean %>% keras::fit(X, as.numeric(y), epochs=50, verbose=0)
    m.var %>% keras::fit(X, (y - predict(m.mean, X))^2, epochs=50, verbose=0)
    model$model <- list(mean=m.mean, var=m.var)
  }

  class(model) <- "Simple.NN"
  model
}

#' @importFrom magrittr %>%
predict.Simple.NN = function(model, data){
  X <- scale(model.matrix(model$formula, data), center = model$X.center, scale = model$X.scale)

  if(model$type == "classification") {
    preds <- model$model %>% predict(X)
    y <- do.call(c, lapply(1:nrow(data), function(i) sample(model$levels, 1, prob=preds[i,])))
    y <- factor(y, levels=model$levels)
  }
  else{
    preds <- model$model$mean %>% predict(X)
    y <- preds + rnorm(length(preds), 0, sd = sqrt(abs(model$model$var %>% predict(X))) + 0.001)
  }
  y
}

compute_kernel <- function(x, y) {
  x_size <- keras::k_shape(x)[1]
  y_size <- keras::k_shape(y)[1]
  dim <- keras::k_shape(x)[2]
  tiled_x <- k_tile(
    keras::k_reshape(x, k_stack(list(x_size, 1, dim))),
    keras::k_stack(list(1, y_size, 1))
  )
  tiled_y <- k_tile(
    keras::k_reshape(y, keras::k_stack(list(1, y_size, dim))),
    keras::k_stack(list(x_size, 1, 1))
  )
  keras::k_exp(-keras::k_mean(keras::k_square(tiled_x - tiled_y), axis = 3) /
                 keras::k_cast(dim, tf$float64))
}

compute_mmd <- function(x, y, sigma_sqr = 1) {
  x_kernel <- compute_kernel(x, x)
  y_kernel <- compute_kernel(y, y)
  xy_kernel <- compute_kernel(x, y)
  keras::k_mean(x_kernel) + keras::k_mean(y_kernel) - 2 * keras::k_mean(xy_kernel)
}


