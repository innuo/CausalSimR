basic_test <- function(){
  full.data <- read.csv("../CausalSimPy/data/5d.csv")
  dataset <- DataSet$new(full.data)
  dataset$fill_missing()
  #dataset$drop_missing()

  sim <- CausalSimModel$new(dataset)
  sim$learn_structure()
  sim$plot()
  sim$learn_samplers()

  df <- sim$sample(10000)

  plot(full.data$Price, full.data$VolumeBought, col="blue")
  points(df$Price, df$VolumeBought, col="red")

}

basic_attach_test <- function(){
  df1 <- read.csv("../CausalSimPy/data/5d_1.csv")
  dataset <- DataSet$new(df1)
  df2 <- read.csv("../CausalSimPy/data/5d_2.csv")
  dataset$attach_data(df2)

  sim <- CausalSimModel$new(dataset)
  sim$learn_structure()
  sim$plot()
  sim$learn_samplers()

  df <- sim$sample(10000)

  plot(full.data$Price, full.data$VolumeBought, col="blue")
  points(df$Price, df$VolumeBought, col="red")

}


missing_test <- function(train.sample.size = 1000){
  full.data <- read.csv("../CausalSimPy/data/5d.csv")

  data <- full.data[sample(1:nrow(full.data), train.sample.size),]
  missing.data <- drop.data.cells(data, 0.3) #add missings

  dataset <- DataSet$new(missing.data)
  dataset$fill_missing()
  #dataset$drop_missing()

  sim <- CausalSimModel$new(dataset)
  sim$learn_structure()
  sim$learn_samplers()
  sim$plot()

  filled.gibbs <- sim$fill_gibbs(missing.data)
  filled.mice <- sim$dataset$filled.data

  #diagnostics(data, filled.gibbs, missing.data, "Gibbs")
  #diagnostics(data, filled.mice, missing.data, "MICE")
}


diagnostics <- function(data, filled, missing.data, label){
  for(i in 1:ncol(data)){
    missing.inds <- is.na(missing.data[,i])
    if(class(data[,i]) == 'numeric'){
      rho = cor(data[missing.inds, i], filled[missing.inds, i])
      rmse = rmse(data[missing.inds, i], filled[missing.inds, i])
      plot(data[missing.inds, i], filled[missing.inds, i], pch=20,
           col="blue", main=paste(label, ":", names(data)[i], ", rmse =", round(rmse, 2)), xlab="Original", ylab="Filled")
      abline(0, 1, col="red")
      grid()
    }
    else{
      cat("\n\n")
      cat("==============\n")
      print(paste(label, ":", names(data)[i]))
      cat("-------------\n")
      tmp <- cbind.data.frame(data[missing.inds, i], filled[missing.inds, i])
      names(tmp) <- c("Original", "Filled")
      print(table(tmp))
    }
  }
}

drop.data.cells <- function(df, fraction){
  for(i in 1:ncol(df)){
    na.inds <- sample(1:nrow(df), round(nrow(df) * fraction))
    df[na.inds, i] <- NA
  }
  df
}

rmse <- function(x, y){
  sqrt(mean((x-y)^2))
}
