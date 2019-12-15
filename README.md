# CausalSimR: 

```
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
```
