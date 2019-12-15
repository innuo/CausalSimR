# CausalSimR: 

```
  full.data <- read.csv("../CausalSimPy/data/5d.csv")

  data <- full.data[sample(1:nrow(full.data), train.sample.size),]
  missing.data <- drop.data.cells(data, 0.3) # drop some cells in the data frame

  dataset <- DataSet$new(missing.data) # create a dataset object
  dataset$fill_missing()  # impute missing data
  #dataset$drop_missing() # drop mising rows

  sim <- CausalSimModel$new(dataset) # create simulation model
  sim$learn_structure() # learn the causal structure
  sim$learn_samplers() # learn the conditional samplers
  sim$plot() # plot the structure

  filled.gibbs <- sim$fill_gibbs(missing.data) #fill in missing data
  df <- sim$sample(100) #sample a 100 rows from the model
  
  sim$structure_from_json_string(json.string) # update the structure
  sim$learn_samplers() # re-learn the conditional samplers
  df <- sim$sample(100)
```
