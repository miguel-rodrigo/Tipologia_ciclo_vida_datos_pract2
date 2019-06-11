test_bench <- function(train.data1, train.data2, label.colname, verbose=TRUE, n.folds=10, n.repeats=4){
  require(data.table)
  require(caret)
  
  # if(!is.factor(train.data1[, get(label.colname)])){
  #   train.data1[, label.colname := as.factor(get(label.colname))]
  # }
  
  trControl <- trainControl(
    method = 'repeatedcv',
    number = n.folds,
    repeats = n.repeats,
    verboseIter = verbose,
    savePredictions = 'final'
  )
  
  tuneGrid <- expand.grid(
    mtry = 2:4,
    splitrule = c('gini', 'extratrees'),
    min.node.size = c(3, 5, 10)
  )
  
  # browser()
  
  trainer <- function(data){
    if("train" %in% class(data)){
      return(data)
    } else {
      train(
        as.formula(paste0(label.colname, "~.")),
        data = data,
        method = 'ranger',
        trControl = trControl,
        tuneGrid = tuneGrid,
        importance = 'impurity',
        num.trees = 500,
        verbose = T
      )
    }
  }
  
  print(paste("Training for", as.character(n.folds), "folds, ", as.character(n.repeats), "times:"))
  
  models <- lapply(list(train.data1, train.data2), trainer)
  
  return(models)
}

test_bench.assess <- function(models){
  results <- lapply(models, function(m){
    p <- as.data.table(m$pred)
    bt <- m$bestTune
    
    return(p[mtry==bt$mtry & min.node.size==bt$min.node.size, .(Accuracy=sum(pred==obs)/.N), by=Resample])
  })
  
  return(t.test(results[[1]]$Accuracy, results[[2]]$Accuracy))
}