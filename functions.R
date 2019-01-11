ForwardCrossValidationLM <- function(data){
  p <- dim(data)[2] - 1
  K <- 10
  set.seed(123)
  folds <- sample(1:K, nrow(data), replace=TRUE)
  
  CV <- rep(0,p)
  vars <- names(data[,-which(names(data)=="yield_anomaly")])
  varsres <- rep(0,p)
  ftest <- rep(0,p)
  m <- ""
  for(i in 1:p) {
    temp <- rep(0,length(vars))
    for(j in 1:length(vars)){
      f <- as.formula(paste("yield_anomaly ~", m, "+", vars[j]))
      for(k in (1:K)) {
        reg <- lm(f, data=data[folds!=k,])
        pred <- predict(reg, data[folds==k,])
        temp[j] <- temp[j] + sum((data$yield_anomaly[folds==k] - pred)^2)
      }
    }
    if (i != p && i != 1)
      m <- paste(m, " +", vars[which.min(temp)], sep="")
    if (i == 1)
      m <- paste(m, vars[which.min(temp)], sep="")
    varsres[i] <- vars[which.min(temp)]
    vars <- vars[-which.min(temp)]
    CV[i] <- min(temp)/nrow(data)
  }
  res <- NULL
  
  res$CV <- CV
  res$m <- m
  res$vars <- varsres
  return(res)
}

build_model <- function() {
  model <- keras_model_sequential() 
  model %>% layer_dense(units = 5, activation = 'relu', input_shape = ncol(mais_train)-1) %>%
    layer_dense(units = 1, name="sortie")
  
  model %>% compile(loss = 'mean_squared_error', optimizer = optimizer_rmsprop())
}

build_perceptron <- function() {
  
  model <- build_model()
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
  
  epochs <- 100
  
  # Fit the model and store training stats
  history <- model %>% fit(
    as.matrix(mais_train[,-which(names(mais_test)=="yield_anomaly")]),
    mais_train$yield_anomaly,
    epochs = epochs,
    validation_split = 0.2,
    verbose=0,
    callbacks = list(early_stop)
  )
  return(history)
}

optimizeMtryAndPlotResults <- function(){
  if(RECHERCHE_HYPERPARAMETRE) {
    oob.err <- numeric(length(mais_train)-1)
    test.err <- numeric(length(mais_train)-1)
    
    for(mtry in 1:(length(mais_train)-1)) {
      rf <- randomForest(
        yield_anomaly ~ .,
        data = mais_train,
        mtry=mtry,
        xtest = scaled_test, 
        ytest = mais_test$yield_anomaly,
        ntree=nbtree
      )
      oob.err[mtry]= rf$mse[nbtree]
      test.err[mtry]= rf$test$mse[nbtree]
    }
    
  } else {
    load("env_mais.RData")
  }
  tibble::tibble(
    'Out of Bag Error' = oob.err,
    'Test error' = test.err,
    mtries = 1:(length(mais_train)-1)
  ) %>%
    gather(Metric, MSE, -mtries) %>%
    ggplot(aes(mtries, MSE, color = Metric)) +
    geom_line() +
    scale_y_continuous() +
    xlab("Mtry")
  mtry <- which.min(oob.err)
  cat("Param?tre mtry optimal: ", mtry)
  
  res <- NULL
  res$mtry <- mtry
  res$oob.err <- oob.err
  res$test.err <- test.err
  return(res)
}

optimizeNtreeAndPlotResults <- function() {
  rf <- randomForest(
    formula = yield_anomaly ~ .,
    data = mais_train,
    xtest = scaled_test, 
    ytest = mais_test$yield_anomaly,
    ntree = 1000
  )
  tibble::tibble(
    'Out of Bag Error' = rf$mse,
    'Test error' = rf$test$mse,
    ntrees = 1:rf$ntree
  ) %>%
    gather(Metric, MSE, -ntrees) %>%
    ggplot(aes(ntrees, MSE, color = Metric)) +
    geom_line() +
    scale_y_continuous() +
    xlab("Number of trees")
  nbtree <- which.min(rf$mse)
  cat("Nombre d'arbres optimal: ",nbtree)
  
  return(nbtree)
}
