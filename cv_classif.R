require(MASS)
library(e1071)
library(class)

#fonction réalisant la validation croisée (avec K=10 par défaut) 
CV_eval <- function(model, data, hyperparameters=c(), fold=10){
  K <- fold
  n <- nrow(data)
  folds <- sample(1:K,n,replace=TRUE)
  mean <- 0
  errors <- rep(0, K)
  for(k in (1:K)){
    if ((model=='adl') || (model=='adq')){
      if (model=='adl'){
        model.fit <- lda(formula=as.factor(y)~., data=data[folds!=k,])
      }else{
        model.fit <- qda(formula=as.factor(y)~., data=data[folds!=k,])
      }
      pred <- predict(model.fit, data[folds==k,])
      errors[k] <- sum(data$y[folds==k]!=pred$class) / length(pred$class)
      mean <- mean + (errors[k]*length(pred$class))
    }else if ((model=='bayesien_naif') || (model=='reg_logistique')){
      if (model=='bayesien_naif'){
        model.fit <- naiveBayes(formula=as.factor(y)~., data=data[folds!=k,])
        pred <- predict(model.fit, data[folds==k,])
      }else{
        glm.fit <- glm(as.factor(y)~., data=data[folds!=k,], family=binomial)
        pred <- predict(glm.fit, newdata=data[folds==k,], type='response')>0.5
      }
      errors[k] <- sum(data$y[folds==k]!=pred) / length(pred)
      mean <- mean + (errors[k]*length(pred))
    }else if (model=='knn'){
      neigh <- hyperparameters$K
      train <- data[folds!=k, -which(names(data)=="y")]
      train$y <- NULL 
      cl <- as.factor(data[folds!=k,]$y)
      test <- data[folds==k, -which(names(data)=="y")]
      test$y <- NULL
      model.pred <- knn(train=train, test=test, cl=cl, k=neigh)
      errors[k] <- sum(data$y[folds==k]!=model.pred) / length(model.pred)
      mean <- mean + (errors[k]*length(model.pred))
    }else{
      stop('Le modèle demandé n\'est pas implémenté!')
    }
  }
  mean <- mean/n
  sd <- sqrt((1/(K-1))*sum((errors - rep(mean, K))^2))
  return(c(mean, sd))
}

