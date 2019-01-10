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