classifieur_astronomie <- function(dataset){
  load("env_astronomy.RData")
  dataset$objid <- NULL
  dataset$rerun <- NULL
  scaled_dataset <- t(apply(dataset, 1, function(r)(r - centers) / scales ))
  z <- predict(astr.mod, scaled_dataset)
  return(z)
}

library(keras)
classifieur_images <- function(list) {
  model <- load_model_hdf5("model.h5")  
  n<-length(list)
  dim_images <- c(100,100)
  z <- rep(0, length(list))
  for(i in 1:n){
    images <- array(0, dim=c(2, dim_images[1], dim_images[2], 3))
    images[1,,,] <- image_to_array(image_load(path=list[i], target_size=dim_images, interpolation = "bilinear"))
    temp <- predict(model, images[,,,], batch_size = 2)[1,]
    z[i] <- which.max(temp)
  }
  z[z==1] <- 'car'
  z[z==2] <- 'cat'
  z[z==3] <- 'flower'
  return(as.factor(z))
}

rendement_mais <- function(dataset) {
  load("env_mais.RData")
  dataset$X <- NULL
  scaled_dataset <- t(apply(dataset, 1, function(r)(r - centers) / scales ))
  z <- predict(mais.mod, scaled_dataset)
  return(z)
}
