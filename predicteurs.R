classifieur_astronomie <- function(dataset){
  load("env_astronomy.RData")
  dataset$objid <- NULL
  dataset$rerun <- NULL
  scaled_dataset <- t(apply(dataset, 1, function(r)(r - centers) / scales ))
  z <- predict(astr.mod, scaled_dataset)
  return(z)
}