rlm.fit <- function(x, y){
  obj <- MASS::rlm(y ~ x, data = as.data.frame(cbind(x, y)))
  obj
}
