rlm_fit <- function(x, y){
  x <- x[, -1]
  obj <- MASS::rlm(y ~ x, data = as.data.frame(cbind(x, y)))
  obj
}
