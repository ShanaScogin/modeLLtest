test_that("Simple model with cvll runs", {
  skip("Travis erroring for some reason - runs on local")

  set.seed(123456)
  b0 <- .2 # True value for the intercept
  b1 <- .5 # True value for the slope
  n <- 500 # Sample size
  X <- runif(n, -1, 1)

  Y <- b0 + b1 * X + rnorm(n, 0, 1) # N(0, 1 error)

  obj_cvll <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "OLS")

})


