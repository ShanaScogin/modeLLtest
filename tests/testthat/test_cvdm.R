test_that("Simple model with cvdm runs", {

  set.seed(123456)
  b0 <- .2 # True value for the intercept
  b1 <- .5 # True value for the slope
  n <- 500 # Sample size
  X <- runif(n, -1, 1)

  Y <- b0 + b1 * X + rnorm(n, 0, 1) # N(0, 1 error)

  obj_cvdm <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "MR")
  test_stat <- obj_cvdm$test_stat
  check_against <- c(3.45354)
  expect_equal(round(as.numeric(test_stat), 5),
               check_against)

  test_pval <- obj_cvdm$p_value
  check_against <- c(0.00030)
  expect_equal(round(as.numeric(test_pval), 6),
               check_against)

})




