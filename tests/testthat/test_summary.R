test_that("summary methods return the stored results", {

  set.seed(123456)
  X <- runif(100, -1, 1)
  Y <- 0.2 + 0.5 * X + rnorm(100)
  dat <- data.frame(cbind(Y, X))

  obj_cvdm <- cvdm(Y ~ X, dat, method1 = "OLS", method2 = "MR")
  s <- summary(obj_cvdm)
  expect_s3_class(s, "summary.cvdm")
  expect_equal(s$table[1, "Johnson's t"], as.numeric(obj_cvdm$test_stat))
  expect_equal(s$table[1, "p-value"], as.numeric(obj_cvdm$p_value))
  expect_output(print(s), "Preferred method")

  obj_ols <- cvll(Y ~ X, dat, method = "OLS")
  obj_mr <- cvll(Y ~ X, dat, method = "MR")
  s <- summary(obj_ols)
  expect_s3_class(s, "summary.cvll")
  expect_equal(s$total, sum(obj_ols$cvll))
  expect_output(print(s), "OLS")

  s <- summary(cvlldiff(obj_ols$cvll, obj_mr$cvll, obj_ols$df))
  expect_s3_class(s, "summary.cvlldiff")
  expect_output(print(s), "p-value: [0-9]")
  expect_output(print(summary(cvlldiff(obj_ols$cvll, obj_mr$cvll))),
                "not available")
})

test_that("summary.cvmf returns coefficient tables", {

  skip_on_cran()
  set.seed(1)
  x1 <- rnorm(50)
  y <- survival::Surv(rexp(50, exp(0.5 * x1)))
  obj_cvmf <- cvmf(y ~ x1, data = data.frame(x1))
  s <- summary(obj_cvmf)
  expect_s3_class(s, "summary.cvmf")
  expect_equal(unname(s$plm_table[, "coef"]),
               unname(obj_cvmf$plm$coefficients))
  expect_equal(unname(s$irr_table[, "coef"]),
               unname(obj_cvmf$irr$coefficients))
  expect_output(print(s), "IRR extended Wald")
})
