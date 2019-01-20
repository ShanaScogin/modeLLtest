test_that("Simple model with two covariates runs with cvmf", {
  skip_on_cran()
  ## need to figure out why skip_on_cran() is skipping and failing crap

  set.seed(12345)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x2e <- x2 + rnorm(100, 0, 0.5)

  y <- rexp(100, exp(x1 + x2))
  y <- survival::Surv(y)

  dat <- data.frame(y, x1, x2e)
  form <- y ~ x1 + x2e

  results <- cvmf(formula = form, data = dat)

  stat <- as.numeric(results$cvmf_stat[[1]])
  check_against <- c(62)
  expect_equal(stat, check_against)

  best <- results$best[[1]]
  check_against <- c("IRR")
  expect_equal(best, check_against)

  p <- as.numeric(results$p_value[[1]])
  check_against <- c(0.021)
  expect_equal(p, check_against)

  p <- as.numeric(results$cvmf_p[[1]])
  check_against <- c(0.02097874)
  expect_equal(p, check_against)

  coef <- as.numeric(results$irr_coefs[[1]][c(1)])
  check_against <- c(0.9641172)
  expect_equal(round(coef, 7), check_against)

  coef <- as.numeric(results$plm_coefs[[1]][c(1)])
  check_against <- c(0.9253325)
  expect_equal(coef, check_against)

})

