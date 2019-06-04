test_that("Simple model with two covariates runs with cvmf", {

  set.seed(12345)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x2e <- x2 + rnorm(100, 0, 0.5)

  y <- rexp(100, exp(x1 + x2))
  y <- survival::Surv(y)

  dat <- data.frame(y, x1, x2e)
  form <- y ~ x1 + x2e

  results <- cvmf(formula = form, data = dat)

  stat <- as.numeric(results$cvmf$statistic)
  check_against <- c(62)
  expect_equal(stat, check_against)

  best <- results$best[[1]]
  check_against <- c("IRR")
  expect_equal(best, check_against)

  p <- as.numeric(results$p_value)
  check_against <- c(0.021)
  expect_equal(p, check_against)

  coef <- as.numeric(results$irr_coefs[[1]][c(1)])
  check_against <- c(0.9641172)
  expect_equal(round(coef, 7), check_against)

  coef <- as.numeric(results$plm_coefs[[1]][c(1)])
  check_against <- c(0.9253325)
  expect_equal(coef, check_against)

})

test_that("Testing NAs", {
  skip_on_cran() # for when submit to cran

  set.seed(12345)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x2e <- x2 + rnorm(100, 0, 0.5)

  y <- rexp(100, exp(x1 + x2))
  y <- survival::Surv(y)

  dat <- data.frame(y, x1, x2e)
  set.seed(12345)
  dat <- as.data.frame(lapply(dat,
                       function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15),
                                               size = length(cc), replace = TRUE) ]))
  form <- y ~ x1 + x2e

  results <- cvmf(formula = form, data = dat)

  stat <- as.numeric(results$cvmf$statistic)
  check_against <- c(37)
  expect_equal(stat, check_against)

  best <- results$best[[1]]
  check_against <- c("IRR")
  expect_equal(best, check_against)

})

test_that("Simple test with na.action and no nas", {
  skip_on_cran() # for when submit to cran

  set.seed(12345)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x2e <- x2 + rnorm(100, 0, 0.5)

  y <- rexp(100, exp(x1 + x2))
  y <- survival::Surv(y)

  dat <- data.frame(y, x1, x2e)
  form <- y ~ x1 + x2e

  results <- cvmf(formula = form, data = dat, na.action = na.fail)

  stat <- as.numeric(results$cvmf$statistic)
  check_against <- c(62)
  expect_equal(stat, check_against)

  best <- results$best[[1]]
  check_against <- c("IRR")
  expect_equal(best, check_against)

  p <- as.numeric(results$p_value)
  check_against <- c(0.021)
  expect_equal(p, check_against)

  coef <- as.numeric(results$irr_coefs[[1]][c(1)])
  check_against <- c(0.9641172)
  expect_equal(round(coef, 7), check_against)

  coef <- as.numeric(results$plm_coefs[[1]][c(1)])
  check_against <- c(0.9253325)
  expect_equal(coef, check_against)

})
