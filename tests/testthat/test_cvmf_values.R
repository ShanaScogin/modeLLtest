# SRS NOTE SEPT 2026: I'm adding this here before I start making changes to
# cvmf to optimize code and runtime

test_that("cvmf cross-validated values are unchanged (README example)", {

  set.seed(12345)
  x1 <- rnorm(100)
  x2 <- rnorm(100)
  x2e <- x2 + rnorm(100, 0, 0.5)
  y <- survival::Surv(rexp(100, exp(x1 + x2)))
  dat <- data.frame(y, x1, x2e)

  results <- cvmf(y ~ x1 + x2e, data = dat)

  expect_equal(sum(results$cvpl_irr), -421.276804, tolerance = 1e-6)
  expect_equal(sum(results$cvpl_plm), -421.025224, tolerance = 1e-6)
  expect_equal(results$cvpl_irr[1], -4.380013, tolerance = 1e-6)
  expect_equal(results$cvpl_plm[1], -4.373070, tolerance = 1e-6)
})

test_that("cvmf cross-validated values are unchanged with ties and censoring", {
  skip_on_cran()

  set.seed(3)
  n <- 120
  z1 <- rnorm(n)
  z2 <- rbinom(n, 1, 0.4)
  tt <- round(rexp(n, exp(0.6 * z1)), 1) + 0.1 # rounded to create ties
  st <- rbinom(n, 1, 0.75)                     # about 25% censored
  dat <- data.frame(tt, st, z1, z2)

  efron <- cvmf(survival::Surv(tt, st) ~ z1 + z2, data = dat)
  expect_equal(sum(efron$cvpl_irr), -386.928485, tolerance = 1e-6)
  expect_equal(sum(efron$cvpl_plm), -386.587320, tolerance = 1e-6)
  expect_equal(as.numeric(efron$cvmf$statistic), 64)

  breslow <- cvmf(survival::Surv(tt, st) ~ z1 + z2, data = dat,
                  method = "breslow")
  expect_equal(sum(breslow$cvpl_irr), -389.167420, tolerance = 1e-6)
  expect_equal(sum(breslow$cvpl_plm), -388.781649, tolerance = 1e-6)
  expect_equal(as.numeric(breslow$cvmf$statistic), 67)
})
