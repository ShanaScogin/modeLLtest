test_that("Simple model with cvdm runs", {
#  skip("Travis erroring for some reason - runs on local")

  data(nepaldem)
  nepaldem <- na.omit(nepaldem)
  Y <- nepaldem$percent_regvote1999
  X <- cbind(nepaldem$landless_gap, nepaldem$below1pa_gap, nepaldem$sharecrop_gap,
    nepaldem$service_gap, nepaldem$fixmoney_gap, nepaldem$fixprod_gap,
    nepaldem$per_without_instcredit, nepaldem$totoalkilled_1000,
    nepaldem$hdi_gap1, nepaldem$ln_pop2001, nepaldem$totalcontestants1999,
    nepaldem$cast_eth_fract)
  ones <- rep(1, length(Y))
  X <- cbind(ones, X)

  obj_rlm_mm <- cvll_rlm_mm(X, Y, length(Y), ncol(X))
  test_num <- as.numeric(obj_rlm_mm[1])
  check_against <- c(-2.800588)
  expect_equal(round(test_num, 6), check_against)

  test_num2 <- as.numeric(obj_rlm_mm[2])
  check_against <- c(-4.027866)
  expect_equal(round(test_num2, 6), check_against)

  X <- X[, -1]
  obj_cvdm_rr <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "RLM-MM")
  test_stat <- as.numeric(obj_cvdm_rr$test_stat)
  check_against <- c(-2.097227)
  expect_equal(round(test_stat, 6), check_against)

})




