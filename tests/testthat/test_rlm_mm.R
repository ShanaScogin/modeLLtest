test_that("RLM-mm and cvdm with rlm-mm runs", {

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

  set.seed(9827)
  obj_rlm_mm <- cvll_rlm_mm(X, Y, length(Y), ncol(X))
  test_num <- as.numeric(obj_rlm_mm[1])
  check_against <- c(-2.752584)
  expect_equal(round(test_num, 6), check_against)

  test_num2 <- as.numeric(obj_rlm_mm[2])
  check_against <- c(-4.011667)
  expect_equal(round(test_num2, 6), check_against)

  X <- X[, -1]
  set.seed(97276)
  obj_cvdm_rr <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "RLM-MM")
  test_stat <- as.numeric(obj_cvdm_rr$test_stat)
  check_against <- c(-2.414619)
  expect_equal(round(test_stat, 6), check_against)

})




