test_that("RLM-m and cvdm with rlm runs", {

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

  obj_rlm_m <- cvll_rlm_m(X, Y, length(Y), ncol(X))
  test_num <- as.numeric(obj_rlm_m[1])
  check_against <- c(-2.852842)
  expect_equal(round(test_num, 6), check_against)

  test_num2 <- as.numeric(obj_rlm_m[2])
  check_against <- c(-3.881559)
  expect_equal(round(test_num2, 6), check_against)

  X <- X[, -1]
  obj_cvdm_rr <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "RLM")
  test_stat <- as.numeric(obj_cvdm_rr$test_stat)
  check_against <- c(-2.363149)
  expect_equal(round(test_stat, 6), check_against)

})




