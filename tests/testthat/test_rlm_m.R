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

  obj_rlm_m <- cvll_rlm_m(X, Y, length(Y), ncol(X))
  # test_stat <- obj_cvdm$test_stat
  # check_against <- c(3.45354)
  # expect_equal(round(as.numeric(test_stat), 5),
  #              check_against)
  #
  # test_pval <- obj_cvdm$p_value
  # check_against <- c(0.00030)
  # expect_equal(round(as.numeric(test_pval), 6),
  #              check_against)

})




