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
  test_num <- obj_rlm_m[[1]][[1]]
  check_against <- c(-3.809918)
  expect_equal(round(test_num, 6), check_against)

  test_num2 <- obj_rlm_m[[2]][[3]]
  check_against <- c(-2.653222)
  expect_equal(round(test_num2, 6), check_against)

})




