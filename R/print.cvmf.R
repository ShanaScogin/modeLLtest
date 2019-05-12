
print.cvmf <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvmf")) {
    stop("use only with \"cvmf\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  cat(x$best, " supported with a two-sided p-value of ",
      x$p_value, sep = "", "\n")

  cat("\nSuccesses: ", x$cvmf$statistic, " out of ", x$cvmf$parameter, " trials",
      "\np-value: ",x$p_value,
      ", 95 percent confidence interval: ", x$cvmf$conf.int[c(1)], " ", x$cvmf$conf.int[c(2)],
      "\nProbability of success: ", x$cvmf$statistic / x$cvmf$parameter, "\n", sep = "")

  sd_plm <- sqrt(diag(matrix(c(unlist(x$plm$var)),
                      ncol = length(x$coef),
                      byrow = TRUE)))
  sd_irr <- sqrt(diag(matrix(c(unlist(x$irr$var)),
                      ncol = length(x$coef),
                      byrow = TRUE)))
  df <- sum(!is.na(x$irr_coefs[[1]]))

  tmp <- cbind(as.numeric(x$plm_coefs[[1]]), exp(as.numeric(x$plm_coefs[[1]])), sd_plm,
               2 * (1 - pnorm(abs(as.numeric(x$plm_coefs[[1]]) / sd_plm))))
  rownames(tmp) <- unlist(x$coef)
  colnames(tmp) <- c("coef", "exp(coef)", "se(coef)", "p")

  cat("\nPartial likelihood estimator\n")
  print(tmp)
  cat("\nWald test = ", x$plm$wald.test, " on ", df, " df,", " p = ",
      1 - pchisq(as.numeric(x$plm$wald.test), df), "\n", sep="")

  tmp <- cbind(as.numeric(x$irr_coefs[[1]]), exp(as.numeric(x$irr_coefs[[1]])), sd_irr,
               2 * (1 - pnorm(abs(as.numeric(x$irr_coefs[[1]]) / sd_irr))))
  row.names(tmp) <- unlist(x$coef)
  colnames(tmp) <- c("coef", "exp(coef)", "se(coef)", "p")

  cat("\nRobust estimator\n")
  print(tmp)
  cat("\nExtended Wald test = ", x$irr$wald.test, " on ", df, " df,", " p = ",
      1 - pchisq(as.numeric(x$irr$wald.test), df), "\n", sep="")

  invisible(x)

}
