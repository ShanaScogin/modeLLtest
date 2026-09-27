#' Summarizing modeLLtest Results
#'
#' \code{summary} methods for objects returned by \code{\link{cvdm}},
#' \code{\link{cvll}}, \code{\link{cvlldiff}}, and \code{\link{cvmf}}.
#' They collect the test results stored in the object into a list that can
#' be printed or used programmatically. No models are re-estimated.
#'
#' @param object an object of class \code{cvdm}, \code{cvll},
#'   \code{cvlldiff}, or \code{cvmf}.
#' @param x an object returned by one of the \code{summary} methods.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of class \code{summary.cvdm}, \code{summary.cvll},
#'   \code{summary.cvlldiff}, or \code{summary.cvmf}. Each is a list that
#'   contains the \code{call} (except for \code{cvlldiff}), the preferred
#'   method (\code{best}) where relevant, and a \code{table} of the test
#'   results. \code{summary.cvmf} also contains \code{plm_table} and
#'   \code{irr_table}, the coefficient tables for the partial likelihood
#'   and robust estimators.
#'
#' @seealso \code{\link{cvdm_object}}, \code{\link{cvll_object}},
#'   \code{\link{cvlldiff_object}}, \code{\link{cvmf_object}}
#'
#' @examples
#' \donttest{
#' set.seed(123456)
#' X <- runif(200, -1, 1)
#' Y <- 0.2 + 0.5 * X + rnorm(200)
#' obj_cvdm <- cvdm(Y ~ X, data.frame(cbind(Y, X)),
#'                  method1 = "OLS", method2 = "MR")
#' summary(obj_cvdm)
#' }
#' @name summary.modeLLtest
NULL

#' @rdname summary.modeLLtest
#' @export
summary.cvdm <- function(object, ...) {

  tab <- cbind(object$test_stat, object$df, object$p_value)
  dimnames(tab) <- list("", c("Johnson's t", "df", "p-value"))

  ans <- list(call = object$call,
              best = object$best,
              n = object$n,
              table = tab)
  class(ans) <- "summary.cvdm"
  ans
}

#' @rdname summary.modeLLtest
#' @export
print.summary.cvdm <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Cross-validated difference in means (CVDM) test\n")
  cat("Observations: ", x$n, "\n\n", sep = "")
  print(signif(x$table, digits))
  cat("\nPreferred method: ", x$best, "\n", sep = "")
  cat("(Positive test statistics support method1; negative support method2.)\n")

  invisible(x)
}

#' @rdname summary.modeLLtest
#' @export
summary.cvll <- function(object, ...) {

  tab <- summary(object$cvll)

  ans <- list(call = object$call,
              method = object$method,
              n = object$n,
              df = object$df,
              total = sum(object$cvll),
              table = tab)
  class(ans) <- "summary.cvll"
  ans
}

#' @rdname summary.modeLLtest
#' @export
print.summary.cvll <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Leave-one-out cross-validated log-likelihoods\n")
  cat("Method: ", x$method, "\n", sep = "")
  cat("Observations: ", x$n, ",  degrees of freedom: ", x$df, "\n\n", sep = "")
  print(x$table, digits = digits)
  cat("\nSum of cross-validated log-likelihoods: ",
      format(x$total, digits = digits), "\n", sep = "")

  invisible(x)
}

#' @rdname summary.modeLLtest
#' @export
summary.cvlldiff <- function(object, ...) {

  ans <- list(best = object$best,
              test_stat = object$test_stat,
              p_value = object$p_value)
  class(ans) <- "summary.cvlldiff"
  ans
}

#' @rdname summary.modeLLtest
#' @export
print.summary.cvlldiff <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  cat("\nBias-corrected Johnson's t-test on the difference between two vectors\n")
  cat("of cross-validated log-likelihoods\n\n")
  cat("Johnson's t: ", format(x$test_stat, digits = digits), "\n", sep = "")
  if (is.numeric(x$p_value)) {
    cat("p-value: ", format(x$p_value, digits = digits), "\n", sep = "")
  } else {
    cat("p-value: not available (rerun cvlldiff() with df for a p-value)\n")
  }
  cat("\nPreferred: ", x$best, "\n", sep = "")
  cat("(Positive test statistics support the first vector; negative support the second.)\n")

  invisible(x)
}

#' @rdname summary.modeLLtest
#' @export
summary.cvmf <- function(object, ...) {

  k <- length(object$coef_names)
  coef_table <- function(fit) {
    b <- as.numeric(fit$coefficients)
    se <- sqrt(diag(matrix(unlist(fit$var), ncol = k, byrow = TRUE)))
    tab <- cbind(b, exp(b), se, 2 * (1 - pnorm(abs(b / se))))
    dimnames(tab) <- list(object$coef_names,
                          c("coef", "exp(coef)", "se(coef)", "p"))
    tab
  }
  df <- sum(!is.na(object$irr$coefficients))

  successes <- as.numeric(object$cvmf$statistic)
  trials <- as.numeric(object$cvmf$parameter)
  tab <- cbind(successes, trials, successes / trials,
               object$cvmf$conf.int[1], object$cvmf$conf.int[2],
               object$cvmf$p.value)
  dimnames(tab) <- list("", c("IRR better", "n", "proportion",
                              "95% lower", "95% upper", "p-value"))

  wald <- rbind(c(object$plm$wald.test, df,
                  1 - pchisq(as.numeric(object$plm$wald.test), df)),
                c(object$irr$ewald.test, df,
                  1 - pchisq(as.numeric(object$irr$ewald.test), df)))
  dimnames(wald) <- list(c("PLM Wald", "IRR extended Wald"),
                         c("statistic", "df", "p-value"))

  ans <- list(call = object$call,
              best = object$best,
              table = tab,
              plm_table = coef_table(object$plm),
              irr_table = coef_table(object$irr),
              wald = wald)
  class(ans) <- "summary.cvmf"
  ans
}

#' @rdname summary.modeLLtest
#' @export
print.summary.cvmf <- function(x, digits = max(3, getOption("digits") - 3), ...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Cross-validated median fit (CVMF) test\n")
  cat("(Binomial test of how often IRR has the higher cross-validated partial likelihood)\n\n")
  print(signif(x$table, digits))
  cat("\nPreferred method: ", x$best, "\n", sep = "")

  cat("\nPartial likelihood maximization (PLM)\n")
  print(signif(x$plm_table, digits))
  cat("\nIteratively reweighted robust (IRR)\n")
  print(signif(x$irr_table, digits))
  cat("\n")
  print(signif(x$wald, digits))

  invisible(x)
}
