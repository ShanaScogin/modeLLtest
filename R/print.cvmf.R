
print.cvmf <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvmf")) {
    stop("use only with \"cvmf\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  cat(x$best, " supported with a two-sided p-value of ",
      x$p, sep = "", "\n")

  invisible(x)

}
