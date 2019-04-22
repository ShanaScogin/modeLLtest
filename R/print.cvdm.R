
print.cvdm <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvdm")) {
    stop("use only with \"cvdm\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  cat(x$best, " supported with a p-value of ",
      x$p_value, sep = "", "\n")

#  cat("\n", x$test_stat)

  invisible(x)

}
