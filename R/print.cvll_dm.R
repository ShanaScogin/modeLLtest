
print.cvll_dm <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvll_dm")) {
    stop("use only with \"cvll_dm\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("The estimation used to create ", x$best, " is supported with a p-value of ",
      x$p_value, sep = "", "\n")

#  cat("\n", x$test_stat)

  invisible(x)

}
