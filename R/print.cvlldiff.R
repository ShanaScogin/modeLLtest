
print.cvlldiff <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvlldiff")) {
    stop("use only with \"cvlldiff\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("The estimation used to create ", x$best, " is supported with a p-value of ",
      x$p_value, sep = "", "\n")

  #  cat("\n", x$test_stat)

  invisible(x)

}
