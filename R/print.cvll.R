
print.cvll <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvll")) {
    stop("use only with \"cvll\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")

  max <- max(x$cvll)
  min <- min(x$cvll)
  med <- median(x$cvll)
  mean <- mean(x$cvll)

  cat("A vector of ", x$n,
      " leave-one-out cross-validated log-likelihoods was created.", sep = "", "\n")

  cat("\nSummary stats\n")
  tmp <- cbind(max, min, med, mean)
  rownames(tmp) <- c("")
  colnames(tmp) <- c("max", "min", "med", "mean")

  print(tmp)

  invisible(x)

}
