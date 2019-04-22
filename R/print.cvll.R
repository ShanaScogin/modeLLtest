#
# print.cvll <- function(x, digits = max(3, getOption("digits") - 4), ...) {
#
#   if(!inherits(x, "cvll")) {
#     stop("use only with \"cvll\" objects")
#   }
#
#   op <- options(digits = digits)
#   on.exit(options(op))
#
#   cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
#
#   cat("A vector of ", x$n,
#       " leave-one-out cross-validated log-likelihoods was created", sep = "", "\n")
#
#   x$test_stat
#
#   invisible(x)
#
# }
