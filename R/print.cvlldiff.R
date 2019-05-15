
print.cvlldiff <- function(x, digits = max(3, getOption("digits") - 4), ...) {

  if(!inherits(x, "cvlldiff")) {
    stop("use only with \"cvlldiff\" objects")
  }

  op <- options(digits = digits)
  on.exit(options(op))

  if(class(x$p_value) != "character") {
    cat("The estimation used to create ", x$best, " is supported with a p-value of ",
        x$p_value, sep = "", "\n")

  } else {
    cat("The estimation used to create ", x$best, " is supported.\n",
    "Please rerun function with degrees of freedom for a p-value.",
        sep = "", "\n")
  }

  invisible(x)

}
