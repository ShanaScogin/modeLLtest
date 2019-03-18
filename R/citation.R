.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ModeLLtest: Compare models with Cross Validated Log Likelihood
                        Shana Scogin, University of Notre Dame
                        Sarah Petersen, University of Notre Dame
                        Bruce A. Desmarais, Penn State University
                        Jeff Harden, University of Notre Dame
                        Type help('modeLLtest') to get started.
                        Development website: https://github.com/ShanaScogin/modeLLtest")
}

repo <- git2r::repository(".", discover = TRUE)
