\name{cvdm_object}
\alias{cvdm_object}

\title{Cross-Validated Difference in Means (CVDM) Object}

\description{
This class of objects is returned by the \code{\link{cvdm}} function to compare two methods of estimating a formula.
}

\value{

The following components must be included in a legitimate \code{cvdm} object.

\item{best}{name of the estimation method favored by the cvdm test.}

\item{test_stat}{object returned by the bias-corrected Johnson's t-test. A positive test statistic supports method 1 and a negative test statistic supports method 2.}

\item{p_value}{p-value for the test statistic.}

\item{n}{number of observations.}

\item{df}{degrees of freedom.}

The object also contain the following: \code{call}, \code{x}, and \code{y}. See \code{\link{lm}} documentation for more.

}

\seealso{
\code{\link{cvdm}}
}

\keyword{cvdm}
