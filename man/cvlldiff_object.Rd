\name{cvlldiff_object}
\alias{cvlldiff_object}

\title{Cross-Validated Difference in Means (CVDM) Object from General \code{\link{cvlldiff}} Function}

\description{
This class of objects is returned by the \code{\link{cvlldiff}} function to compare vectors of cross-validated log-likelihood values.
}

\value{

The following components must be included in a legitimate \code{cvlldiff} object.

\item{best}{name of the estimation method favored by the cvdm test.}

\item{test_stat}{object returned by the bias-corrected Johnson's t-test. A positive test statistic supports the method that generated the first vector of cross-validated log-likelihood values and a negative test statistic supports the method that generated the second vector.}

\item{p_value}{p-value for the test statistic.}

}

\seealso{
\code{\link{cvlldiff}}
}

\keyword{cvlldiff}
\keyword{cvdm}
