\name{cvmf_object}
\alias{cvmf_object}

\title{Cross-Validated Median Fit (CVMF) Object}

\description{
This class of objects is returned by the \code{\link{cvmf}} function to test between the partial likelihood maximization (PLM) and the iteratively reweighted robust (IRR) method of estimation for a given application of the Cox model.
}

\value{

The following components must be included in a legitimate \code{cvmf} object.

\item{best}{name of the model of estimation favored by the cvmf test.}

\item{p}{p-value of the binomial test used to test between estimation models.}

\item{cvmf}{full output of the binomial test used to test between estimation methods. See documentation for \code{\link{binom.test}} for more information.}

\item{coef_names}{names of the coefficients.}

\item{irr}{full output for the iteratively reweighted robust (IRR) method of estimating the Cox model. See documentation for \code{\link[coxrobust]{coxr}} in the package coxrobust for more information.}

\item{plm}{full output for the partial likelihood maximization (PLM) method of estimating the Cox model. See documentation for \code{\link[survival]{coxph}} in the package survival for more information.}

\item{irr_coefs}{estimates obtained from IRR method of estimating the Cox model. See documentation for \code{\link[coxrobust]{coxr}} in the package coxrobust for more information.}

\item{plm_coefs}{estimates obtained from PLM method of estimating the Cox model. See documentation for \code{\link[survival]{coxph}} in the package survival for more information.}

\item{cvpl_irr}{observation-wise contributions to the log-partial likelihood for IRR method of estimating the Cox model. See Desmarais and Hardin (Political Analysis 20:113-135, 2012) for more about the test and Verweij and Houwelingen (Statistics in Medicine 12(24): 2305–14, 1993) for more about the measure}

\item{cvpl_plm}{observation-wise contributions to the log-partial likelihood for PLM method of estimating the Cox model. See Desmarais and Hardin (Political Analysis 20:113-135, 2012) for more about the test and Verweij and Houwelingen (Statistics in Medicine 12(24): 2305–14, 1993) for more about the measure}

The object also contain the following: \code{call}, \code{x}, and \code{y}.

}

\seealso{
\code{\link{cvmf}}
}

\keyword{cvmf}
\keyword{cox}
