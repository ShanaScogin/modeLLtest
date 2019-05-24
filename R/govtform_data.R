#' Data from Golder (2010) on government formation in Western Europe
#'
#' Data from a study on Western European government formation
#' duration. Data is at the country-level (N = 409).
#'
#' @docType data
#'
#' @usage data(govtform)
#'
#' @format A data frame with 410 rows and 18 variables:
#' \describe{
#'  \item{countryname}{names of countries used in analysis}
#'  \item{country}{unique number identifying each country}
#'  \item{cabinet}{unique number identifying each country.
#'  Begins with country code, followed by cabinets 1 - n}
#'  \item{bargainingdays}{the number of days between either an election
#'  or the resignation of the previous government and the day on
#'  which the new government is officially inaugurated}
#'  \item{datein}{date on which a government took office. Format is YYMMDD}
#'  \item{dateout}{date on which a government left office. Format is YYMMDD}
#'  \item{postelection}{dichotomous variable that equals 1 if a government
#'  is the first to form after an election (more uncertainty) and 0 if it
#'  forms in an interelection period (less uncertainty)}
#'  \item{nonpartisan}{dichotomous variable that equals 1 if the government
#'  is nonpartisan and 0 otherwise}
#'  \item{legislative_parties}{this variable is calculated as 1/$\sigma$s$_{i}^{2}$ ,
#'  where $s_{i}$ is the percentage of legislative seats won by the ith party}
#'  \item{inconclusive}{the number of inconclusive bargaining rounds prior to
#'  a new government successfully forming}
#'  \item{cabinetname}{cabinet name identified by surname
#'  of prime minister (followed by a number if the PM presided
#'  over more than one cabinet)}
#'  \item{singleparty_majority}{dichotomous variable that equals 1 if a single
#'  party controls a majority of the legislative seats, 0 otherwise}
#'  \item{polarization}{}
#'  \item{continuation}{dichotomous variable that equals 1 if the outgoing
#'  government or formateur gets the first opportunity to form a new
#'  government, 0 otherwise. Data for this variable come from DvR (1998).
#'  Of the four countries in my dataset that are not in DvR
#'  (Greece, Portugal, Spain, and the United Kingdom), only the UK has this
#'  feature (Bogdanor 1995, 147-150).}
#'  \item{positive_parl}{dichotomous variable that equals 1 if a new government
#'  requires the explicit support of a legislative majority in order to take
#'  office, 0 otherwise (Bergman 1995, PhD Thesis, Dept of Pol Sci, Umea
#'  University, Sweden)}
#'  \item{post_legislative_parties}{}
#'  \item{post_polariz}{}
#'  \item{post_positive}{}
#' }
#'
#' @keywords datasets
#'
#' @references Golder, S. N. (2010). Bargaining delays in the government
#' formation process. Comparative Political Studies, 43(1), 3-32.
#' \href{https://doi.org/10.1177/0010414009341714}{https://doi.org/10.1177/0010414009341714}
#'
#' @source \href{http://sonagolder.com/research}{http://sonagolder.com/research}
#'
#' @examples
#' data(govtform)
#' \donttest{
#' library(survival)
#' library(coxrobust)
#' library(modeLLtest)
#'
#' # Survival models with data from Golder (2010)
#' golder_surv <- Surv(govform$bargainingdays)
#' golder_x <- cbind(govtform$postelection, govtform$legislative_parties,
#'    govtform$polarization, govtform$positive_parl, govtform$post_legislative_parties,
#'    govtform$post_polariz, govtform$post_positive, govtform$continuation,
#'    govtform$singleparty_majority)
#' colnames(golder_x) <- c("govtform$postelection", "govtform$legislative_parties",
#'    "govtform$polarization", "govtform$positive_parl", "govtform$post_legislative_parties",
#'    "govtform$post_polariz", "govtform$post_positive", "govtform$continuation",
#'    "govtform$singleparty_majority")
#' govtform.cox <- coxph(govtform.surv ~ govtform.x, method = "efron",
#'    data = govtform)
#' govtform.robust <- coxr(govtform.surv ~ govtform.x, data = govtform)
#'
#' # Comparing PLM to IRR methods of estimating the survival model
#' obj_cvmf_golder <- cvmf(golder_surv ~ golder_x, method = "efron",
#'    data = govtform)
#'
#' obj_cvmf_golder
#' }
"govtform"
NULL
