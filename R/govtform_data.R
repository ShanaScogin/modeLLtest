#' Data from Golder (2010) on government formation in Western Europe
#'
#' Data from a study on Western European government formation
#' duration. Data is at the country-level (N = 409). Variable names are
#' taken directly from original dataset. The data is publicly available
#' and has been included here with the endorsement of the author. Please
#' see the original codebook for a more detailed description of the
#' variables.
#'
#' @docType data
#'
#' @usage data(govtform)
#'
#' @format A data frame with 410 rows and 18 variables. The following are taken
#' from the codebook at
#' \href{https://doi.org/10.7910/DVN/BUWZBA}{Dr. Sona N. Golder's Harvard Dataverse Page}.
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
#'  \item{legislative_parties}{a fraction representing the number of parties that
#'  have wone legislative seats. See codebook for more detail}
#'  \item{inconclusive}{the number of inconclusive bargaining rounds prior to
#'  a new government successfully forming}
#'  \item{cabinetname}{cabinet name identified by surname
#'  of prime minister (followed by a number if the PM presided
#'  over more than one cabinet)}
#'  \item{singleparty_majority}{dichotomous variable that equals 1 if a single
#'  party controls a majority of the legislative seats, 0 otherwise}
#'  \item{polarization}{measures the level of ideological polarization in the party
#'  system. See codebook for more detail}
#'  \item{continuation}{dichotomous variable that equals 1 if the outgoing
#'  government or formateur gets the first opportunity to form a new
#'  government, 0 otherwise. See codebook for more detail}
#'  \item{positive_parl}{dichotomous variable that equals 1 if a new government
#'  requires the explicit support of a legislative majority in order to take
#'  office, 0 otherwise. See codebook for more detail}
#'  \item{post_legislative_parties}{interaction term made by multiplying the postelection
#'  variable with the legislative_parties variable}
#'  \item{post_polariz}{interaction term made by multiplying the postelection
#'  variable with the polarization variable}
#'  \item{post_positive}{interaction term made by multiplying the postelection
#'  variable with the positive_parl variable}
#' }
#'
#' @keywords datasets
#'
#' @references Golder, S. N. (2010). Bargaining delays in the government
#' formation process. Comparative Political Studies, 43(1), 3-32.
#' \href{https://doi.org/10.1177/0010414009341714}{https://doi.org/10.1177/0010414009341714}
#'
#' @source \href{https://doi.org/10.7910/DVN/BUWZBA}{Dr. Sona N. Golder's Harvard Dataverse Page}
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' \donttest{
#'
#' data(govtform)
#'
#' library(survival)
#' library(coxrobust)
#' library(modeLLtest)
#'
#' # Survival models with data from Golder (2010)
#' golder_surv <- Surv(govtform$bargainingdays)
#' golder_x <- cbind(govtform$postelection, govtform$legislative_parties,
#'    govtform$polarization, govtform$positive_parl, govtform$post_legislative_parties,
#'    govtform$post_polariz, govtform$post_positive, govtform$continuation,
#'    govtform$singleparty_majority)
#' colnames(golder_x) <- c("govtform$postelection", "govtform$legislative_parties",
#'    "govtform$polarization", "govtform$positive_parl", "govtform$post_legislative_parties",
#'    "govtform$post_polariz", "govtform$post_positive", "govtform$continuation",
#'    "govtform$singleparty_majority")
#' golder_cox <- coxph(golder_surv ~ golder_x, method = "efron",
#'    data = govtform)
#' golder_robust <- coxr(golder_surv ~ golder_x, data = govtform)
#'
#' # Comparing PLM to IRR methods of estimating the survival model
#' obj_cvmf_golder <- cvmf(golder_surv ~ golder_x, method = "efron",
#'    data = govtform)
#'
#' obj_cvmf_golder
#'
#' }
#' \dontshow{setwd(.old_wd)}
"govtform"
NULL
